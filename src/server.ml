open Core
open Async
open Async_ssl.Std
open Async_smtp_types
module Config = Server_config
module Plugin = Server_plugin_intf
module Log = Mail_log

module type S = sig
  type server_state
  type t

  val start
    :  server_state:server_state
    -> config:Config.t
    -> log:Mail_log.t
    -> t Deferred.Or_error.t

  val config : t -> Config.t
  val ports : t -> int list
  val close : ?timeout:unit Deferred.t -> t -> unit Deferred.Or_error.t
end

module type For_test = sig
  type server_state

  val session
    :  server_state:server_state
    -> log:Mail_log.t
    -> ?max_message_size:Byte_units.t
    -> ?timeouts:Config.Timeouts.t
    -> ?tls_options:Config.Tls_options.t
    -> ?emulate_tls:bool
    -> ?malformed_emails:[ `Reject | `Wrap ]
    -> ?local_ip_address:Socket.Address.Inet.t
    -> ?remote_ip_address:Socket.Address.Inet.t
    -> Reader.t
    -> Writer.t
    -> unit Deferred.t
end

let read_line_with_timeouts ~close_started ~(timeouts : Config.Timeouts.t) reader =
  let start = Time_float.now () in
  let read_result =
    match%map Reader.read_line reader with
    | `Eof -> `Eof
    | `Ok line -> `Ok line
  in
  let line_or_close_started =
    Deferred.choose
      [ choice read_result (fun result -> `Read_result result)
      ; choice close_started (fun () -> `Close_started)
      ]
  in
  match%bind Clock.with_timeout timeouts.receive line_or_close_started with
  | `Result (`Read_result result) -> return result
  | `Timeout -> return `Timeout
  | `Result `Close_started ->
    let idle_for = Time_float.(diff (now ()) start) in
    let remaining_timeout = Time_float.Span.(timeouts.receive_after_close - idle_for) in
    if Time_float.Span.(remaining_timeout > zero)
    then (
      match%map Clock.with_timeout remaining_timeout read_result with
      | `Result result -> result
      | `Timeout -> `Timeout)
    else return `Timeout
;;

(* RFC 5321 specifies that the max size of that a server should support at LEAST be 64KB
   https://www.rfc-editor.org/rfc/rfc5321#section-4.5.3.1.7. [100KB] is large, but is
   likely smaller than [max_size], but larger than [64KB]. *)
let initial_buffer_size_bytes = Byte_units.of_kilobytes 100. |> Byte_units.bytes_int_exn

let try_with_out_of_memory f =
  match Result.try_with f with
  | Error Out_of_memory -> Error `Out_of_memory
  | Error exn -> raise exn
  | Ok res -> Ok res
;;

(* Utility to read all data up to and including a '\n.' discarding the '\n.'. *)
let read_data ~max_size ~close_started ~timeouts reader =
  let max_len = max_size |> Byte_units.bytes_int_exn in
  let write_string s buf =
    let%bind.Result buf in
    (* [Bigbuffer] does not bound its underlying buffer. It's possible for [buf] to resize
       to something between [max_len] and [2 * max_len]. *)
    if Bigbuffer.length buf + String.length s > max_len
    then Error `Too_much_data
    else (
      let%map.Result () = try_with_out_of_memory (fun () -> Bigbuffer.add_string buf s) in
      buf)
  in
  let rec loop ~is_first buf =
    match%bind read_line_with_timeouts ~close_started ~timeouts reader with
    | `Timeout -> return (Error `Timeout)
    | `Eof -> return (Error `Eof)
    | `Ok "." -> return buf
    | `Ok line ->
      let buf = if not is_first then write_string "\n" buf else buf in
      let decoded = Dot_escaping.decode_line_string line in
      let buf = write_string decoded buf in
      loop ~is_first:false buf
  in
  let%bind.Deferred.Result buf =
    try_with_out_of_memory (fun () ->
      Int.min max_len initial_buffer_size_bytes |> Bigbuffer.create)
    |> Deferred.return
  in
  loop ~is_first:true (Ok buf)
;;

module Make (Cb : Plugin.S) = struct
  let write_reply_impl ~log ?raw_writer ?write_reply () =
    let write_reply_default reply =
      match raw_writer with
      | Some writer ->
        Writer.write writer (Smtp_reply.to_string reply);
        Writer.write writer "\r\n";
        Writer.flushed writer
      | None ->
        if Smtp_reply.is_ok reply
        then Deferred.unit
        else failwithf !"Unconsumed failure: %{Smtp_reply}" reply ()
    in
    let write_reply = Option.value write_reply ~default:write_reply_default in
    let write_reply ~here ~flows ~component reply =
      Log.debug log (lazy (Log.Message.create ~here ~flows ~component ~reply "->"));
      write_reply reply
    in
    Staged.stage write_reply
  ;;

  let on_plugin_error_no_reply
    ~log
    ?(tags = [])
    ?reply
    ~here
    ~flows
    ~component
    ~plugin
    err
    =
    let level, error =
      match err with
      | `Reject err -> `Info, Reject_or_error.error err
      | `Exn exn -> `Error, Error.of_exn exn
    in
    let tags = ("plugin", plugin) :: tags in
    Log.message
      log
      ~level
      (lazy (Mail_log.Message.of_error error ~here ~flows ~component ~tags ?reply))
  ;;

  let on_plugin_error
    ~log
    ?tags
    ~write_reply
    ?(default_reject = Smtp_reply.service_unavailable_421)
    ~here
    ~flows
    ~component
    ~plugin
    err
    =
    let reject =
      match err with
      | `Reject err -> Option.value (Reject_or_error.reject err) ~default:default_reject
      | `Exn _ -> default_reject
    in
    on_plugin_error_no_reply ~log ?tags ~reply:reject ~here ~flows ~component ~plugin err;
    write_reply ~here ~flows ~component reject
  ;;

  let protect_plugin ~log ~here ~flows ~component ~plugin f =
    let component = component @ [ "PLUGIN"; plugin ] in
    match%map
      Monitor.try_with ~run:`Schedule ~rest:`Log (fun () ->
        f ~log:(Log.with_flow_and_component log ~flows ~component))
    with
    | Ok (Ok res) -> Ok res
    | Ok (Error err) -> Error (`Reject (Reject_or_error.tag ~here ~tag:plugin err))
    | Error exn -> Error (`Exn exn)
  ;;

  let extensions (type session) ~tls_options (plugins : session Plugin.Extension.t list) =
    let auth_extensions =
      List.filter_map plugins ~f:(function
        | Plugin.Extension.Auth (module Auth : Plugin.Auth with type session = session) ->
          Some Auth.mechanism
        | _ -> None)
      |> function
      | [] -> []
      | _ :: _ as mechs -> [ Smtp_extension.Auth mechs ]
    in
    let other_extensions =
      List.filter_map plugins ~f:(function
        | Plugin.Extension.Auth _ -> None (* handled above *)
        | Plugin.Extension.Start_tls _ ->
          Option.map tls_options ~f:(const Smtp_extension.Start_tls))
    in
    [ Smtp_extension.Mime_8bit_transport ] @ auth_extensions @ other_extensions
  ;;

  let rec session_loop
    ~state
    ~log
    ~tls_options
    ~max_message_size
    ~malformed_emails
    ~server_events
    ~close_started
    ~timeouts
    ~session_flows
    ~reader
    ?raw_writer
    ?write_reply
    (session : Cb.Session.t)
    =
    let extensions session =
      extensions ~tls_options (Cb.Session.extensions ~state session)
    in
    let write_reply =
      write_reply_impl ~log ?raw_writer ?write_reply () |> Staged.unstage
    in
    let bad_sequence_of_commands ~here ~flows ~component cmd =
      write_reply ~here ~flows ~component (Smtp_reply.bad_sequence_of_commands_503 cmd)
    in
    let closing_connection ~here ~flows ~component () =
      write_reply ~here ~flows ~component Smtp_reply.closing_connection_221
    in
    let command_not_implemented ~here ~flows ~component cmd =
      write_reply ~here ~flows ~component (Smtp_reply.command_not_implemented_502 cmd)
    in
    let command_not_recognized ~here ~flows ~component msg =
      write_reply ~here ~flows ~component (Smtp_reply.command_not_recognized_500 msg)
    in
    let ok_completed ~here ~flows ~component ?(extra = []) msg =
      let extra = List.map extra ~f:Smtp_extension.to_string in
      let msg = String.concat ~sep:"\n" (msg :: extra) in
      write_reply ~here ~flows ~component (Smtp_reply.ok_completed_250 msg)
    in
    let ok_continue ~here ~flows ~component () =
      ok_completed ~here ~flows ~component "continue"
    in
    let service_ready ~here ~flows ~component msg =
      write_reply ~here ~flows ~component (Smtp_reply.service_ready_220 msg)
    in
    let start_mail_input ~here ~flows ~component () =
      write_reply ~here ~flows ~component Smtp_reply.start_mail_input_354
    in
    let syntax_error ~here ~flows ~component msg =
      write_reply ~here ~flows ~component (Smtp_reply.syntax_error_501 msg)
    in
    (* This is kind of like a state machine. [next] is the node the machine is on [next]
       returns Error for invalid transition [next] returns Ok fun for valid transitions
       fun is invoked on the result and this loops back into the state machine. [state] is
       meta data that gets passed through and mutated by the machine
    *)
    let loop ~flows ~component ~next =
      let component = component @ [ "read-loop" ] in
      let rec loop () =
        match%bind read_line_with_timeouts ~close_started ~timeouts reader with
        | `Eof ->
          Log.info
            log
            (lazy (Log.Message.create ~here:[%here] ~flows ~component "DISCONNECTED"));
          return ()
        | `Timeout ->
          Log.info
            log
            (lazy
              (Log.Message.create
                 ~here:[%here]
                 ~flows
                 ~component
                 "Timeout while reading command"));
          let%bind () =
            write_reply ~here:[%here] ~flows ~component Smtp_reply.command_timeout_421
          in
          return ()
        | `Ok "" ->
          (*_ .Net System.Net.Mail.SmtpClient sends an unexpected empty line. It seems
              okay to silently skip these. *)
          Log.debug
            log
            (lazy
              (Log.Message.create
                 ~here:[%here]
                 ~flows
                 ~component
                 "Broken client sent empty line"));
          loop ()
        | `Ok input ->
          (match Option.try_with (fun () -> Smtp_command.of_string input) with
           | None ->
             Log.debug
               log
               (lazy
                 (Log.Message.create
                    ~here:[%here]
                    ~flows
                    ~component
                    ~tags:[ "command", input ]
                    "Got unexpected input on reader"));
             let%bind () = command_not_recognized ~here:[%here] ~flows ~component input in
             loop ()
           | Some cmd ->
             Log.debug
               log
               (lazy
                 (Log.Message.create
                    ~here:[%here]
                    ~flows
                    ~component
                    ~command:cmd
                    "Got input on reader"));
             next cmd)
      in
      loop ()
    in
    let rec top ~session : unit Deferred.t =
      let component = [ "smtp-server"; "session"; "top" ] in
      let flows = session_flows in
      loop ~flows ~component ~next:(function
        | Smtp_command.Quit -> closing_connection ~here:[%here] ~flows ~component ()
        | Smtp_command.Noop ->
          let%bind () = ok_continue ~here:[%here] ~flows ~component () in
          top ~session
        | Smtp_command.Reset ->
          let%bind () = ok_continue ~here:[%here] ~flows ~component () in
          top ~session
        | Smtp_command.Hello helo -> top_helo ~extended:false ~flows ~session helo
        | Smtp_command.Extended_hello helo -> top_helo ~extended:true ~flows ~session helo
        | Smtp_command.Auth (meth, initial_resp) ->
          top_auth ~flows ~session ~meth ~initial_resp
        | Smtp_command.Start_tls ->
          (* We assume that [Cb.Session.extensions] only changes after a protocol upgrade. *)
          (match
             Cb.Session.extensions ~state session
             |> List.find_map ~f:(function
               | Plugin.Extension.Start_tls cb ->
                 Option.map tls_options ~f:(fun opts -> opts, cb)
               | _ -> None)
           with
           | None ->
             let%bind () =
               command_not_implemented
                 ~here:[%here]
                 ~flows
                 ~component
                 Smtp_command.Start_tls
             in
             top ~session
           | Some (tls_options, cbs) -> top_start_tls ~session ~flows tls_options cbs)
        | Smtp_command.Sender sender -> top_envelope ~flows ~session sender
        | (Smtp_command.Recipient _ | Smtp_command.Data) as cmd ->
          let%bind () = bad_sequence_of_commands ~here:[%here] ~flows ~component cmd in
          top ~session
        | Smtp_command.Help as cmd ->
          let%bind () = command_not_implemented ~here:[%here] ~flows ~component cmd in
          top ~session)
    and top_helo ~flows ~extended ~session helo =
      let component = [ "smtp-server"; "session"; "helo" ] in
      let plugin = "Session.helo" in
      let tags = [ "extended", Bool.to_string extended; "helo", helo ] in
      match%bind
        protect_plugin ~here:[%here] ~log ~component ~flows ~plugin (fun ~log ->
          Cb.Session.helo ~state ~log session helo)
      with
      | Ok session ->
        let extensions = extensions session in
        let greeting, extra =
          if extended then "Continue, extensions follow:", extensions else "Continue", []
        in
        let%bind () = ok_completed ~here:[%here] ~flows ~component ~extra greeting in
        Log.info
          log
          (lazy
            (Log.Message.create
               ~here:[%here]
               ~flows
               ~component
               ~tags
               "session_helo:accepted"));
        top ~session
      | Error err ->
        on_plugin_error
          ~log
          ~tags
          ~write_reply
          ~here:[%here]
          ~flows
          ~component
          ~plugin
          err
    and top_start_tls
      ~flows
      ~session
      tls_options
      (module Tls_cb : Plugin.Start_tls with type session = Cb.Session.t)
      =
      let component = [ "smtp-server"; "session"; "starttls" ] in
      let%bind () =
        service_ready ~here:[%here] ~flows ~component "Begin TLS transition"
      in
      let old_reader = reader in
      let old_writer = raw_writer |> Option.value_exn in
      let reader_pipe_r, reader_pipe_w = Pipe.create () in
      let writer_pipe_r, writer_pipe_w = Pipe.create () in
      match%bind
        Ssl.server
          ?version:tls_options.Config.Tls_options.version
          ?options:tls_options.Config.Tls_options.options
          ?name:tls_options.Config.Tls_options.name
          ?ca_file:tls_options.Config.Tls_options.ca_file
          ?ca_path:tls_options.Config.Tls_options.ca_path
          ~allowed_ciphers:tls_options.Config.Tls_options.allowed_ciphers
          ~crt_file:tls_options.Config.Tls_options.crt_file
          ~key_file:tls_options.Config.Tls_options.key_file
            (* Closing ssl connection will close the pipes which will in turn close the
               readers. *)
          ~net_to_ssl:(Reader.pipe old_reader)
          ~ssl_to_net:(Writer.pipe old_writer)
          ~ssl_to_app:reader_pipe_w
          ~app_to_ssl:writer_pipe_r
          ()
      with
      | Error e ->
        Log.error log (lazy (Log.Message.of_error ~here:[%here] ~flows ~component e));
        return ()
      | Ok tls ->
        let%bind new_reader = Reader.of_pipe (Info.of_string "SMTP/TLS") reader_pipe_r in
        let%bind new_writer, `Closed_and_flushed_downstream closed_and_flushed =
          Writer.of_pipe (Info.of_string "SMTP/TLS") writer_pipe_w
        in
        let version = Ssl.Connection.version tls in
        let v = Sexp.to_string_hum (Ssl.Version.sexp_of_t version) in
        Log.info
          log
          (lazy
            (Log.Message.create
               ~here:[%here]
               ~flows
               ~component
               (sprintf "Using TLS protocol %s" v)));
        let teardown () =
          let%bind () = Writer.close new_writer in
          let%bind () = closed_and_flushed in
          Ssl.Connection.close tls;
          let%bind () =
            Ssl.Connection.closed tls
            >>| Result.iter_error ~f:(fun e ->
              Log.error
                log
                (lazy (Log.Message.of_error ~here:[%here] ~flows ~component e)))
          in
          Reader.close new_reader
        in
        let plugin = "Tls.upgrade_to_tls" in
        Monitor.protect
          ~run:`Schedule
          ~rest:`Log
          (fun () ->
            match%bind
              protect_plugin ~here:[%here] ~log ~flows ~component ~plugin (fun ~log ->
                Tls_cb.upgrade_to_tls ~log session)
            with
            | Ok session ->
              session_loop
                ~state
                ~log
                ~tls_options:(Some tls_options)
                ~max_message_size
                ~malformed_emails
                ~server_events
                ~close_started
                ~timeouts
                ~session_flows
                ~reader:new_reader
                ~raw_writer:new_writer
                session
            | Error err ->
              on_plugin_error_no_reply ~log ~here:[%here] ~flows ~component ~plugin err;
              Deferred.unit)
          ~finally:teardown
    and top_auth ~flows ~session ~meth ~initial_resp =
      let component = [ "smtp-server"; "session"; "auth" ] in
      match
        Cb.Session.extensions ~state session
        |> List.find_map ~f:(function
          | Plugin.Extension.Auth
              ((module Auth : Plugin.Auth with type session = Cb.Session.t) as auth) ->
            Option.some_if (String.Caseless.equal meth Auth.mechanism) auth
          | _ -> None)
      with
      | None ->
        command_not_implemented
          ~here:[%here]
          ~flows
          ~component
          (Smtp_command.Auth (meth, None))
      | Some (module Auth : Plugin.Auth with type session = Cb.Session.t) ->
        let initial_resp = ref initial_resp in
        let auth_finished = ref false in
        let challenge_lock = ref false in
        let send_challenge_and_expect_response msg =
          if !auth_finished
          then
            failwith
              "Calling [send_challenge] after [authenticate] flow completed/aborted."
          else if !challenge_lock
          then failwith "Concurrent calls of [send_challenge]"
          else (
            match !initial_resp with
            | Some resp ->
              initial_resp := None;
              Smtp_monad.try_with ~here:[%here] (fun () ->
                return (Base64.decode_exn resp))
            | None ->
              challenge_lock := true;
              let%bind () =
                write_reply
                  ~here:[%here]
                  ~flows
                  ~component
                  (Smtp_reply.start_authentication_input_334 (Base64.encode_exn msg))
              in
              (match%bind Reader.read_line reader with
               | `Eof ->
                 Smtp_monad.error_string
                   ~here:[%here]
                   "Client disconnected during authentication flow"
               | `Ok resp ->
                 let%map result =
                   Smtp_monad.try_with ~here:[%here] (fun () ->
                     return (Base64.decode_exn resp))
                 in
                 (* Deliberately only release the lock on success. This ensures that calls
                    after failure will continue to fail. *)
                 challenge_lock := Result.is_error result;
                 result))
        in
        let plugin = "Auth.negotiate" in
        (match%bind
           let%map res =
             protect_plugin ~log ~here:[%here] ~flows ~component ~plugin (fun ~log ->
               Auth.negotiate ~log session ~send_challenge_and_expect_response)
           in
           auth_finished := true;
           res
         with
         | Error err ->
           let%bind () =
             on_plugin_error
               ~log
               ~write_reply
               ~default_reject:Smtp_reply.authentication_credentials_invalid_535
               ~here:[%here]
               ~flows
               ~component
               ~plugin
               err
           in
           top ~session
         | Ok session ->
           let%bind () =
             write_reply
               ~here:[%here]
               ~flows
               ~component
               Smtp_reply.authentication_successful_235
           in
           top ~session)
    and top_envelope ~flows ~session sender_str =
      let flows = Log.Flows.extend flows `Inbound_envelope in
      let component = [ "smtp-server"; "session"; "envelope"; "sender" ] in
      let allowed_extensions = extensions session in
      match
        Smtp_envelope.Sender.of_string_with_arguments ~allowed_extensions sender_str
      with
      | Error err ->
        Log.info
          log
          (lazy
            (Log.Message.of_error
               ~here:[%here]
               ~flows
               ~component
               ~sender:(`String sender_str)
               (Error.tag err ~tag:"Unable to parse MAIL FROM")));
        let%bind () =
          syntax_error
            ~here:[%here]
            ~flows
            ~component
            (sprintf "Cannot parse '%s'" sender_str)
        in
        top ~session
      | Ok (sender, sender_args) ->
        let plugin = "Envelope.mail_from" in
        (match%bind
           protect_plugin ~log ~here:[%here] ~flows ~component ~plugin (fun ~log ->
             Cb.Envelope.mail_from ~state ~log session sender sender_args)
         with
         | Error err ->
           let tags = [ Mail_log_tags.sender, sender_str ] in
           let%bind () =
             on_plugin_error
               ~log
               ~tags
               ~write_reply
               ~here:[%here]
               ~flows
               ~component
               ~plugin
               err
           in
           top ~session
         | Ok data ->
           Log.info
             log
             (lazy
               (Log.Message.create
                  ~here:[%here]
                  ~flows
                  ~component
                  ~sender:(`String sender_str)
                  ~session_marker:`Mail_from
                  "MAIL FROM"));
           let%bind () = ok_continue ~here:[%here] ~flows ~component () in
           envelope ~session ~flows data)
    and envelope ~session ~flows data =
      let component = [ "smtp-server"; "session"; "envelope"; "top" ] in
      loop ~flows ~component ~next:(function
        | Smtp_command.Quit -> closing_connection ~here:[%here] ~flows ~component ()
        | Smtp_command.Noop ->
          let%bind () = ok_continue ~here:[%here] ~flows ~component () in
          envelope ~session ~flows data
        | Smtp_command.Recipient recipient ->
          envelope_recipient ~session ~flows data recipient
        | Smtp_command.Data -> envelope_data ~session ~flows data
        | ( Smtp_command.Hello _
          | Smtp_command.Extended_hello _
          | Smtp_command.Sender _
          | Smtp_command.Start_tls
          | Smtp_command.Auth _ ) as cmd ->
          let%bind () = bad_sequence_of_commands ~here:[%here] ~flows ~component cmd in
          envelope ~session ~flows data
        | Smtp_command.Reset ->
          let%bind () = ok_continue ~here:[%here] ~flows ~component () in
          top ~session
        | Smtp_command.Help as cmd ->
          let%bind () = command_not_implemented ~here:[%here] ~flows ~component cmd in
          envelope ~session ~flows data)
    and envelope_recipient ~session ~flows data recipient_str =
      let component = [ "smtp-server"; "session"; "envelope"; "recipient" ] in
      match Email_address.of_string recipient_str with
      | Error err ->
        Log.info
          log
          (lazy
            (Log.Message.of_error
               ~here:[%here]
               ~flows
               ~component
               ~recipients:[ `String recipient_str ]
               (Error.tag err ~tag:"cannot parse recipient")));
        let%bind () =
          syntax_error
            ~here:[%here]
            ~flows
            ~component
            (sprintf "Cannot parse %s" recipient_str)
        in
        envelope ~session ~flows data
      | Ok recipient ->
        let plugin = "Envelope.rcpt_to" in
        (match%bind
           protect_plugin ~log ~here:[%here] ~flows ~component ~plugin (fun ~log ->
             Cb.Envelope.rcpt_to ~state ~log session data recipient)
         with
         | Error err ->
           let tags = [ Mail_log_tags.recipient, recipient_str ] in
           let%bind () =
             on_plugin_error
               ~log
               ~tags
               ~write_reply
               ~here:[%here]
               ~flows
               ~component
               ~plugin
               err
           in
           envelope ~session ~flows data
         | Ok data ->
           Log.info
             log
             (lazy
               (Log.Message.create
                  ~here:[%here]
                  ~flows
                  ~component
                  ~recipients:[ `Email recipient ]
                  ~session_marker:`Rcpt_to
                  "RCPT TO"));
           let%bind () = ok_continue ~here:[%here] ~flows ~component () in
           envelope ~session ~flows data)
    and envelope_data ~session ~flows data =
      let component = [ "smtp-server"; "session"; "envelope"; "data" ] in
      let plugin = "Envelope.accept_data" in
      match%bind
        protect_plugin ~here:[%here] ~log ~flows ~component ~plugin (fun ~log ->
          Cb.Envelope.accept_data ~state ~log session data)
      with
      | Error err ->
        let%bind () =
          on_plugin_error ~log ~write_reply ~here:[%here] ~flows ~component ~plugin err
        in
        envelope ~session ~flows data
      | Ok data ->
        let%bind () = start_mail_input ~here:[%here] ~flows ~component () in
        (match%bind
           read_data ~max_size:max_message_size ~close_started ~timeouts reader
         with
         | Error `Eof ->
           Log.info
             log
             (lazy (Log.Message.create ~here:[%here] ~flows ~component "MESSAGE_ABORTED"));
           return ()
         | Error `Timeout ->
           Log.info
             log
             (lazy
               (Log.Message.create
                  ~here:[%here]
                  ~flows
                  ~component
                  "Timeout while reading data"));
           let%bind () =
             write_reply ~here:[%here] ~flows ~component Smtp_reply.data_timeout_421
           in
           return ()
         | Error `Too_much_data ->
           let%bind () =
             write_reply
               ~here:[%here]
               ~flows
               ~component
               Smtp_reply.exceeded_storage_allocation_552
           in
           top ~session
         | Error `Out_of_memory ->
           let%bind () =
             write_reply
               ~here:[%here]
               ~flows
               ~component
               Smtp_reply.insufficent_system_storage_452
           in
           top ~session
         | Ok raw ->
           Log.debug
             log
             (lazy
               (Log.Message.create
                  ~here:[%here]
                  ~flows
                  ~component
                  ~tags:[ "data", Bigbuffer.contents raw ]
                  "DATA"));
           let email =
             match Or_error.try_with (fun () -> Email.of_bigbuffer raw) with
             | Ok email -> Ok email
             | Error error ->
               Log.error
                 log
                 (lazy (Log.Message.of_error ~here:[%here] ~flows ~component error));
               (match malformed_emails with
                | `Reject -> Error (Smtp_reply.syntax_error_501 "Malformed Message")
                | `Wrap ->
                  let raw = Bigbuffer.contents raw in
                  Ok
                    (Email.Simple.Content.text_utf8
                       ~extra_headers:
                         [ "X-JS-Parse-Error", sprintf !"%{sexp:Error.t}" error ]
                       raw
                      :> Email.t))
           in
           (match email with
            | Error error ->
              let%bind () = write_reply ~here:[%here] ~flows ~component error in
              top ~session
            | Ok email ->
              let original_msg =
                Smtp_envelope.create' ~info:(Cb.Envelope.smtp_envelope_info data) ~email
              in
              Smtp_events.envelope_received server_events original_msg;
              Log.info
                log
                (lazy
                  (Log.Message.create
                     ~here:[%here]
                     ~flows
                     ~component
                     ~email:(`Envelope original_msg)
                     ~compute_body_fingerprint_hash:true
                     ~session_marker:`Data
                     "DATA"));
              let component = [ "smtp-server"; "session"; "envelope"; "routing" ] in
              let plugin = "Envelope.process" in
              (match%bind
                 protect_plugin ~here:[%here] ~log ~flows ~component ~plugin (fun ~log ->
                   Cb.Envelope.process ~state ~log ~flows session data email)
               with
               | Error err ->
                 let%bind () =
                   on_plugin_error
                     ~log
                     ~write_reply
                     ~here:[%here]
                     ~flows
                     ~component
                     ~plugin
                     err
                 in
                 top ~session
               | Ok response ->
                 Log.info
                   log
                   (lazy
                     (Log.Message.create
                        ~here:[%here]
                        ~flows
                        ~component
                        ~tags:[ "processed", response ]
                        "PROCESSED"));
                 let%bind () = ok_completed ~here:[%here] ~flows ~component response in
                 top ~session)))
    in
    top ~session
  ;;

  let start_session
    ~state
    ~log
    ~malformed_emails
    ~tls_options
    ~emulate_tls_for_test
    ~max_message_size
    ~reader
    ~server_events
    ~close_started
    ~timeouts
    ?raw_writer
    ?write_reply
    ~session_flows
    ~local_ip_address
    ~remote_ip_address
    ()
    =
    let write_reply' =
      Staged.unstage (write_reply_impl ~log ?raw_writer ?write_reply ())
    in
    let component = [ "smtp-server"; "session"; "init" ] in
    let plugin = "Session.connect" in
    let flows = session_flows in
    Log.info
      log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows
           ~component
           ~local_ip_address
           ~remote_ip_address
           ~session_marker:`Connected
           "CONNECTED"));
    match%bind
      protect_plugin ~here:[%here] ~log ~flows ~component ~plugin (fun ~log ->
        Cb.Session.connect ~state ~log ~local:local_ip_address ~remote:remote_ip_address)
    with
    | Error err ->
      on_plugin_error
        ~log
        ~write_reply:write_reply'
        ~here:[%here]
        ~flows
        ~component
        ~plugin
        err
    | Ok session ->
      let plugin = "Emulated_for_test.Tls.upgrade_to_tls" in
      (match%bind
         if emulate_tls_for_test
         then (
           match
             List.find_map (Cb.Session.extensions ~state session) ~f:(function
               | Plugin.Extension.Start_tls cb -> Some cb
               | _ -> None)
           with
           | None ->
             return
               (Error
                  (`Exn
                    (Failure
                       "Session initiated with claim of pre-established TLS but Plugin \
                        does not provide TLS callback")))
           | Some (module Tls : Plugin.Start_tls with type session = Cb.Session.t) ->
             protect_plugin ~here:[%here] ~log ~flows ~component ~plugin (fun ~log ->
               Tls.upgrade_to_tls ~log session))
         else return (Ok session)
       with
       | Error err ->
         on_plugin_error
           ~log
           ~write_reply:write_reply'
           ~here:[%here]
           ~flows
           ~component
           ~plugin
           err
       | Ok session ->
         let greeting = Cb.Session.greeting session in
         Log.info
           log
           (lazy
             (Log.Message.create
                ~here:[%here]
                ~flows
                ~component
                ~tags:[ "greeting", greeting ]
                "Session.connect:accepted"));
         Monitor.protect
           ~run:`Schedule
           ~rest:`Log
           (fun () ->
             let%bind () =
               write_reply'
                 ~here:[%here]
                 ~flows
                 ~component
                 (Smtp_reply.service_ready_220 greeting)
             in
             session_loop
               ~state
               ~log
               ~tls_options
               ~max_message_size
               ~malformed_emails
               ~server_events
               ~close_started
               ~timeouts
               ~reader
               ?raw_writer
               ?write_reply
               ~session_flows
               session)
           ~finally:(fun () ->
             let plugin = "Session.disconnect" in
             match%map
               protect_plugin ~here:[%here] ~log ~flows ~component ~plugin (fun ~log ->
                 Cb.Session.disconnect ~state ~log session)
             with
             | Ok () -> ()
             | Error err ->
               on_plugin_error_no_reply ~log ~here:[%here] ~flows ~component ~plugin err))
  ;;

  type t =
    { config : Config.t
    ; (* One server per port *)
      servers : Tcp.Server.inet list
    ; close_started : unit Ivar.t
    }

  let tcp_servers ~server_state ~config ~log ~server_events ~close_started =
    let start_server where_to_listen =
      let local_ip_address = Config.Where_to_listen.socket_address where_to_listen in
      let tcp_options = Config.tcp_options config in
      let max_accepts_per_batch =
        Option.bind tcp_options ~f:Config.Tcp_options.max_accepts_per_batch
      in
      let backlog = Option.bind tcp_options ~f:Config.Tcp_options.backlog in
      Tcp.Server.create
        (Config.Where_to_listen.to_tcp_where_to_listen where_to_listen)
        ?max_accepts_per_batch
        ?backlog
        ~on_handler_error:
          (`Call
            (fun remote_ip_address exn ->
              (* Silence the [inner_monitor] errors *)
              Log.error
                log
                (lazy
                  (Log.Message.of_error
                     ~here:[%here]
                     ~flows:Log.Flows.none
                     ~component:[ "smtp-server"; "tcp" ]
                     ~local_ip_address
                     ~remote_ip_address
                     (Error.of_exn ~backtrace:`Get exn)))))
        ~max_connections:(Config.max_concurrent_receive_jobs_per_port config)
        (fun remote_ip_address reader writer ->
          let local_ip_address =
            Option.try_with (fun () ->
              match Writer.fd writer |> Fd.file_descr_exn |> Core_unix.getsockname with
              | Core_unix.ADDR_UNIX _ -> assert false
              | Core_unix.ADDR_INET (host, port) -> Socket.Address.Inet.create host ~port)
            |> Option.value ~default:local_ip_address
          in
          let session_flows = Log.Flows.create `Server_session in
          Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
            start_session
              ~state:server_state
              ~log
              ~server_events
              ~tls_options:config.Config.tls_options
              ~emulate_tls_for_test:false
              ~malformed_emails:config.Config.malformed_emails
              ~max_message_size:config.Config.max_message_size
              ~session_flows
              ~reader
              ~raw_writer:writer
              ~local_ip_address
              ~remote_ip_address
              ~close_started
              ~timeouts:config.timeouts
              ())
          >>| Result.iter_error ~f:(fun err ->
            Log.error
              log
              (lazy
                (Log.Message.of_error
                   ~here:[%here]
                   ~flows:session_flows
                   ~component:[ "smtp-server"; "tcp" ]
                   ~local_ip_address
                   ~remote_ip_address
                   err))))
    in
    Deferred.List.map ~how:`Parallel (Config.where_to_listen config) ~f:start_server
  ;;

  let start ~server_state ~config ~log =
    let close_started = Ivar.create () in
    let server_events = Smtp_events.create () in
    let%map servers =
      tcp_servers
        ~server_state
        ~config
        ~log
        ~server_events
        ~close_started:(Ivar.read close_started)
    in
    let plugin_rpcs =
      List.map (Cb.rpcs ()) ~f:(Rpc.Implementation.lift ~f:(fun () -> server_state))
    in
    don't_wait_for (Rpc_server.start (config, server_events) ~log ~plugin_rpcs);
    Ok { config; servers; close_started }
  ;;

  let config t = t.config
  let ports t = List.map t.servers ~f:(fun server -> Tcp.Server.listening_on server)

  let close ?(timeout = Deferred.never ()) t =
    Ivar.fill_if_empty t.close_started ();
    let%bind () = Deferred.List.iter ~how:`Parallel t.servers ~f:Tcp.Server.close in
    let finished =
      List.map t.servers ~f:Tcp.Server.close_finished_and_handlers_determined
      |> Deferred.all_unit
    in
    match%bind
      Deferred.choose
        [ Deferred.choice timeout (fun () -> `Timeout)
        ; Deferred.choice finished (fun () -> `Finished)
        ]
    with
    | `Finished -> Deferred.Or_error.ok_unit
    | `Timeout -> Deferred.Or_error.error_string "Timed out flushing all sessions."
  ;;
end

module For_test (P : Plugin.S) = struct
  include Make (P)

  let session
    ~server_state
    ~log
    ?(max_message_size = Byte_units.of_bytes_int Int.max_value_30_bits)
    ?(timeouts = Config.Timeouts.default)
    ?tls_options
    ?(emulate_tls = false)
    ?(malformed_emails = `Reject)
    ?(local_ip_address = Socket.Address.Inet.create_bind_any ~port:0)
    ?(remote_ip_address = Socket.Address.Inet.create_bind_any ~port:0)
    reader
    writer
    =
    start_session
      ~state:server_state
      ~log
      ~malformed_emails
      ~tls_options
      ~emulate_tls_for_test:emulate_tls
      ~max_message_size
      ~reader
      ~server_events:(Smtp_events.create ())
      ~close_started:(Deferred.never ())
      ~timeouts
      ~raw_writer:writer
      ~session_flows:(Mail_log.Flows.create `Server_session)
      ~local_ip_address
      ~remote_ip_address
      ()
  ;;
end

let bsmtp_log =
  Lazy.map Async.Log.Global.log ~f:(fun log ->
    log |> Log.adjust_log_levels ~remap_info_to:`Debug)
;;

let read_bsmtp ?(log = Lazy.force bsmtp_log) reader =
  let server_events = Smtp_events.create () in
  Pipe.create_reader ~close_on_exception:true (fun out ->
    let session_flows = Log.Flows.create `Server_session in
    let module Smtp =
      Make (struct
        module State = Unit
        module Session = Plugin.Simple.Session

        module Envelope = struct
          include Plugin.Simple.Envelope

          let process ~state:() ~log:_ ~flows:_ _session t email =
            let envelope = smtp_envelope t email in
            let%bind () = Pipe.write out (Ok envelope) in
            Smtp_monad.return "bsmtp"
          ;;
        end

        let rpcs = Plugin.Simple.rpcs
      end)
    in
    Smtp.start_session
      ~state:()
      ~log
      ~tls_options:Config.default.tls_options
      ~emulate_tls_for_test:false
      ~max_message_size:(Byte_units.of_megabytes 250.)
      ~malformed_emails:Config.default.malformed_emails
      ~reader
      ~server_events
      ~close_started:(Deferred.never ())
      ~timeouts:Config.Timeouts.default
      ~session_flows
      ?raw_writer:None
      ~write_reply:(fun reply ->
        if Smtp_reply.is_ok reply
        then return ()
        else Pipe.write out (Or_error.error_string (Smtp_reply.to_string reply)))
      ~local_ip_address:(Socket.Address.Inet.create_bind_any ~port:0)
      ~remote_ip_address:(Socket.Address.Inet.create_bind_any ~port:0)
      ())
;;
