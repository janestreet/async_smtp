open Core
open Async
open Async_ssl.Std
open Email_message

module Config = Server_config
module Plugin = Server_plugin

module Log = Mail_log

module type S = sig
  type t

  val start :  config:Config.t -> log:Mail_log.t -> t Deferred.Or_error.t

  val config : t -> Config.t
  val ports  : t -> int list

  val close  : ?timeout:unit Deferred.t -> t -> unit Deferred.Or_error.t
end

module type For_test = sig
  val session
    :  ?send:(Envelope.With_next_hop.t list -> string Deferred.Or_error.t)
    -> ?quarantine:(reason:Quarantine_reason.t -> Envelope.With_next_hop.t list -> unit Deferred.Or_error.t)
    -> log:Mail_log.t
    -> ?max_message_size:Byte_units.t
    -> ?tls_options:Config.Tls.t
    -> ?emulate_tls:bool
    -> ?malformed_emails:[`Reject|`Wrap]
    -> ?local:Address.t
    -> remote:Address.t
    -> Reader.t
    -> Writer.t
    -> unit Deferred.t
end


(* Utility to read all data up to and including a '\n.' discarding the '\n.'. *)
let read_data ~max_size reader =
  let max_size = max_size |> Byte_units.bytes |> Float.to_int in
  let rec loop ~is_first accum =
    let add_string s =
      Option.iter accum ~f:(fun accum -> Bigbuffer.add_string accum s)
    in
    let return () =
      match accum with
      | Some accum -> return (`Ok accum)
      | None -> return `Too_much_data
    in
    Reader.read_line reader
    >>= function
    | `Eof ->
      if not is_first then add_string "\n";
      return ()
    | `Ok "." ->
      return ()
    | `Ok line ->
      let too_long =
        match accum with
        | None -> true
        | Some accum ->
          String.length line + Bigbuffer.length accum + 1 > max_size
      in
      if too_long then loop ~is_first None
      else begin
        if not is_first then add_string "\n";
        let len = String.length line in
        if len > 1 && (String.get line 0 |> Char.equal '.')
        then begin
          (* slice takes start and stop positions. *)
          let s = String.slice line 1 len in
          add_string s
        end
        else add_string line;
        loop ~is_first:false accum
      end
  in
  Some (Bigbuffer.create max_size) |> loop ~is_first:true
;;

module Make(Cb : Plugin.S) = struct


  let write_reply_impl ~log ?raw_writer ?write_reply () =
    let write_reply_default reply =
      match raw_writer with
      | Some writer ->
        Writer.write writer
          (Smtp_reply.to_string reply);
        Writer.write writer "\r\n";
        Writer.flushed writer
      | None ->
        if Smtp_reply.is_ok reply then Deferred.unit
        else failwithf !"Unconsumed failure: %{Smtp_reply}" reply ()
    in
    let write_reply = Option.value write_reply ~default:write_reply_default in
    let write_reply ~here ~flows ~component reply =
      Log.debug log (lazy (Log.Message.create
                             ~here
                             ~flows
                             ~component
                             ~reply
                             "->"));
      write_reply reply
    in
    Staged.stage write_reply
  ;;

  let extensions (type session) ~tls_options (plugins:session Plugin.Extension.t list) =
    let auth_extensions =
      List.filter_map plugins ~f:(function
        | Plugin.Extension.Auth (module Auth : Plugin.Auth with type session=session) ->
          Some Auth.mechanism
        | _ -> None)
      |> function
      | [] -> []
      | (_::_) as mechs ->
        [ Smtp_extension.Auth mechs ]
    in
    let other_extensions =
      List.filter_map plugins ~f:(function
        | Plugin.Extension.Auth _ -> None (* handled above *)
        | Plugin.Extension.Start_tls _ ->
          Option.map tls_options ~f:(const Smtp_extension.Start_tls))
    in
    [ Smtp_extension.Mime_8bit_transport ]
    @ auth_extensions
    @ other_extensions

  let rec session_loop
            ~log
            ~tls_options
            ~max_message_size
            ~malformed_emails
            ~server_events
            ~send_envelope
            ~quarantine
            ~session_flows
            ~reader
            ?raw_writer
            ?write_reply
            (session:Cb.Session.t) =
    let extensions session = extensions ~tls_options (Cb.Session.extensions session) in
    let write_reply = write_reply_impl ~log ?raw_writer ?write_reply () |> Staged.unstage in
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
    let ok_completed ~here ~flows ~component ?(extra=[]) msg =
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
    let service_unavailable ~here ~flows ~component () =
      write_reply ~here ~flows ~component Smtp_reply.service_unavailable_421
    in
    let start_mail_input ~here ~flows ~component () =
      write_reply ~here ~flows ~component Smtp_reply.start_mail_input_354
    in
    let syntax_error ~here ~flows ~component msg =
      write_reply ~here ~flows ~component (Smtp_reply.syntax_error_501 msg)
    in
    let transaction_failed ~here ~flows ~component msg =
      write_reply ~here ~flows ~component (Smtp_reply.transaction_failed_554 msg)
    in
    (* This is kind of like a state machine.
       [next] is the node the machine is on
       [next] returns Error for invalid transition
       [next] returns Ok fun for valid transitions
       fun is invoked on the result and this loops back into the state machine.
       [state] is meta data that gets passed through and mutated by the machine
    *)
    let loop ~flows ~component ~next =
      let component = component @ ["read-loop"] in
      let rec loop () =
        Reader.read_line reader
        >>= function
        | `Eof ->
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                "DISCONNECTED"));
          return ()
        | `Ok "" ->
          (*_ .Net System.Net.Mail.SmtpClient sends an unexpected empty line.
            It seems okay to silently skip these. *)
          Log.debug log (lazy (Log.Message.create
                                 ~here:[%here]
                                 ~flows
                                 ~component
                                 "Broken client sent empty line"));
          loop ()
        | `Ok input ->
          match Option.try_with (fun () -> Smtp_command.of_string input) with
          | None ->
            Log.debug log (lazy (Log.Message.create
                                   ~here:[%here]
                                   ~flows
                                   ~component
                                   ~tags:["command", input]
                                   "Got unexpected input on reader"));
            command_not_recognized ~here:[%here] ~flows ~component input
            >>= fun () ->
            loop ()
          | Some cmd ->
            Log.debug log (lazy (Log.Message.create
                                   ~here:[%here]
                                   ~flows
                                   ~component
                                   ~command:cmd
                                   "Got input on reader"));
            next cmd
      in
      loop ()
    in
    let rec top ~session : unit Deferred.t =
      let component = ["smtp-server"; "session"; "top"] in
      let flows = session_flows in
      loop ~flows ~component ~next:(function
        | Smtp_command.Quit ->
          closing_connection ~here:[%here] ~flows ~component ()
        | Smtp_command.Noop ->
          ok_continue ~here:[%here] ~flows ~component ()
          >>= fun () ->
          top ~session
        | Smtp_command.Reset ->
          ok_continue ~here:[%here] ~flows ~component ()
          >>= fun () ->
          top ~session
        | Smtp_command.Hello helo ->
          top_helo ~extended:false ~flows ~session helo
        | Smtp_command.Extended_hello helo ->
          top_helo ~extended:true ~flows ~session helo
        | Smtp_command.Auth (meth,initial_resp) ->
          top_auth ~flows ~session ~meth ~initial_resp
        | Smtp_command.Start_tls ->
          begin match
            (* We assume that [Cb.Session.extensions] only changes after a protocol
               upgrade. *)
            Cb.Session.extensions session
            |> List.find_map ~f:(function
              | Plugin.Extension.Start_tls cb ->
                Option.map tls_options ~f:(fun opts -> opts, cb)
              | _ -> None)
          with
          | None ->
            command_not_implemented ~here:[%here] ~flows ~component Smtp_command.Start_tls
            >>= fun () ->
            top ~session
          | Some (tls_options,cbs) ->
            top_start_tls ~session ~flows tls_options cbs
          end
        | Smtp_command.Sender sender ->
          top_envelope ~flows ~session sender
        | (Smtp_command.Recipient _ | Smtp_command.Data) as cmd ->
          bad_sequence_of_commands ~here:[%here] ~flows ~component cmd
          >>= fun () ->
          top ~session
        | (Smtp_command.Help) as cmd ->
          command_not_implemented ~here:[%here] ~flows ~component cmd
          >>= fun () ->
          top ~session )
    and top_helo ~flows ~extended ~session helo =
      let component = ["smtp-server"; "session"; "helo"] in
      Deferred.Or_error.try_with (fun () ->
        Cb.Session.helo
          ~log:(Log.with_flow_and_component log
                  ~flows:session_flows
                  ~component:(component @ ["plugin"; "Session.helo"]))
          session
          helo
      )
      >>= function
      | Ok (`Continue session) ->
        let extensions = extensions session in
        let greeting, extra =
          if extended then
            "Continue, extensions follow:", extensions
          else
            "Continue", []
        in
        ok_completed ~here:[%here] ~flows ~component ~extra greeting
        >>= fun () ->
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~tags:["extended", Bool.to_string extended; "helo", helo]
                              "session_helo:accepted"));
        top ~session
      | Ok (`Deny reply) ->
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~reply
                              ~tags:["extended", Bool.to_string extended; "helo", helo]
                              "session_helo:deny"));
        write_reply ~here:[%here] ~flows ~component reply
        >>= fun () ->
        top ~session
      | Ok (`Disconnect None) ->
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~tags:["extended", Bool.to_string extended; "helo", helo]
                              "session_helo:disconnect"));
        return ()
      | Ok (`Disconnect (Some reply)) ->
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~reply
                              ~tags:["extended", Bool.to_string extended; "helo", helo]
                              "session_helo:disconnect"));
        write_reply ~here:[%here] ~flows ~component reply
      | Error err ->
        Log.error log
          (lazy (Log.Message.of_error
                   ~here:[%here]
                   ~flows
                   ~component:(component @ ["plugin"; "Session.helo"])
                   ~tags:["extended", Bool.to_string extended; "helo", helo]
                   (Error.tag err ~tag:"session_helo")));
        service_unavailable ~here:[%here] ~flows ~component ()
    and top_start_tls ~flows ~session tls_options (module Tls_cb : Plugin.Start_tls with type session=Cb.Session.t) =
      let component = ["smtp-server"; "session"; "starttls"] in
      service_ready ~here:[%here] ~flows ~component "Begin TLS transition"
      >>= fun () ->
      let old_reader = reader in
      let old_writer = raw_writer |> Option.value_exn in
      let reader_pipe_r,reader_pipe_w = Pipe.create () in
      let writer_pipe_r,writer_pipe_w = Pipe.create () in
      Ssl.server
        ?version:tls_options.Config.Tls.version
        ?options:tls_options.Config.Tls.options
        ?name:tls_options.Config.Tls.name
        ?ca_file:tls_options.Config.Tls.ca_file
        ?ca_path:tls_options.Config.Tls.ca_path
        ~allowed_ciphers:tls_options.Config.Tls.allowed_ciphers
        ~crt_file:tls_options.Config.Tls.crt_file
        ~key_file:tls_options.Config.Tls.key_file
        (* Closing ssl connection will close the pipes which will in turn close
           the readers. *)
        ~net_to_ssl:(Reader.pipe old_reader)
        ~ssl_to_net:(Writer.pipe old_writer)
        ~ssl_to_app:reader_pipe_w
        ~app_to_ssl:writer_pipe_r
        ()
      >>= function
      | Error e ->
        Log.error ~dont_send_to_monitor:() log
          (lazy (Log.Message.of_error
                   ~here:[%here]
                   ~flows
                   ~component e));
        return ()
      | Ok tls ->
        Reader.of_pipe (Info.of_string "SMTP/TLS") reader_pipe_r
        >>= fun new_reader ->
        Writer.of_pipe (Info.of_string "SMTP/TLS") writer_pipe_w
        >>= fun (new_writer, `Closed_and_flushed_downstream closed_and_flushed) ->
        let version = Ssl.Connection.version tls in
        let v = Sexp.to_string_hum (Ssl.Version.sexp_of_t version) in
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              (sprintf "Using TLS protocol %s" v)));
        let teardown () =
          Writer.close new_writer
          >>= fun () ->
          closed_and_flushed
          >>= fun () ->
          Ssl.Connection.close tls;
          Ssl.Connection.closed tls
          >>| Result.iter_error ~f:(fun e ->
            Log.error ~dont_send_to_monitor:() log
              (lazy (Log.Message.of_error
                       ~here:[%here]
                       ~flows
                       ~component e)))
          >>= fun () ->
          Reader.close new_reader
        in
        Monitor.protect (fun () ->
          Tls_cb.upgrade_to_tls
            ~log:(Log.with_flow_and_component log
                    ~flows
                    ~component:(component @ ["plugin"; "Tls.upgrade_to_tls"]))
            session
          >>= fun session ->
          session_loop
            ~log
            ~tls_options:(Some tls_options)
            ~max_message_size
            ~malformed_emails
            ~server_events
            ~send_envelope
            ~quarantine
            ~session_flows
            ~reader:new_reader
            ~raw_writer:new_writer
            session)
          ~finally:teardown
    and top_auth ~flows ~session ~meth ~initial_resp =
      let component = ["smtp-server"; "session"; "auth"] in
      match Cb.Session.extensions session
            |> List.find_map ~f:(function
              | Plugin.Extension.Auth
                  ((module Auth : Plugin.Auth with type session=Cb.Session.t) as auth) ->
                Option.some_if (String.Caseless.equal meth Auth.mechanism) auth
              | _ -> None)
      with
      | None ->
        command_not_implemented ~here:[%here] ~flows ~component
          (Smtp_command.Auth (meth, None))
      | Some (module Auth : Plugin.Auth with type session=Cb.Session.t) ->
        let initial_resp = ref initial_resp in
        let auth_finished = ref false in
        let challenge_lock = ref false in
        let send_challenge_and_expect_response msg =
          if !auth_finished then
            failwith "Calling [send_challenge] after [authenticate] flow completed/aborted."
          else if !challenge_lock then
            failwith "Concurrent calls of [send_challenge]"
          else match !initial_resp with
            | Some resp ->
              initial_resp := None;
              Deferred.Or_error.return (Base64.decode_exn resp)
            | None ->
              challenge_lock := true;
              write_reply ~here:[%here] ~flows ~component
                (Smtp_reply.start_authentication_input_334
                   (Base64.encode msg))
              >>= fun () ->
              Reader.read_line reader
              >>| function
              | `Eof -> Error (Error.of_string "Client disconnected during authentication flow")
              | `Ok resp ->
                let resp = Base64.decode_exn resp in
                (* Deliberately only release the lock on success.
                   This ensures that calls after failure will continue to fail. *)
                challenge_lock := false;
                Ok resp
        in
        Deferred.Or_error.try_with (fun () ->
          Auth.negotiate
            ~log:(Log.with_flow_and_component log
                    ~flows
                    ~component:(component @ ["plugin"; "Auth.authenticate"]))
            session ~send_challenge_and_expect_response)
        >>| (fun res -> auth_finished := true; res)
        >>= function
        | Error err ->
          Log.error log (lazy (Log.Message.of_error
                                 ~here:[%here]
                                 ~flows
                                 ~component:(component @ ["plugin"; "Auth.authenticate"])
                                 err));
          write_reply ~here:[%here] ~flows ~component
            Smtp_reply.authentication_credentials_invalid_535
          >>= fun () ->
          top ~session
        | Ok (Error err) ->
          Log.error log (lazy (Log.Message.of_error
                                 ~here:[%here]
                                 ~flows
                                 ~component:(component @ ["plugin"; "Auth.authenticate"])
                                 err));
          Deferred.unit
        | Ok (Ok (`Deny reply)) ->
          write_reply ~here:[%here] ~flows ~component reply
          >>= fun () ->
          top ~session
        | Ok (Ok (`Allow session)) ->
          write_reply ~here:[%here] ~flows ~component
            Smtp_reply.authentication_successful_235
          >>= fun () ->
          top ~session
    and top_envelope ~flows ~session sender_str =
      let flows = Log.Flows.extend flows `Inbound_envelope in
      let component = ["smtp-server"; "session"; "envelope"; "sender"] in
      let allowed_extensions = extensions session in
      match Sender.of_string_with_arguments ~allowed_extensions sender_str with
      | Error err ->
        Log.info log (lazy (Log.Message.of_error
                              ~here:[%here]
                              ~flows
                              ~component
                              ~sender:(`String sender_str)
                              (Error.tag err ~tag:"Unable to parse MAIL FROM")));
        syntax_error ~here:[%here] ~flows ~component (sprintf "Cannot parse '%s'" sender_str)
        >>= fun () ->
        top ~session
      | Ok (sender,sender_args) ->
        Deferred.Or_error.try_with (fun () ->
          Cb.Envelope.mail_from session sender sender_args
            ~log:(Log.with_flow_and_component log
                    ~flows
                    ~component:(component @ ["plugin"; "Envelope.mail_from"])))
        >>= function
        | Ok (`Reject reject) ->
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                ~sender:(`String sender_str)
                                ~reply:reject
                                "mail_from:REJECTED"));
          write_reply ~here:[%here] ~flows ~component reject
          >>= fun () ->
          top ~session
        | Error err ->
          Log.error log
            (lazy (Log.Message.of_error
                     ~here:[%here]
                     ~flows
                     ~component:(component @ ["plugin"; "Envelope.mail_from"])
                     ~sender:(`String sender_str)
                     (Error.tag err ~tag:"mail_from")));
          service_unavailable ~here:[%here] ~flows ~component ()
          >>= fun () ->
          top ~session
        | Ok (`Continue data) ->
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                ~sender:(`String sender_str)
                                ~session_marker:`Mail_from
                                "MAIL FROM"));
          ok_continue ~here:[%here] ~flows ~component ()
          >>= fun () ->
          envelope ~session ~flows data
    and envelope ~session ~flows data =
      let component = ["smtp-server"; "session"; "envelope"; "top"] in
      loop ~flows ~component ~next:(function
        | Smtp_command.Quit ->
          closing_connection ~here:[%here] ~flows ~component ()
        | Smtp_command.Noop ->
          ok_continue ~here:[%here] ~flows ~component ()
          >>= fun () ->
          envelope ~session ~flows data
        | Smtp_command.Recipient recipient ->
          envelope_recipient ~session ~flows data recipient
        | Smtp_command.Data ->
          envelope_data ~session ~flows data
        | ( Smtp_command.Hello _ | Smtp_command.Extended_hello _
          | Smtp_command.Sender _ | Smtp_command.Start_tls
          | Smtp_command.Auth _) as cmd ->
          bad_sequence_of_commands ~here:[%here] ~flows ~component cmd
          >>= fun () ->
          envelope ~session ~flows data
        | Smtp_command.Reset ->
          ok_continue ~here:[%here] ~flows ~component () >>= fun () -> top ~session
        | (Smtp_command.Help) as cmd ->
          command_not_implemented ~here:[%here] ~flows ~component cmd
          >>= fun () ->
          envelope ~session ~flows data )
    and envelope_recipient ~session ~flows data recipient_str =
      let component = ["smtp-server"; "session"; "envelope"; "recipient"] in
      match Email_address.of_string recipient_str with
      | Error err ->
        Log.info log (lazy (Log.Message.of_error
                              ~here:[%here]
                              ~flows
                              ~component
                              ~recipients:[`String recipient_str]
                              (Error.tag err ~tag:"cannot parse recipient")));
        syntax_error ~here:[%here] ~flows ~component (sprintf "Cannot parse %s" recipient_str)
        >>= fun () ->
        envelope ~session ~flows data
      | Ok recipient ->
        Deferred.Or_error.try_with (fun () ->
          Cb.Envelope.rcpt_to session data recipient
            ~log:(Log.with_flow_and_component log
                    ~flows
                    ~component:(component @ ["plugin"; "Envelope.rcpt_to"])))
        >>= function
        | Ok (`Reject reject) ->
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                ~recipients:[`Email recipient]
                                ~reply:reject
                                "rcpt_to:REJECTED"));
          write_reply ~here:[%here] ~flows ~component reject
          >>= fun () ->
          envelope ~session ~flows data
        | Error err ->
          Log.error log
            (lazy (Log.Message.of_error
                     ~here:[%here]
                     ~flows
                     ~component:(component @ ["plugin"; "Envelope.rcpt_to"])
                     ~recipients:[`Email recipient]
                     (Error.tag err ~tag:"rcpt_to")));
          service_unavailable ~here:[%here] ~flows ~component ()
          >>= fun () ->
          envelope ~session ~flows data
        | Ok (`Continue data) ->
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                ~recipients:[`Email recipient]
                                ~session_marker:`Rcpt_to
                                "RCPT TO"));
          ok_continue ~here:[%here] ~flows ~component ()
          >>= fun () ->
          envelope ~session ~flows data
    and envelope_data ~session ~flows data =
      let component = ["smtp-server"; "session"; "envelope"; "data"] in
      Deferred.Or_error.try_with (fun () ->
        Cb.Envelope.accept_data session data
          ~log:(Log.with_flow_and_component log
                  ~flows
                  ~component:(component @ ["plugin"; "Envelope.accept_data"])))
      >>= function
      | Ok (`Reject reject) ->
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component:(component @ ["plugin"; "Envelope.accept_data"])
                              ~reply:reject
                              "Envelope.accept_data:REJECTED"));
        write_reply ~here:[%here] ~flows ~component reject
        >>= fun () ->
        envelope ~session ~flows data
      | Error err ->
        Log.error log
          (lazy (Log.Message.of_error
                   ~here:[%here]
                   ~flows
                   ~component:(component @ ["plugin"; "Envelope.accept_data"])
                   (Error.tag err ~tag:"process_envelope")));
        service_unavailable ~here:[%here] ~flows ~component ()
        >>= fun () ->
        envelope ~session ~flows data
      | Ok (`Continue data) ->
        start_mail_input ~here:[%here] ~flows ~component ()
        >>= fun () ->
        read_data ~max_size:max_message_size reader
        >>= function
        | `Too_much_data ->
          write_reply ~here:[%here] ~flows ~component Smtp_reply.exceeded_storage_allocation_552
          >>= fun () ->
          top ~session
        | `Ok raw ->
          Log.debug log (lazy (Log.Message.create
                                 ~here:[%here]
                                 ~flows
                                 ~component
                                 ~tags:["data", Bigbuffer.contents raw]
                                 "DATA"));
          let email =
            match Or_error.try_with (fun () -> Email.of_bigbuffer raw) with
            | Ok email -> Ok email
            | Error error ->
              Log.error ~dont_send_to_monitor:() log
                (lazy (Log.Message.of_error
                         ~here:[%here]
                         ~flows
                         ~component
                         error));
              match malformed_emails with
              | `Reject ->
                Error (Smtp_reply.syntax_error_501 "Malformed Message")
              | `Wrap ->
                let raw = Bigbuffer.contents raw in
                Ok (Email.Simple.Content.text
                      ~extra_headers:["X-JS-Parse-Error", sprintf !"%{sexp:Error.t}" error]
                      raw :> Email.t)
          in
          match email with
          | Error error ->
            write_reply ~here:[%here] ~flows ~component error
            >>= fun () ->
            top ~session
          | Ok email ->
            let original_msg =
              Envelope.create' ~info:(Cb.Envelope.smtp_envelope_info data) ~email
            in
            Smtp_events.envelope_received server_events original_msg;
            Log.info log (lazy (Log.Message.create
                                  ~here:[%here]
                                  ~flows
                                  ~component
                                  ~email:(`Envelope original_msg)
                                  ~session_marker:`Data
                                  "DATA"));
            let component = ["smtp-server"; "session"; "envelope"; "routing"] in
            Deferred.Or_error.try_with (fun () ->
              Cb.Envelope.process session data email
                ~log:(Log.with_flow_and_component log
                        ~flows
                        ~component:(component @ ["plugin"; "Envelope.data"])))
            >>= function
            | Ok (`Reject reject) ->
              Log.info log (lazy (Log.Message.create
                                    ~here:[%here]
                                    ~flows
                                    ~component
                                    ~reply:reject
                                    "process_envelope:REJECTED"));
              write_reply ~here:[%here] ~flows ~component reject
              >>= fun () ->
              top ~session
            | Error err ->
              Log.error log
                (lazy (Log.Message.of_error
                         ~here:[%here]
                         ~flows
                         ~component:(component @ ["plugin"; "Envelope.data"])
                         (Error.tag err ~tag:"process_envelope")));
              service_unavailable ~here:[%here] ~flows ~component ()
              >>= fun () ->
              top ~session
            | Ok (`Consume ok) ->
              Log.info log (lazy (Log.Message.create
                                    ~here:[%here]
                                    ~flows
                                    ~component
                                    ~tags:["consumed-id", ok]
                                    "process_envelope:CONSUMED"));
              ok_completed ~here:[%here] ~flows ~component ok
              >>= fun () ->
              top ~session
            | Ok (`Quarantine (envelopes_with_next_hops, reply, reason)) ->
              begin
                let component = ["smtp-server"; "session"; "envelope"; "quarantine"] in
                quarantine ~flows ~reason ~original_msg envelopes_with_next_hops
                >>= function
                | Error err ->
                  List.iter envelopes_with_next_hops ~f:(fun envelope_with_next_hops ->
                    Log.error log
                      (lazy (Log.Message.of_error
                               ~here:[%here]
                               ~flows
                               ~component
                               ~email:(`Envelope (Envelope.With_next_hop.envelope envelope_with_next_hops))
                               ~tags:(List.map (Envelope.With_next_hop.next_hop_choices envelope_with_next_hops)
                                        ~f:(fun c -> "next-hop",Address.to_string c))
                               (Error.tag err ~tag:"quarantining"))));
                  transaction_failed ~here:[%here] ~flows ~component "error spooling"
                  >>= fun () ->
                  top ~session
                | Ok () ->
                  List.iter envelopes_with_next_hops ~f:(fun envelope_with_next_hops ->
                    let msg_id =
                      Envelope.With_next_hop.envelope envelope_with_next_hops
                      |> Envelope.id |> Envelope.Id.to_string
                    in
                    Log.info log (lazy (Log.Message.create
                                          ~here:[%here]
                                          ~flows
                                          ~component
                                          ~spool_id:msg_id
                                          ~tags:["quarantine-reason", Quarantine_reason.to_string reason]
                                          "QUARANTINED")));
                  write_reply ~here:[%here] ~flows ~component reply
                  >>= fun () ->
                  top ~session
              end
            | Ok (`Send envelopes_with_next_hops) ->
              let component = ["smtp-server"; "session"; "envelope"; "spooling"] in
              send_envelope ~flows ~original_msg envelopes_with_next_hops
              >>= function
              | Error err ->
                List.iter envelopes_with_next_hops ~f:(fun envelope_with_next_hops ->
                  Log.error log
                    (lazy (Log.Message.of_error
                             ~here:[%here]
                             ~flows
                             ~component
                             ~email:(`Envelope (Envelope.With_next_hop.envelope envelope_with_next_hops))
                             ~tags:(List.map (Envelope.With_next_hop.next_hop_choices envelope_with_next_hops)
                                      ~f:(fun c -> "next-hop",Address.to_string c))
                             (Error.tag err ~tag:"spooling"))));
                transaction_failed ~here:[%here] ~flows ~component "error spooling"
                >>= fun () ->
                top ~session
              | Ok id ->
                List.iter envelopes_with_next_hops ~f:(fun envelope_with_next_hops ->
                  let sender =
                    `Sender (Envelope.With_next_hop.sender envelope_with_next_hops)
                  in
                  let recipients =
                    Envelope.With_next_hop.recipients envelope_with_next_hops
                    |> List.map ~f:(fun email -> `Email email)
                  in
                  let message_size =
                    Envelope.With_next_hop.email envelope_with_next_hops
                    |> Email.raw_content
                    |> Bigstring_shared.length
                  in
                  Log.info log (lazy (Log.Message.create
                                        ~here:[%here]
                                        ~flows
                                        ~component
                                        ~spool_id:id
                                        ~sender
                                        ~recipients
                                        ~message_size
                                        ~tags:(List.map (Envelope.With_next_hop.next_hop_choices envelope_with_next_hops)
                                                 ~f:(fun c -> "next-hop",Address.to_string c))
                                        "SPOOLED")));
                ok_completed ~here:[%here] ~flows ~component (sprintf "id=%s" id)
                >>= fun () ->
                top ~session
    in
    top ~session
  ;;

  let start_session
        ~log
        ~malformed_emails
        ~tls_options
        ~emulate_tls_for_test
        ~max_message_size
        ~reader
        ~server_events
        ?raw_writer
        ?write_reply
        ~send_envelope
        ~quarantine
        ~session_flows
        ~local_address
        ~remote_address
        () =
    let write_reply' = write_reply_impl ~log ?raw_writer ?write_reply () |> Staged.unstage in
    let component = [ "smtp-server"; "session"; "init" ] in
    let flows = session_flows in
    Log.info log (lazy (Log.Message.create
                          ~here:[%here]
                          ~flows
                          ~component
                          ~local_address
                          ~remote_address
                          ~session_marker:`Connected
                          "CONNECTED"));
    Deferred.Or_error.try_with (fun () ->
      Cb.Session.connect
        ~log:(Log.with_flow_and_component log
                ~flows
                ~component:(component @ ["plugin"; "Session.connect"]))
        ~local:local_address
        ~remote:remote_address)
    >>= function
    | Ok (`Disconnect None) ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            "Session.connect:disconnect"));
      return ()
    | Ok (`Disconnect (Some reply)) ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            ~reply
                            "Session.connect:disconnect"));
      write_reply' ~here:[%here] ~flows ~component reply
    | Error err ->
      Log.info log (lazy (Log.Message.of_error
                            ~here:[%here]
                            ~flows
                            ~component
                            (Error.tag err ~tag:"Session.connect")));
      write_reply' ~here:[%here] ~flows ~component
        Smtp_reply.service_unavailable_421
    | Ok (`Accept session) ->
      begin if emulate_tls_for_test then (
        match
          List.find_map (Cb.Session.extensions session) ~f:(function
            | Plugin.Extension.Start_tls cb -> Some cb
            | _ -> None)
        with
        | None ->
          Deferred.Or_error.errorf
            "Session initiated with claim of pre-established TLS \
             but Plugin does not provide TLS callback"
        | Some (module Tls : Plugin.Start_tls with type session = Cb.Session.t) ->
          Deferred.Or_error.try_with (fun () ->
            Tls.upgrade_to_tls
              ~log:(Log.with_flow_and_component log
                      ~flows
                      ~component:(component @ ["plugin"; "Tls.upgrade_to_tls"]))
              session))
        else Deferred.Or_error.return session
      end
      >>= function
      | Error err ->
        Log.info log (lazy (Log.Message.of_error
                              ~here:[%here]
                              ~flows
                              ~component
                              (Error.tag err ~tag:"Session.connect")));
        write_reply' ~here:[%here] ~flows ~component
          Smtp_reply.service_unavailable_421
      | Ok session ->
        let greeting = Cb.Session.greeting session in
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~tags:["greeting", greeting]
                              "Session.connect:accepted"));
        Monitor.protect (fun () ->
          write_reply' ~here:[%here] ~flows ~component
            (Smtp_reply.service_ready_220 greeting)
          >>= fun () ->
          session_loop
            ~log
            ~tls_options
            ~max_message_size
            ~malformed_emails
            ~server_events
            ~reader
            ?raw_writer
            ?write_reply
            ~send_envelope
            ~quarantine
            ~session_flows
            session)
          ~finally:(fun () ->
            Cb.Session.disconnect ~log session)
  ;;

  type server = Inet of Tcp.Server.inet | Unix of Tcp.Server.unix

  type t =
    { config       : Config.t;
      (* One server per port *)
      servers      : server list;
      spool        : Spool.t
    }
  ;;

  let tcp_servers ~spool ~config ~log ~server_events =
    let start_servers where ~make_local_address ~make_tcp_where_to_listen
          ~make_remote_address ~to_server =
      let local_address = make_local_address where in
      Tcp.Server.create (make_tcp_where_to_listen where)
        ~on_handler_error:(`Call (fun remote_address exn ->
          let remote_address = make_remote_address remote_address in
          (* Silence the [inner_monitor] errors *)
          Log.error log ~dont_send_to_monitor:()
            (lazy (Log.Message.of_error
                     ~here:[%here]
                     ~flows:Log.Flows.none
                     ~component:["smtp-server";"tcp"]
                     ~local_address
                     ~remote_address
                     (Error.of_exn ~backtrace:`Get exn)))))
        ~max_connections:(Config.max_concurrent_receive_jobs_per_port config)
        (fun address_in reader writer ->
           let session_flows = Log.Flows.create `Server_session in
           let remote_address = make_remote_address address_in in
           let send_envelope ~flows ~original_msg envelopes_with_next_hops =
             Spool.add spool ~flows ~original_msg envelopes_with_next_hops
             >>|? Envelope.Id.to_string
           in
           let quarantine ~flows ~reason ~original_msg messages =
             Spool.quarantine spool ~flows ~reason ~original_msg messages
           in
           Deferred.Or_error.try_with (fun () ->
             start_session
               ~log
               ~server_events
               ~tls_options:config.Config.tls_options
               ~emulate_tls_for_test:false
               ~malformed_emails:config.Config.malformed_emails
               ~max_message_size:config.Config.max_message_size
               ~session_flows
               ~reader ~raw_writer:writer ~send_envelope ~quarantine
               ~local_address
               ~remote_address
               ()
           )
           >>| Result.iter_error ~f:(fun err ->
             Log.error log
               (lazy (Log.Message.of_error
                        ~here:[%here]
                        ~flows:session_flows
                        ~component:["smtp-server"; "tcp"]
                        ~local_address
                        ~remote_address
                        err))))
      >>| to_server
    in
    Deferred.List.map ~how:`Parallel (Config.where_to_listen config) ~f:(function
      | `Port port ->
        let make_local_address port = `Inet (Host_and_port.create ~host:"0.0.0.0" ~port) in
        let make_tcp_where_to_listen = Tcp.on_port in
        let make_remote_address (`Inet (inet_in, port_in)) =
          `Inet (Host_and_port.create ~host:(Unix.Inet_addr.to_string inet_in) ~port:port_in)
        in
        let to_server s = Inet s in
        start_servers port ~make_local_address ~make_tcp_where_to_listen
          ~make_remote_address ~to_server
      | `File file ->
        let make_local_address socket = `Unix socket in
        let make_tcp_where_to_listen = Tcp.on_file in
        let make_remote_address (`Unix socket_in) = `Unix socket_in in
        let to_server s = Unix s in
        start_servers file ~make_local_address ~make_tcp_where_to_listen
          ~make_remote_address ~to_server)
  ;;

  let start ~config ~log =
    let server_events = Smtp_events.create () in
    Spool.create ~config ~log ()
    >>=? fun spool ->
    tcp_servers ~spool ~config ~log ~server_events
    >>| fun servers ->
    don't_wait_for
      (Rpc_server.start (config, spool, server_events) ~log ~plugin_rpcs:(Cb.rpcs ()));
    Ok { config; servers; spool }
  ;;

  let config t = t.config
  ;;

  let ports t = List.filter_map t.servers ~f:(function
    | Inet server -> Some (Tcp.Server.listening_on server)
    | Unix _server -> None)

  let close ?timeout t =
    Deferred.List.iter ~how:`Parallel t.servers ~f:(function
      | Inet server -> Tcp.Server.close server
      | Unix server -> Tcp.Server.close server)
    >>= fun () ->
    Deferred.List.iter ~how:`Parallel t.servers ~f:(function
      | Inet server -> Tcp.Server.close_finished server
      | Unix server -> Tcp.Server.close_finished server)
    >>= fun () ->
    Spool.kill_and_flush ?timeout t.spool
    >>= function
    | `Finished -> Deferred.Or_error.ok_unit
    | `Timeout  -> Deferred.Or_error.error_string "Messages remaining in queue"
end

module For_test(P : Plugin.S) = struct
  include Make(P)
  let session
        ?(send=(fun _ -> failwith "Sending is not implemented"))
        ?(quarantine=(fun ~reason:_ _ -> failwith "Quarantine is not implemented"))
        ~log
        ?(max_message_size=Byte_units.create `Bytes (Float.of_int Int.max_value_30_bits))
        ?tls_options
        ?(emulate_tls=false)
        ?(malformed_emails=`Reject)
        ?(local=(`Inet (Host_and_port.create ~host:"0.0.0.0" ~port:0)))
        ~remote
        reader
        writer =
    start_session
      ~log
      ~malformed_emails
      ~tls_options
      ~emulate_tls_for_test:emulate_tls
      ~max_message_size
      ~reader
      ~server_events:(Smtp_events.create ())
      ~raw_writer:writer
      ~send_envelope:(fun ~flows:_ ~original_msg:_ msgs -> send msgs)
      ~quarantine:(fun ~flows:_ ~reason ~original_msg:_ msgs -> quarantine ~reason msgs)
      ~session_flows:(Mail_log.Flows.create `Server_session)
      ~local_address:local
      ~remote_address:remote
      ()
end

let bsmtp_log = Lazy.map Async.Log.Global.log ~f:(fun log ->
  log
  |> Log.adjust_log_levels ~remap_info_to:`Debug)

let read_bsmtp ?(log=Lazy.force bsmtp_log) reader =
  let server_events = Smtp_events.create () in
  Pipe.create_reader ~close_on_exception:true (fun out ->
    let session_flows = Log.Flows.create `Server_session in
    let module Smtp =
      Make(struct
        module Session = Plugin.Simple.Session
        module Envelope = struct
          include Plugin.Simple.Envelope
          let process ~log:_ _session t email =
            let envelope = smtp_envelope t email in
            Pipe.write out (Ok envelope)
            >>| fun () ->
            `Consume "bsmtp"
        end
        let rpcs = Plugin.Simple.rpcs
      end)
    in
    Smtp.start_session
      ~log
      ~tls_options:Config.default.tls_options
      ~emulate_tls_for_test:false
      ~max_message_size:Config.default.max_message_size
      ~malformed_emails:Config.default.malformed_emails
      ~reader
      ~server_events
      ~session_flows
      ?raw_writer:None
      ~write_reply:(fun reply ->
        if Smtp_reply.is_ok reply then return ()
        else begin
          Pipe.write out (Or_error.error_string (Smtp_reply.to_string reply))
        end)
      ~send_envelope:(fun ~flows:_ ~original_msg:_ _ ->
        Deferred.Or_error.error_string "Not implemented")
      ~quarantine:(fun ~flows:_ ~reason:_ ~original_msg:_ _ ->
        Deferred.Or_error.error_string "Not implemented")
      ~local_address:(`Unix "*pipe*")
      ~remote_address:(`Unix "*pipe*")
      ())
;;

let mbox_log = Lazy.map Async.Log.Global.log ~f:(fun log ->
  log
  |> Log.adjust_log_levels ~remap_info_to:`Debug)

let read_mbox ?(log=Lazy.force mbox_log) reader =
  ignore log;
  let buffer = Bigbuffer.create 50000 in
  let starts_new_message line =
    (* This is fine: the RFC specifies that lines starting with "From " in the
       body of the message must be escaped like "> From". No comment on that
       design decision. *)

    String.is_prefix line ~prefix:"From "
  in
  (* http://tools.ietf.org/html/rfc5321#section-4.5.2 *)
  let escape_single_dot = function
    | "." -> ".."
    | line -> line
  in
  let message () =
    let email = Email.of_bigbuffer buffer in
    Bigbuffer.clear buffer;
    Envelope.of_email email
  in
  let rec read_msg () =
    Reader.read_line reader
    >>= function
    | `Eof ->
      if Bigbuffer.length buffer = 0
      then return `Eof
      else return (`Ok (message ()))
    | `Ok line ->
      if starts_new_message line
      then begin
        (* Discard the "From " line since it's not part of the message body. *)
        if Bigbuffer.length buffer = 0
        then read_msg ()
        else return (`Ok (message ()))
      end
      else begin
        Bigbuffer.add_string buffer (escape_single_dot line);
        Bigbuffer.add_char buffer '\n';
        read_msg ()
      end
  in
  let rec read_msgs out =
    read_msg ()
    >>= function
    | `Eof -> return ()
    | `Ok msg ->
      Pipe.write out msg
      >>= fun () ->
      read_msgs out
  in
  Pipe.create_reader ~close_on_exception:true read_msgs
;;
