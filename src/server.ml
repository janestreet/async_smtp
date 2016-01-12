open Core.Std
open Async.Std
open Async_ssl.Std
open Types
open Email_message.Std

module Config = Server_config


module Callbacks = Server_callbacks
;;

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




module Glog(S:sig
    val session : Session.t
  end) = struct
  let tags =
    [ "local",   Host_and_port.to_string S.session.Session.local
    ; "remote",  Host_and_port.to_string S.session.Session.remote
    ; "session", S.session.Session.id
    ]
  let debug = Log.Global.debug ~tags
  let info = Log.Global.info ~tags
  let error = Log.Global.error ~tags
end

let rec start_session
    ~config
    ~reader
    ?(is_protocol_upgrade=false)
    ?writer
    ?write_reply
    ~send_envelope
    ~quarantine
    ~session
    (module Cb : Callbacks.S) =
  let module Glog = Glog(struct let session = session end) in
  let write_reply_default reply =
    match writer with
    | Some writer ->
      Writer.write_line writer (Reply.to_string reply);
      Writer.flushed writer
    | None ->
      if Reply.is_ok reply then Deferred.unit
      else failwithf !"Unconsumed failure: %{Reply}" reply ()
  in
  let write_reply = Option.value write_reply ~default:write_reply_default in
  let write_reply reply =
    Glog.debug !"Writing reply: %{Reply}" reply;
    write_reply reply
  in
  let write_maybe_reply = function
    | Some reply -> write_reply reply
    | None -> return ()
  in
  let add_session_id msg = sprintf "%s [session=%s]" msg session.Session.id in
  let bad_sequence_of_commands cmd =
    let msg = Command.to_string cmd |> add_session_id in
    write_reply (Reply.Bad_sequence_of_commands_503 msg)
  in
  let closing_connection () =
    write_reply Reply.Closing_connection_221
  in
  let command_not_implemented cmd =
    let msg = Command.to_string cmd |> add_session_id in
    write_reply (Reply.Command_not_implemented_502 msg)
  in
  let command_not_recognized msg =
    let msg = add_session_id msg in
    write_reply (Reply.Command_not_recognized_500 msg)
  in
  let ok_completed ?(extra=[]) msg =
    let msg = String.concat ~sep:"\n" (add_session_id msg :: extra) in
    write_reply (Reply.Ok_completed_250 msg)
  in
  let ok_continue () =
    ok_completed "continue"
  in
  let service_ready msg =
    let msg = add_session_id msg in
    write_reply (Reply.Service_ready_220 msg) in
  let start_mail_input () =
    write_reply Reply.Start_mail_input_354
  in
  let syntax_error msg =
    let msg = add_session_id msg in
    write_reply (Reply.Syntax_error_501 msg)
  in
  let transaction_failed msg =
    let msg = add_session_id msg in
    write_reply (Reply.Transaction_failed_554 msg)
  in
  (* This is kind of like a state machine.
     [next] is the node the machine is on
        [next] returns Error for invalid transition
        [next] returns Ok fun for valid transitions
           fun is invoked on the result and this loops back into the state machine.
     [state] is meta data that gets passed through and mutated by the machine
  *)
  let rec loop ~next =
    Reader.read_line reader
    >>= function
    | `Eof ->
      Glog.debug "Got Eof on reader";
      return ()
    | `Ok input ->
      match Option.try_with (fun () -> Command.of_string input) with
      | None ->
        Glog.debug "Got unexpected input on reader: %s" input;
        command_not_recognized input
        >>= fun () ->
        loop ~next
      | Some cmd ->
        Glog.debug "Got input on reader: %s" input;
        next cmd
  in
  let rec top ~session =
    loop ~next:(function
      | Command.Quit ->
        closing_connection ()
      | Command.Noop ->
        ok_continue () >>= fun () -> top ~session
      | Command.Reset ->
        ok_continue () >>= fun () -> top ~session
      | Command.Hello helo ->
        top_helo ~extended:false ~session helo
      | Command.Extended_hello helo ->
        top_helo ~extended:true ~session helo
      | Command.Start_tls -> begin
        match config.Config.tls_options with
        | None ->
          command_not_implemented Command.Start_tls >>= fun () -> top ~session
        | Some tls_options ->
          top_start_tls ~session tls_options
        end
      | Command.Sender sender ->
        top_envelope ~session sender
      | (Command.Recipient _ | Command.Data) as cmd ->
        bad_sequence_of_commands cmd >>= fun () -> top ~session
      | (Command.Help) as cmd ->
        command_not_implemented cmd >>= fun () -> top ~session)
  and top_helo ~extended ~session helo =
    Cb.session_helo ~session helo
    >>= function
    | `Continue ->
      let extensions =
        (match Session.tls session with
         | None -> [ "STARTTLS" ]
         | Some _ -> [])
        @
        [ "8BITMIME" ]
      in
      let greeting, extra =
        if extended then
          "Continue, extensions follow:", extensions
        else
          "Continue, extensions follow: " ^ String.concat ~sep:", " extensions, []
      in
      ok_completed ~extra greeting
      >>= fun () ->
      top ~session
    | `Deny reject ->
      write_reply reject
      >>= fun () ->
      top ~session
    | `Disconnect reject ->
      write_maybe_reply reject
  and top_start_tls ~session tls_options =
    service_ready "Begin TLS transition"
    >>= fun () ->
    let old_reader = reader in
    let old_writer = writer |> Option.value_exn in
    let reader_pipe_r,reader_pipe_w = Pipe.create () in
    let writer_pipe_r,writer_pipe_w = Pipe.create () in
    Ssl.server
      ?version:tls_options.Config.Tls.version
      ?name:tls_options.Config.Tls.name
      ?ca_file:tls_options.Config.Tls.ca_file
      ?ca_path:tls_options.Config.Tls.ca_path
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
      Glog.error !"Failed to start TLS, aborting session: %{sexp:Error.t}" e;
      return ()
    | Ok tls ->
      let session =
        Session.create
          ~id:(session.Session.id ^ "|TLS")
          ~local:session.Session.local
          ~remote:session.Session.remote
          ~tls
          ()
      in
      Reader.of_pipe (Info.of_string "SMTP/TLS") reader_pipe_r
      >>= fun new_reader ->
      Writer.of_pipe (Info.of_string "SMTP/TLS") writer_pipe_w
      >>= fun (new_writer, _) ->
      Deferred.all_ignore
        [ begin
          Ssl.Connection.closed tls
          >>= fun result ->
          Result.iter_error result ~f:(fun e ->
            Glog.error !"TLS error: %{sexp:Error.t}" e);
          Deferred.all_unit
            [ Reader.close new_reader
            ; Writer.flushed new_writer
              >>= fun () ->
              Writer.close new_writer
            ]
        end
        ; begin
          start_session
            ~config
            ~reader:new_reader
            ~writer:new_writer
            ~send_envelope
            ~quarantine
            ~session
            ~is_protocol_upgrade:true
            (module Cb : Callbacks.S)
        end
        ]
  and top_envelope ~session sender =
    match Sender.of_string sender with
    | Error _err ->
      syntax_error (sprintf "Cannot parse %s" sender)
      >>= fun () ->
      top ~session
    | Ok sender ->
      Cb.process_sender ~session sender
      >>= function
      | `Reject reject ->
        write_reply reject
        >>= fun () ->
        top ~session
      | `Continue ->
        ok_continue ()
        >>= fun () ->
        envelope ~session ~sender ~recipients:[] ~rejected_recipients:[]
  and envelope ~session ~sender ~recipients ~rejected_recipients =
    loop ~next:(function
      | Command.Quit ->
        closing_connection ()
      | Command.Noop ->
        ok_continue () >>= fun () -> envelope ~session ~sender ~recipients ~rejected_recipients
      | Command.Recipient recipient ->
        envelope_recipient ~session ~sender ~recipients ~rejected_recipients recipient
      | Command.Data when not (List.is_empty recipients) ->
        envelope_data ~session ~sender ~recipients ~rejected_recipients
      | ( Command.Hello _ | Command.Extended_hello _
        | Command.Sender _ | Command.Data
        | Command.Start_tls
        ) as cmd ->
        bad_sequence_of_commands cmd
        >>= fun () -> envelope ~session ~sender ~recipients ~rejected_recipients
      | Command.Reset ->
        ok_continue () >>= fun () -> top ~session
      | (Command.Help) as cmd ->
        command_not_implemented cmd
        >>= fun () -> envelope ~session ~sender ~recipients ~rejected_recipients)
  and envelope_recipient ~session ~sender ~recipients ~rejected_recipients recipient =
    match Email_address.of_string recipient with
    | Error _ ->
      syntax_error (sprintf "Cannot parse %s" recipient)
      >>= fun () ->
      envelope ~session ~sender ~recipients ~rejected_recipients
    | Ok recipient ->
      Cb.process_recipient ~session ~sender recipient
      >>= function
      | `Reject reject ->
        let rejected_recipients = rejected_recipients @ [recipient] in
        write_reply reject
        >>= fun () ->
        envelope ~session ~sender ~recipients ~rejected_recipients
      | `Continue ->
        let recipients = recipients @ [recipient] in
        ok_continue ()
        >>= fun () ->
        envelope ~session ~sender ~recipients ~rejected_recipients
  and envelope_data ~session ~sender ~recipients ~rejected_recipients =
    start_mail_input ()
    >>= fun () ->
    read_data ~max_size:config.Config.max_message_size reader
    >>= function
    | `Too_much_data ->
      write_reply Reply.Exceeded_storage_allocation_552
      >>= fun () ->
      top ~session
    | `Ok data ->
      Glog.debug !"Got data on reader: %s" (data |> Bigbuffer.contents);
      let email =
        match Email.of_bigbuffer data with
        | Ok email -> Ok email
        | Error error ->
          Glog.info !"Syntax error in message %{sexp:Error.t}" error;
          match config.Config.malformed_emails with
          | `Reject ->
            Error (Reply.Syntax_error_501 "Malformed Message")
          | `Wrap ->
            let data = data |> Bigbuffer.contents in
            Ok (Email.Simple.Content.text
                  ~extra_headers:["X-JS-Parse-Error", sprintf !"%{sexp:Error.t}" error]
                  data :> Email.t)
      in
      match email with
      | Error error ->
        write_reply error
        >>= fun () ->
        top ~session
      | Ok email ->
        Glog.debug !"Parsed message: %{sexp:Email.t}" email;
        let original_msg =
          Envelope.create ~sender ~recipients ~rejected_recipients ~email ()
        in
        Cb.process_envelope ~session original_msg
        >>= function
        | `Reject reject ->
          write_reply reject
          >>= fun () ->
          top ~session
        | `Consume ok ->
          ok_completed ok
          >>= fun () ->
          top ~session
        | `Quarantine (envelopes_with_next_hops, reply, reason) ->
          begin
            quarantine ~reason ~original_msg envelopes_with_next_hops
            >>= function
            | Error err ->
              Glog.error
                !"Error quarantining. \
                  error: %{sexp:Error.t} \
                  original_msg: %{sexp: Envelope.t} \
                  envelope_with_next_hops: %{sexp: Envelope_with_next_hop.t list}"
                err original_msg envelopes_with_next_hops;
              transaction_failed "error spooling"
              >>= fun () ->
              top ~session
            | Ok () ->
              write_reply reply
              >>= fun () ->
              top ~session
          end
        | `Send envelopes_with_next_hops ->
          send_envelope ~original_msg envelopes_with_next_hops
          >>= function
          | Error err ->
            Glog.error
              !"Error spooling. \
                error: %{sexp:Error.t} \
                original_msg: %{sexp: Envelope.t} \
                envelope_with_next_hops: %{sexp: Envelope_with_next_hop.t list}"
              err original_msg envelopes_with_next_hops;
            transaction_failed "error spooling"
            >>= fun () ->
            top ~session
          | Ok id ->
            Glog.info "Messages spooled with id=%s" id;
            ok_completed (sprintf "id=%s" id)
            >>= fun () ->
            top ~session
  in
  if is_protocol_upgrade
  then top ~session
  else begin
    Cb.session_connect ~session
    >>= function
    | `Disconnect reject ->
      Glog.debug !"Rejected session: %{sexp: Reply.t option}" reject;
      write_maybe_reply reject
    | `Accept hello ->
      Glog.info "Starting session";
      service_ready hello
      >>= fun () ->
      top ~session
  end

;;

module Protect_callbacks(Cb:Callbacks.S) : Callbacks.S = struct
  let session_connect ~session =
    Monitor.try_with (fun () ->
        Cb.session_connect ~session
      )
    >>| function
    | Ok res -> res
    | Error exn ->
      let module Glog = Glog(struct let session = session end) in
      Glog.error !"Exception in `Cb.session_connect %{sexp:Session.t}`\n %{Exn}" session exn;
      `Disconnect (Some Reply.Service_unavailable_421)

  let session_helo ~session helo =
    Monitor.try_with (fun () ->
        Cb.session_helo ~session helo
      )
    >>| function
    | Ok res -> res
    | Error exn ->
      let module Glog = Glog(struct let session = session end) in
      Glog.error !"Exception in `Cb.session_helo %{sexp:Session.t} %{sexp:string}`\n %{Exn}" session helo exn;
      `Deny Reply.Service_unavailable_421

  let process_sender ~session sender =
    Monitor.try_with (fun () ->
        Cb.process_sender ~session sender
      )
    >>| function
    | Ok res -> res
    | Error exn ->
      let module Glog = Glog(struct let session = session end) in
      Glog.error !"Exception in `Cb.process_sender %{sexp:Session.t} %{sexp:Sender.t}`\n %{Exn}" session sender exn;
      `Reject Reply.Service_unavailable_421

  let process_recipient ~session ~sender recipient =
    Monitor.try_with (fun () ->
        Cb.process_recipient ~session ~sender recipient
      )
    >>| function
    | Ok res -> res
    | Error exn ->
      let module Glog = Glog(struct let session = session end) in
      Glog.error !"Exception in `Cb.process_recipient %{sexp:Session.t} %{sexp:Sender.t} %{sexp:Email_address.t}`\n %{Exn}" session sender recipient exn;
      `Reject Reply.Service_unavailable_421

  let process_envelope ~session envelope =
    Monitor.try_with (fun () ->
        Cb.process_envelope ~session envelope
      )
    >>| function
    | Ok res -> res
    | Error exn ->
      let module Glog = Glog(struct let session = session end) in
      Glog.error !"Exception in `Cb.process_envelope %{sexp:Session.t} %{sexp:Envelope.t}`\n %{Exn}" session envelope exn;
      `Reject Reply.Service_unavailable_421

  let rpcs = Cb.rpcs
end

let tcp_servers ~spool ~config (module Cb : Callbacks.S) =
  let module Cb = Protect_callbacks(Cb) in
  Deferred.List.map ~how:`Parallel (Config.ports config) ~f:(fun port ->
    let local = Host_and_port.create ~host:"*" ~port in
    Tcp.Server.create (Tcp.on_port port)
      ~max_connections:(Config.max_concurrent_receive_jobs_per_port config)
      (fun (`Inet (inet_in, port_in)) reader writer ->
         let remote =
           Host_and_port.create ~host:(Unix.Inet_addr.to_string inet_in) ~port:port_in
         in
         let send_envelope ~original_msg envelopes_with_next_hops =
           Spool.add spool ~original_msg envelopes_with_next_hops
           >>|? Envelope.Id.to_string
         in
         let quarantine ~reason ~original_msg messages =
           Spool.quarantine spool ~reason ~original_msg messages
         in
         let session = Session.create ~local ~remote () in
         Monitor.try_with (fun () ->
           start_session
             ~config
             ~reader ~writer ~send_envelope ~quarantine
             ~session
             (module Cb))
         >>= function
         | Ok () -> return ()
         | Error exn ->
           let module Glog = Glog(struct let session = session end) in
           Glog.error !"Exception in tcp server session %{sexp:Session.t}: %{Exn}"
             session exn;
           return ()))
;;

type t =
  { config  : Config.t;
    (* One server per port *)
    servers : Tcp.Server.inet list;
    spool   : Spool.t;
  }
;;

let start ~config (module Cb : Callbacks.S) =
  Spool.create ~config ()
  >>=? fun spool ->
  tcp_servers ~spool ~config (module Cb)
  >>| fun servers ->
  don't_wait_for (Rpc_server.start (config, spool) ~plugin_rpcs:Cb.rpcs);
  Ok { config; servers; spool }
;;

let config t = t.config
;;

let ports t = List.map ~f:Tcp.Server.listening_on t.servers

let close ?timeout t =
  Deferred.List.iter ~how:`Parallel t.servers ~f:Tcp.Server.close
  >>= fun () ->
  Deferred.List.iter ~how:`Parallel t.servers ~f:Tcp.Server.close_finished
  >>= fun () ->
  Spool.kill_and_flush ?timeout t.spool
  >>= function
  | `Finished -> Deferred.Or_error.ok_unit
  | `Timeout  -> Deferred.Or_error.error_string "Messages remaining in queue"

let read_bsmtp reader =
  Pipe.init (fun out ->
      start_session
        ~config:(Config.empty)
        ~reader
        ?writer:None
        ~write_reply:(fun reply ->
            Log.Global.debug !"Discarding reply: %{Reply}" reply;
            if Reply.is_ok reply then return ()
            else begin
              Log.Global.flushed ()
              >>= fun () ->
              Pipe.write out (Or_error.error_string (Reply.to_string reply))
            end)
        ~send_envelope:(fun ~original_msg:_ envelope_with_next_hop ->
            Deferred.List.iter envelope_with_next_hop ~f:(fun envelope_with_next_hop ->
                Pipe.write out (Ok (Envelope_with_next_hop.envelope envelope_with_next_hop)))
            >>= fun () ->
            return (Ok "pipped")
          )
        ~quarantine:(fun ~reason:_ ~original_msg:_ _ -> Deferred.Or_error.ok_unit)
        ~session:(Session.create
                    ~local:(Host_and_port.create ~host:"*pipe*" ~port:0)
                    ~remote:(Host_and_port.create ~host:"*pipe*" ~port:0)
                    ())
        (module Callbacks.Simple:Callbacks.S))
;;

let read_mbox reader =
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
    let email =
      Email.of_bigbuffer buffer
      |> Or_error.ok_exn
    in
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
  Pipe.init read_msgs
;;
