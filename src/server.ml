open Core.Std
open Async.Std
open Async_ssl.Std
open Types
open Email_message.Std

module Config = Server_config

module Log = Mail_log


module Callbacks = Server_callbacks

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


let rec start_session
    ~log
    ~config
    ~reader
    ~server_events
    ?(is_protocol_upgrade=false)
    ?writer
    ?write_reply
    ~send_envelope
    ~quarantine
    ~session
    ~session_flows
    (module Cb : Callbacks.S) =
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
  let write_reply ~here ~flows ~component reply =
    Log.debug log (lazy (Log.Message.create
                           ~here
                           ~flows
                           ~component
                           ~reply
                           "->"));
    write_reply reply
  in
  let bad_sequence_of_commands ~here ~flows ~component cmd =
    let msg = Command.to_string cmd in
    write_reply ~here ~flows ~component (Reply.Bad_sequence_of_commands_503 msg)
  in
  let closing_connection ~here ~flows ~component () =
    write_reply ~here ~flows ~component Reply.Closing_connection_221
  in
  let command_not_implemented ~here ~flows ~component cmd =
    let msg = Command.to_string cmd in
    write_reply ~here ~flows ~component (Reply.Command_not_implemented_502 msg)
  in
  let command_not_recognized ~here ~flows ~component msg =
    write_reply ~here ~flows ~component (Reply.Command_not_recognized_500 msg)
  in
  let ok_completed ~here ~flows ~component ?(extra=[]) msg =
    let msg = String.concat ~sep:"\n" (msg :: extra) in
    write_reply ~here ~flows ~component (Reply.Ok_completed_250 msg)
  in
  let ok_continue ~here ~flows ~component () =
    ok_completed ~here ~flows ~component "continue"
  in
  let service_ready ~here ~flows ~component msg =
    write_reply ~here ~flows ~component (Reply.Service_ready_220 msg)
  in
  let service_unavailable ~here ~flows ~component () =
    write_reply ~here ~flows ~component Reply.Service_unavailable_421
  in
  let start_mail_input ~here ~flows ~component () =
    write_reply ~here ~flows ~component Reply.Start_mail_input_354
  in
  let syntax_error ~here ~flows ~component msg =
    write_reply ~here ~flows ~component (Reply.Syntax_error_501 msg)
  in
  let transaction_failed ~here ~flows ~component msg =
    write_reply ~here ~flows ~component (Reply.Transaction_failed_554 msg)
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
        Log.debug log (lazy (Log.Message.create
                               ~here:[%here]
                               ~flows
                               ~component
                               "Got Eof on reader"));
        return ()
      | `Ok input ->
        match Option.try_with (fun () -> Command.of_string input) with
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
        | Command.Quit ->
          closing_connection ~here:[%here] ~flows ~component ()
        | Command.Noop ->
          ok_continue ~here:[%here] ~flows ~component () >>= fun () -> top ~session
        | Command.Reset ->
          ok_continue ~here:[%here] ~flows ~component () >>= fun () -> top ~session
        | Command.Hello helo ->
          top_helo ~extended:false ~flows ~session helo
        | Command.Extended_hello helo ->
          top_helo ~extended:true ~flows ~session helo
        | Command.Start_tls -> begin
            match config.Config.tls_options with
            | None ->
              command_not_implemented ~here:[%here] ~flows ~component Command.Start_tls >>= fun () -> top ~session
            | Some tls_options ->
              top_start_tls ~session ~flows tls_options
          end
        | Command.Sender sender ->
          top_envelope ~flows ~session sender
        | (Command.Recipient _ | Command.Data) as cmd ->
          bad_sequence_of_commands ~here:[%here] ~flows ~component cmd >>= fun () -> top ~session
        | (Command.Help) as cmd ->
          command_not_implemented ~here:[%here] ~flows ~component cmd >>= fun () -> top ~session)
  and top_helo ~flows ~extended ~session helo =
    let component = ["smtp-server"; "session"; "helo"] in
    Deferred.Or_error.try_with (fun () ->
        Cb.session_helo ~session helo
          ~log:(Log.with_flow_and_component log
                  ~flows:session_flows
                  ~component:(component @ ["plugin"; "session_helo"])))
    >>= function
    | Ok `Continue ->
      let extensions =
        (match config.Config.tls_options, Session.tls session with
         | None, _
         | Some _,  Some _ -> []
         | Some _, None -> [ "STARTTLS" ])
        @
        [ "8BITMIME" ]
      in
      let greeting, extra =
        if extended then
          "Continue, extensions follow:", extensions
        else
          "Continue, extensions follow: " ^ String.concat ~sep:", " extensions, []
      in
      ok_completed ~here:[%here] ~flows ~component ~extra greeting
      >>= fun () ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            "accepted"));
      top ~session
    | Ok (`Deny reply) ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            ~reply
                            "deny"));
      write_reply ~here:[%here] ~flows ~component reply
      >>= fun () ->
      top ~session
    | Ok (`Disconnect None) ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            "disconnect"));
      return ()
    | Ok (`Disconnect (Some reply)) ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            ~reply
                            "disconnect"));
      write_reply ~here:[%here] ~flows ~component reply
    | Error err ->
      Log.error log (lazy (Log.Message.of_error
                             ~here:[%here]
                             ~flows
                             ~component:(component @ ["plugin"; "session_helo"])
                             err));
      service_unavailable ~here:[%here] ~flows ~component ()
  and top_start_tls ~flows ~session tls_options =
    let component = ["smtp-server"; "session"; "starttls"] in
    service_ready ~here:[%here] ~flows ~component "Begin TLS transition"
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
      Log.error log (lazy (Log.Message.of_error
                             ~here:[%here]
                             ~flows
                             ~component e));
      return ()
    | Ok tls ->
      let session =
        Session.create
          ~local:session.Session.local
          ~remote:session.Session.remote
          ~tls
          ()
      in
      Reader.of_pipe (Info.of_string "SMTP/TLS") reader_pipe_r
      >>= fun new_reader ->
      Writer.of_pipe (Info.of_string "SMTP/TLS") writer_pipe_w
      >>= fun (new_writer, _) ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            "upgraded to tls"));
      Deferred.all_ignore
        [ begin
          Ssl.Connection.closed tls
          >>= fun result ->
          Result.iter_error result ~f:(fun e ->
              Log.error log (lazy (Log.Message.of_error
                                     ~here:[%here]
                                     ~flows
                                     ~component e)));
          Deferred.all_unit
            [ Reader.close new_reader
            ; Writer.flushed new_writer
              >>= fun () ->
              Writer.close new_writer
            ]
        end
        ; begin
          start_session
            ~log:log
            ~config
            ~server_events
            ~reader:new_reader
            ~writer:new_writer
            ~send_envelope
            ~quarantine
            ~session
            ~is_protocol_upgrade:true
            ~session_flows
            (module Cb : Callbacks.S)
        end ]
  and top_envelope ~flows ~session sender_str =
    let flows = Log.Flows.extend flows `Inbound_envelope in
    let component = ["smtp-server"; "session"; "envelope"; "sender"] in
    match Sender.of_string sender_str with
    | Error _err ->
      syntax_error ~here:[%here] ~flows ~component (sprintf "Cannot parse '%s'" sender_str)
      >>= fun () ->
      top ~session
    | Ok sender ->
      Deferred.Or_error.try_with (fun () ->
          Cb.process_sender ~session sender
            ~log:(Log.with_flow_and_component log
                    ~flows
                    ~component:(component @ ["plugin"; "process_sender"])))
      >>= function
      | Ok (`Reject reject) ->
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~sender:(`String sender_str)
                              ~reply:reject
                              "REJECTED"));
        write_reply ~here:[%here] ~flows ~component reject
        >>= fun () ->
        top ~session
      | Error err ->
        Log.info log (lazy (Log.Message.of_error
                              ~here:[%here]
                              ~flows
                              ~component:(component @ ["plugin"; "process_sender"])
                              ~sender:(`String sender_str)
                              err));
        service_unavailable ~here:[%here] ~flows ~component ()
        >>= fun () ->
        top ~session
      | Ok `Continue ->
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~sender:(`String sender_str)
                              "MAIL FROM"));
        ok_continue ~here:[%here] ~flows ~component ()
        >>= fun () ->
        envelope ~session ~flows ~sender ~recipients:[] ~rejected_recipients:[]
  and envelope ~session ~flows ~sender ~recipients ~rejected_recipients =
    let component = ["smtp-server"; "session"; "envelope"; "top"] in
    loop ~flows ~component ~next:(function
        | Command.Quit ->
          closing_connection ~here:[%here] ~flows ~component ()
        | Command.Noop ->
          ok_continue ~here:[%here] ~flows ~component ()
          >>= fun () ->
          envelope ~session ~flows ~sender ~recipients ~rejected_recipients
        | Command.Recipient recipient ->
          envelope_recipient ~session ~flows ~sender ~recipients ~rejected_recipients recipient
        | Command.Data when not (List.is_empty recipients) ->
          envelope_data ~session ~flows ~sender ~recipients ~rejected_recipients
        | ( Command.Hello _ | Command.Extended_hello _
          | Command.Sender _ | Command.Data
          | Command.Start_tls
          ) as cmd ->
          bad_sequence_of_commands ~here:[%here] ~flows ~component cmd
          >>= fun () -> envelope ~session ~flows ~sender ~recipients ~rejected_recipients
        | Command.Reset ->
          ok_continue ~here:[%here] ~flows ~component () >>= fun () -> top ~session
        | (Command.Help) as cmd ->
          command_not_implemented ~here:[%here] ~flows ~component cmd
          >>= fun () -> envelope ~session ~flows ~sender ~recipients ~rejected_recipients)
  and envelope_recipient ~session ~flows ~sender ~recipients ~rejected_recipients recipient =
    let component = ["smtp-server"; "session"; "envelope"; "recipient"] in
    match Email_address.of_string recipient with
    | Error _ ->
      syntax_error ~here:[%here] ~flows ~component (sprintf "Cannot parse %s" recipient)
      >>= fun () ->
      envelope ~session ~flows ~sender ~recipients ~rejected_recipients
    | Ok recipient ->
      Deferred.Or_error.try_with (fun () ->
          Cb.process_recipient ~session ~sender recipient
            ~log:(Log.with_flow_and_component log
                    ~flows
                    ~component:(component @ ["plugin"; "process_recipient"])))
      >>= function
      | Ok (`Reject reject) ->
        let rejected_recipients = rejected_recipients @ [recipient] in
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~recipients:[`Email recipient]
                              ~reply:reject
                              "REJECTED"));
        write_reply ~here:[%here] ~flows ~component reject
        >>= fun () ->
        envelope ~session ~flows ~sender ~recipients ~rejected_recipients
      | Error err ->
        let rejected_recipients = rejected_recipients @ [recipient] in
        Log.error log (lazy (Log.Message.of_error
                               ~here:[%here]
                               ~flows
                               ~component:(component @ ["plugin"; "process_recipient"])
                               ~recipients:[`Email recipient]
                               err));
        service_unavailable ~here:[%here] ~flows ~component ()
        >>= fun () ->
        envelope ~session ~flows ~sender ~recipients ~rejected_recipients
      | Ok `Continue ->
        let recipients = recipients @ [recipient] in
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~recipients:[`Email recipient]
                              "RCPT TO"));
        ok_continue ~here:[%here] ~flows ~component ()
        >>= fun () ->
        envelope ~session ~flows ~sender ~recipients ~rejected_recipients
  and envelope_data ~session ~flows ~sender ~recipients ~rejected_recipients =
    let component = ["smtp-server"; "session"; "envelope"; "data"] in
    start_mail_input ~here:[%here] ~flows ~component ()
    >>= fun () ->
    read_data ~max_size:config.Config.max_message_size reader
    >>= function
    | `Too_much_data ->
      write_reply ~here:[%here] ~flows ~component Reply.Exceeded_storage_allocation_552
      >>= fun () ->
      top ~session
    | `Ok data ->
      Log.debug log (lazy (Log.Message.create
                             ~here:[%here]
                             ~flows
                             ~component
                             ~tags:["data", Bigbuffer.contents data]
                             "DATA"));
      let email =
        match Email.of_bigbuffer data with
        | Ok email -> Ok email
        | Error error ->
          Log.error log (lazy (Log.Message.of_error
                                 ~here:[%here]
                                 ~flows
                                 ~component
                                 error));
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
        write_reply ~here:[%here] ~flows ~component error
        >>= fun () ->
        top ~session
      | Ok email ->
        let original_msg =
          Envelope.create ~sender ~recipients ~rejected_recipients ~email ()
        in
        Smtp_events.envelope_received server_events original_msg;
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component
                              ~email:(`Envelope original_msg)
                              "DATA"));
        let component = ["smtp-server"; "session"; "envelope"; "routing"] in
        Deferred.Or_error.try_with (fun () ->
            Cb.process_envelope ~session original_msg
              ~log:(Log.with_flow_and_component log
                      ~flows
                      ~component:(component @ ["plugin"; "process_envelope"])))
        >>= function
        | Ok (`Reject reject) ->
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                ~reply:reject
                                "REJECTED"));
          write_reply ~here:[%here] ~flows ~component reject
          >>= fun () ->
          top ~session
        | Error err ->
          Log.error log (lazy (Log.Message.of_error
                                 ~here:[%here]
                                 ~flows
                                 ~component:(component @ ["plugin"; "process_envelope"])
                                 err));
          service_unavailable ~here:[%here] ~flows ~component ()
          >>= fun () ->
          top ~session
        | Ok (`Consume ok) ->
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                ~tags:["consumed-id", ok]
                                "CONSUMED"));
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
                  Log.error log (lazy (Log.Message.of_error
                                         ~here:[%here]
                                         ~flows
                                         ~component
                                         ~email:(`Envelope (Envelope_with_next_hop.envelope envelope_with_next_hops))
                                         ~tags:(List.map (Envelope_with_next_hop.next_hop_choices envelope_with_next_hops)
                                                  ~f:(fun c -> "next-hop",Address.to_string c))
                                       err)));
              transaction_failed ~here:[%here] ~flows ~component "error spooling"
              >>= fun () ->
              top ~session
            | Ok () ->
              List.iter envelopes_with_next_hops ~f:(fun envelope_with_next_hops ->
                  Log.info log (lazy (Log.Message.create
                                        ~here:[%here]
                                        ~flows
                                        ~component
                                        ~email:(`Envelope (Envelope_with_next_hop.envelope envelope_with_next_hops))
                                        ~tags:(List.map (Envelope_with_next_hop.next_hop_choices envelope_with_next_hops)
                                                 ~f:(fun c -> "next-hop",Address.to_string c))
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
                Log.error log (lazy (Log.Message.of_error
                                       ~here:[%here]
                                       ~flows
                                       ~component
                                       ~email:(`Envelope (Envelope_with_next_hop.envelope envelope_with_next_hops))
                                       ~tags:(List.map (Envelope_with_next_hop.next_hop_choices envelope_with_next_hops)
                                                ~f:(fun c -> "next-hop",Address.to_string c))
                                       err)));
            transaction_failed ~here:[%here] ~flows ~component "error spooling"
            >>= fun () ->
            top ~session
          | Ok id ->
            List.iter envelopes_with_next_hops ~f:(fun envelope_with_next_hops ->
                Log.info log (lazy (Log.Message.create
                                      ~here:[%here]
                                      ~flows
                                      ~component
                                      ~spool_id:id
                                      ~tags:(List.map (Envelope_with_next_hop.next_hop_choices envelope_with_next_hops)
                                               ~f:(fun c -> "next-hop",Address.to_string c))
                                      "SPOOLED")));
            ok_completed ~here:[%here] ~flows ~component (sprintf "id=%s" id)
            >>= fun () ->
            top ~session
  in
  let component = [ "smtp-server"; "session"; "init" ] in
  let flows = session_flows in
  Log.info log (lazy (Log.Message.create
                        ~here:[%here]
                        ~flows
                        ~component
                        ~remote_address:session.Session.remote
                        ~local_address:session.Session.local
                        "CONNECTED"));
  if is_protocol_upgrade
  then top ~session
  else begin
    Deferred.Or_error.try_with (fun () ->
        Cb.session_connect ~session
          ~log:(Log.with_flow_and_component log
                  ~flows
                  ~component:(component @ ["plugin"; "session_connect"])))
    >>= function
    | Ok (`Disconnect None) ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            "disconnect"));
      return ()
    | Ok (`Disconnect (Some reply)) ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            ~reply
                            "disconnect"));
      write_reply ~here:[%here] ~flows ~component reply
    | Error err ->
      Log.info log (lazy (Log.Message.of_error
                            ~here:[%here]
                            ~flows
                            ~component
                            err));
      service_unavailable ~here:[%here] ~flows ~component ()
    | Ok (`Accept hello) ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            ~tags:["greeting",hello]
                            "accepted"));
      service_ready ~here:[%here] ~flows ~component hello
      >>= fun () ->
      top ~session
  end
;;

type server = Inet of Tcp.Server.inet | Unix of Tcp.Server.unix

type t =
  { config       : Config.t;
    (* One server per port *)
    servers      : server list;
    spool        : Spool.t
  }
;;

let tcp_servers ~spool ~config ~log ~server_events (module Cb : Callbacks.S) =
  let start_servers where ~make_local_address ~make_tcp_where_to_listen
    ~make_remote_address ~to_server =
    let local = make_local_address where in
    Tcp.Server.create (make_tcp_where_to_listen where)
      ~max_connections:(Config.max_concurrent_receive_jobs_per_port config)
      (fun address_in reader writer ->
         let session_flows = Log.Flows.create `Server_session in
         let remote = make_remote_address address_in in
         let send_envelope ~flows ~original_msg envelopes_with_next_hops =
           Spool.add spool ~flows ~original_msg envelopes_with_next_hops
           >>|? Envelope.Id.to_string
         in
         let quarantine ~flows ~reason ~original_msg messages =
           Spool.quarantine spool ~flows ~reason ~original_msg messages
         in
         let session = Session.create ~local ~remote () in
         Deferred.Or_error.try_with (fun () ->
           start_session
             ~log
             ~server_events
             ~config
             ~session_flows
             ~reader ~writer ~send_envelope ~quarantine
             ~session
             (module Cb))
         >>= function
         | Ok () -> return ()
         | Error err ->
           Log.error log (lazy (Log.Message.of_error
                                  ~here:[%here]
                                  ~flows:session_flows
                                  ~component:["smtp-server"; "tcp"]
                                  ~remote_address:session.Session.remote
                                  ~local_address:session.Session.local
                                  err));
           return ())
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

let start ~config ~log (module Cb : Callbacks.S) =
  let server_events = Smtp_events.create () in
  Spool.create ~config ~log ()
  >>=? fun spool ->
  tcp_servers ~spool ~config ~log ~server_events (module Cb)
  >>| fun servers ->
  don't_wait_for
    (Rpc_server.start (config, spool, server_events) ~log ~plugin_rpcs:Cb.rpcs);
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


let bsmtp_log = Lazy.map Async.Std.Log.Global.log ~f:(fun log ->
    log
    |> Log.adjust_log_levels ~remap_info_to:`Debug)

let read_bsmtp ?(log=Lazy.force bsmtp_log) reader =
  let server_events = Smtp_events.create () in
  Pipe.create_reader ~close_on_exception:true (fun out ->
      let session_flows = Log.Flows.create `Server_session in
      let module Cb : Callbacks.S = struct
        include Callbacks.Simple
        let process_envelope ~log:_ ~session:_ envelope =
          Pipe.write out (Ok envelope)
          >>| fun () ->
          `Consume "bsmtp"
      end in
      start_session
        ~log
        ~config:(Config.empty)
        ~reader
        ~server_events
        ~session_flows
        ?writer:None
        ~write_reply:(fun reply ->
            if Reply.is_ok reply then return ()
            else begin
              Pipe.write out (Or_error.error_string (Reply.to_string reply))
            end)
        ~send_envelope:(fun ~flows:_ ~original_msg:_ _ ->
            Deferred.Or_error.error_string "Not implemented")
        ~quarantine:(fun ~flows:_ ~reason:_ ~original_msg:_ _ ->
            Deferred.Or_error.error_string "Not implemented")
        ~session:(Session.create
                    ~local:(`Inet (Host_and_port.create ~host:"*pipe*" ~port:0))
                    ~remote:(`Inet (Host_and_port.create ~host:"*pipe*" ~port:0))
                    ())
        (module Cb))
;;

let mbox_log = Lazy.map Async.Std.Log.Global.log ~f:(fun log ->
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
  Pipe.create_reader ~close_on_exception:true read_msgs
;;
