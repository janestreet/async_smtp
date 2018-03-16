open Core
open Async
open Async_smtp_types

type 'a smtp_flags
  =  ?tls:bool
  -> 'a

type 'a server_flags
  =  ?max_message_size:Byte_units.t
  -> ?malformed_emails:[ `Reject | `Wrap ]
  -> ?echo_delivery:bool
  -> ?server_log:[ Log.Level.t | `None ]
  -> ?plugin:(module Server.Plugin.S)
  -> ?plugin_log:[ Log.Level.t | `None ]
  -> 'a

type 'a client_flags
  =  ?credentials:Credentials.t
  -> ?client_greeting:string
  -> ?client_log:[ Log.Level.t | `None ]
  -> 'a

let stdout_log ~tag ~level () =
  match level with
  | `None ->
    Log.create
      ~level:`Error
      ~output:[]
      ~on_error:(`Call ignore)
  | #Log.Level.t as level ->
    let output =
      Log.Output.create
        ~flush:(fun () -> Deferred.unit)
        (fun msgs ->
           Queue.iter msgs ~f:(fun msg ->
             printf "%s " tag;
             Option.iter (Log.Message.level msg) ~f:(printf !"%{Log.Level} ");
             printf "%s" (Log.Message.message msg);
             if not (List.is_empty (Log.Message.tags msg)) then
               printf " --";
             List.iter (Log.Message.tags msg) ~f:(fun (k,v) ->
               printf " [%s:%s]" k v);
             printf "\n%!";
           );
           Deferred.unit)
    in
    Log.create ~level  ~output:[output] ~on_error:`Raise
;;

let with_stdout_log ~tag ~level (f:(log:Log.t -> 'a Deferred.t)) : 'a Deferred.t =
  let log = stdout_log ~tag ~level () in
  Monitor.protect (fun () -> f ~log)
    ~finally:(fun () -> Log.close log)
;;

let drain_and_closed r =
  Deferred.all_unit
    [ Monitor.try_with (fun () -> Reader.lines r |> Pipe.drain) >>| ignore
    ; Reader.close_finished r
    ]
;;

(* create a connected [Reader.t] and [Writer.t] pair.
   [echo] can be used to print a copy of every line passing through the pipe.
   We use [Unix.pipe] directly to avoid buffering issues. *)
let rec mk_pipe ?echo here desc =
  let%bind `Reader r, `Writer w =
    Unix.pipe (Info.create ~here "[Smtp_expect_test_helper]" desc sexp_of_string)
  in
  let r = Reader.create r in
  let w = Writer.create w in
  don't_wait_for (Reader.close_finished r >>= fun () -> Writer.close w);
  let%bind r = match echo with
    | None -> return r
    | Some echo ->
      let%bind r', w' = mk_pipe [%here] (sprintf "%s-echo" desc) in
      don't_wait_for (
        Pipe.iter
          (Reader.lines r)
          ~f:(fun l ->
            echo l;
            Writer.write_line w' l;
            Writer.flushed w')
        >>= fun () ->
        Writer.close w');
      return r'
  in
  return (r, w)
;;

let print_and_ignore_exn identity f =
  Monitor.try_with ~extract_exn:true f
  >>| function
  | Ok () -> ()
  | Error exn ->
    Ref.set_temporarily Backtrace.elide true ~f:(fun () ->
      printf !"%s_ERROR: %{sexp:Exn.t}\n" identity exn)
;;

let envelope
      ?(sender="<sender@example.com>")
      ?(recipients=["<recipient@example.com>"])
      ?(data="Subject: TEST EMAIL\n\nTEST EMAIL")
      () =
  let sender, sender_args =
    Smtp_envelope.Sender.of_string_with_arguments sender
      ~allowed_extensions:Smtp_extension.all
    |> Or_error.ok_exn
  in
  let recipients = List.map recipients ~f:(fun s -> Email_message.Email_address.of_string_exn s) in
  let email = Email_message.Email.of_string data in
  Smtp_envelope.create ~sender ~sender_args ~recipients ~email ()
;;

let run
      ~echo
      ~client
      ~server =
  let%bind server_r, client_w =
    mk_pipe ?echo:(Option.some_if echo (printf "> %s\n"))
      [%here] "client->server"
  and client_r, server_w =
    mk_pipe ?echo:(Option.some_if echo (printf "< %s\n"))
      [%here] "server->client"
  in
  let shutdown_now () =
    Deferred.all_unit
      [ drain_and_closed client_r
      ; Writer.close client_w
      ; drain_and_closed server_r
      ; Writer.close server_w
      ]
  in
  (* Hack to ensure the tests terminate in case of protocol errors *)
  let shutdown_later () =
    don't_wait_for begin
      Clock.after (Time.Span.of_sec 1.)
      >>= shutdown_now
    end
  in
  Monitor.protect
    ~finally:shutdown_now
    (fun () ->
       Deferred.all_unit
         [ print_and_ignore_exn "client"
             (fun () ->
                client client_r client_w)
           >>| shutdown_later
         ; print_and_ignore_exn "server"
             (fun () -> server server_r server_w)
           >>| shutdown_later
         ]
    )
;;

module Default_plugin : Server.Plugin.S = struct
  let rpcs () = []

  module Session = struct
    include Server.Plugin.Simple.Session
    let greeting _ = "[SMTP TEST SERVER]"
  end

  module Envelope = Server.Plugin.Simple.Envelope
end

module Safe_plugin(Info : sig
    val level : [ Log.Level.t | `None ]
    val tag : string
  end)(P : Server.Plugin.S) : Server.Plugin.S = struct
  let rpcs () = []

  let with_stdout_log f = with_stdout_log ~tag:Info.tag ~level:Info.level f

  module Session = struct
    module P = P.Session

    type t = P.t

    let connect ~log:_ ~local ~remote =
      with_stdout_log (fun ~log -> P.connect ~log ~local ~remote)
    let disconnect ~log:_ session =
      with_stdout_log (fun ~log -> P.disconnect ~log session)
    let helo ~log:_ session msg =
      with_stdout_log (fun ~log -> P.helo ~log session msg)

    let greeting t = P.greeting t

    let extensions session =
      List.map (P.extensions session) ~f:(function
        | Server.Plugin.Extension.Auth
            (module P : Server.Plugin.Auth with type session = t) ->
          Server.Plugin.Extension.Auth
            (module struct
              type session = t
              let mechanism = P.mechanism
              let negotiate ~log:_ session ~send_challenge_and_expect_response =
                with_stdout_log (fun ~log ->
                  P.negotiate ~log session ~send_challenge_and_expect_response)
            end : Server.Plugin.Auth with type session = t)
        | Server.Plugin.Extension.Start_tls
            (module P : Server.Plugin.Start_tls with type session = t) ->
          Server.Plugin.Extension.Start_tls
            (module struct
              type session = t
              let upgrade_to_tls ~log:_ session =
                with_stdout_log (fun ~log -> P.upgrade_to_tls ~log session)
            end : Server.Plugin.Start_tls with type session = t))
    ;;
  end

  module Envelope = struct
    module P = P.Envelope
    type t = P.t
    let smtp_envelope_info t = P.smtp_envelope_info t
    let mail_from ~log:_ session sender args =
      with_stdout_log (fun ~log -> P.mail_from ~log session sender args)
    let rcpt_to ~log:_ session envelope recipient =
      with_stdout_log (fun ~log -> P.rcpt_to ~log session envelope recipient)
    let accept_data ~log:_ session envelope =
      with_stdout_log (fun ~log -> P.accept_data ~log session envelope)
    let process ~log:_ session envelope email =
      with_stdout_log (fun ~log -> P.process ~log session envelope email)
  end
end

let server_impl
      ?(tls=false)
      ?max_message_size
      ?malformed_emails
      ?(echo_delivery=false)
      ?(server_log=`None)
      ?plugin
      ?(plugin_log=`Debug)
      r w =
  let (module Plugin : Server.Plugin.S) = match plugin with
    | None -> (module Default_plugin)
    | Some (module Plugin : Server.Plugin.S) ->
      (module Safe_plugin(struct
           let level = plugin_log
           let tag = "server.plugin"
         end)(Plugin) : Server.Plugin.S)
  in
  let module S = Server.For_test(Plugin) in
  with_stdout_log ~tag:"server" ~level:server_log (fun ~log ->
    let send_seq = ref 0 in
    S.session
      ~log
      ?max_message_size
      ?malformed_emails
      ~emulate_tls:tls
      ~send:(fun msgs ->
        if echo_delivery then
          List.iter msgs ~f:(printf !"SEND: %{sexp:Smtp_envelope.Routed.Batch.t}\n");
        send_seq := !send_seq + 1;
        Deferred.Or_error.return (sprintf "SENT-%d" !send_seq))
      ~quarantine:(fun ~reason msgs ->
        if echo_delivery then begin
          printf !"QUARANTINE_REASON: %{sexp:Quarantine_reason.t}\n" reason;
          List.iter msgs ~f:(printf !"QUARANTINE: %{sexp:Smtp_envelope.Routed.Batch.t}\n");
        end;
        Deferred.Or_error.ok_unit)
      ~local:(`Inet (Host_and_port.create ~host:"server" ~port:0))
      ~remote:(`Inet (Host_and_port.create ~host:"client" ~port:0))
      r w)
;;

let client_impl
      ?(tls=false)
      ?credentials
      ?(client_greeting="[SMTP TEST CLIENT]")
      ?(client_log=`None)
      ?(envelopes=[])
      r w =
  with_stdout_log ~tag:"client" ~level:client_log (fun ~log ->
    Client.For_test.with_
      ~config:{ Client_config.
                greeting             = Some client_greeting
              ; tls                  = []
              ; send_receive_timeout = `This (Time.Span.of_sec 1.)
              ; final_ok_timeout     = `This (Time.Span.of_sec 1.)
              }
      ?credentials
      ~log
      ~emulate_tls:tls
      ~dest:(`Inet (Host_and_port.create ~host:"server" ~port:0))
      r
      w
      ~f:(fun client ->
        Deferred.List.iter ~how:`Sequential envelopes ~f:(fun envelope ->
          Client.send_envelope client ~log envelope
          >>| ignore)
        >>| Or_error.return)
    >>| function
    | Ok () -> ()
    | Error err ->
      printf !"client_ERROR: %{Error#hum}\n" err)
;;

let gen_impl ~f r w =
  let send msg =
    Writer.write_line w msg;
    Clock.with_timeout (Time.Span.of_sec 1.0) (Writer.flushed w)
    >>| function
    | `Timeout ->
      failwith "Failed to flush in time, maybe you should read something first"
    | `Result () ->
      ()
  in
  let receive msg =
    Clock.with_timeout (Time.Span.of_sec 1.0) (Reader.read_line r)
    >>| function
    | `Timeout ->
      failwithf !"Expected %{sexp:string} but got timeout, \
                  maybe you need to send something first?" msg ()
    | `Result `Eof ->
      failwithf !"Expected %{sexp:string} but reached EOF, \
                  maybe you're expecting too much?" msg ()
    | `Result (`Ok line) ->
      if String.equal msg line then ()
      else
        failwithf !"Expected %{sexp:string} but got %{sexp:string}, \
                    check your protocol and expectations." msg line ()
  in
  let send msg =
    String.split_lines msg
    |> Deferred.List.iter ~f:send ~how:`Sequential
  in
  let receive msg =
    String.split_lines msg
    |> Deferred.List.iter ~f:receive ~how:`Sequential
  in
  f ~send ~receive
;;

let smtp
      ?tls
      ?max_message_size
      ?malformed_emails
      ?echo_delivery
      ?server_log
      ?plugin
      ?plugin_log
      ?credentials
      ?client_greeting
      ?client_log
      envelopes =
  run
    ~echo:true
    ~server:(server_impl
               ?tls
               ?max_message_size
               ?malformed_emails
               ?echo_delivery
               ?server_log
               ?plugin
               ?plugin_log)
    ~client:(client_impl
               ?tls
               ?credentials
               ?client_greeting
               ?client_log
               ~envelopes)
;;

let manual_client
      ?tls
      ?max_message_size
      ?malformed_emails
      ?echo_delivery
      ?server_log
      ?plugin
      ?plugin_log
      f =
  run
    ~echo:false
    ~server:(server_impl
               ?tls
               ?max_message_size
               ?malformed_emails
               ?echo_delivery
               ?server_log
               ?plugin
               ?plugin_log)
    ~client:(gen_impl ~f:(fun ~send ~receive -> f ~client:send ~server:receive))
;;

let manual_server
      ?tls
      ?credentials
      ?client_greeting
      ?client_log
      envelopes
      f =
  run
    ~echo:false
    ~server:(gen_impl ~f:(fun ~send ~receive -> f ~client:receive ~server:send))
    ~client:(client_impl
               ?tls
               ?credentials
               ?client_greeting
               ?client_log
               ~envelopes)
;;
