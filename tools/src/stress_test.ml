open Core.Std
open Async.Std
open Async_smtp.Std

let log =
  Lazy.force Async.Std.Log.Global.log
  |> Async_smtp.Mail_log.adjust_log_levels ~remap_info_to:`Debug

module Config = struct
  type t =
    { dir : string
    ; host : string
    ; port : int
    ; tls : bool
    ; send_n_messages : int
    } [@@deriving fields, sexp]

  let make_tls_certificates t =
    Async_shell.run "openssl"
      [ "req"
      ; "-new"; "-x509" (* generate the request and sign in one step *)
      ; "-newkey"; "rsa:512" (* generate key *)
      ; "-nodes" (* don't encrypt the key *)
      ; "-batch" (* non interactive *)
      ; "-keyout"; t.dir ^/ "ca.key"
      ; "-out"; t.dir ^/ "ca.crt"
      ; "-days"; "1" (* short shelf life is good for testing *)
      ; "-subj"; "/CN=stress-test-CA/"
      ]
    >>= fun () ->
    Async_shell.run "openssl"
      [ "req"; "-new"
      ; "-newkey"; "rsa:512" (* generate key *)
      ; "-nodes" (* don't encrypt the key *)
      ; "-batch" (* non interactive *)
      ; "-keyout"; t.dir ^/ "server.key"
      ; "-out"; t.dir ^/ "server.csr"
      ; "-subj"; sprintf "/CN=%s/" t.host
      ]
    >>= fun () ->
    Async_shell.run "openssl"
      [ "x509"; "-req"
      ; "-days"; "1" (* short shelf life is good for testing *)
      ; "-CA"; t.dir ^/ "ca.crt"
      ; "-CAkey"; t.dir ^/ "ca.key"
      ; "-in"; t.dir ^/ "server.csr"
      ; "-out"; t.dir ^/ "server.crt"
      ; "-set_serial"; "1"
      ]
    >>| fun () ->
    let server =
      { Smtp_server.Config.Tls.
        version = None
      ; options = None
      ; name = None
      ; crt_file = t.dir ^/ "server.crt"
      ; key_file = t.dir ^/ "server.key"
      ; ca_file = Some (t.dir ^/ "ca.crt")
      ; ca_path = None
      }
    in
    let client =
      [ Smtp_client.Config.Domain_suffix.of_string ( t.host),
        { Smtp_client.Config.Tls.
          version = None
        ; options = None
        ; name = None
        ; ca_file = Some (t.dir ^/ "ca.crt")
        ; ca_path = None
        ; mode = `Required
        ; certificate_mode = `Verify
        }
      ; Smtp_client.Config.Domain_suffix.of_string "",
        { Smtp_client.Config.Tls.
          version = None
        ; options = None
        ; name = None
        ; ca_file = None
        ; ca_path = None
        ; mode = `Required
        ; certificate_mode = `Verify (* Causes the message to fail if we have to wrong host *)
        }
      ]
    in
    server,client

  let server_and_client_config t =
    begin if t.tls then begin
      make_tls_certificates t
      >>| fun tls_options -> Some tls_options
    end else begin
        return None
      end
    end
    >>= fun tls_options ->
    let spool_dir = t.dir ^/ "spool-not-used" in
    Deferred.all_ignore
      [ Unix.mkdir ~p:() spool_dir ]
    >>| fun () ->
    let client =
      { Smtp_client.Config.
        greeting = Some "stress-test"
      ; tls = Option.value_map ~f:snd tls_options ~default:[]
      ; send_receive_timeout = `This (Time.Span.of_sec 2.)
      ; final_ok_timeout = `This (Time.Span.of_sec 5.)
      }
    in
    let server =
      { Smtp_server.Config.
        spool_dir
      ; tmp_dir = None
      ; where_to_listen = [`Port t.port]
      ; max_concurrent_send_jobs = 1 (* not used *)
      ; max_concurrent_receive_jobs_per_port = 1
      ; rpc_port = 0 (* not used *)
      ; malformed_emails = `Reject
      ; max_message_size = Byte_units.create `Megabytes 1.
      ; tls_options = Option.map ~f:fst tls_options
      ; client
      }
    in
    server, client
end

let counter = ref 0
let finished = Ivar.create ()

let send ~config ~client_config envelope =
  incr counter;
  Core.Std.Printf.printf "%d\n%!" !counter;
  let port = Config.port config in
  let host = Config.host config in
  don't_wait_for (
    Smtp_client.Tcp.with_
      ~log:(Lazy.force Log.Global.log)
      (`Inet (Host_and_port.create ~host ~port))
      ~config:client_config
      ~f:(fun client ->
        Smtp_client.send_envelope client ~log envelope
        >>|? Smtp_client.Envelope_status.ok_or_error ~allow_rejected_recipients:false
        >>| Or_error.join
        >>|? ignore
      )
    >>| Or_error.ok_exn
  )

let main ?dir ~host ~port ~tls ~send_n_messages ~message_from_stdin () =
  begin match dir with
  | Some dir -> return dir
  | None -> Unix.mkdtemp "/tmp/stress-test-"
  end
  >>= fun dir ->
  let config = { Config.dir; host; port; tls; send_n_messages } in
  begin if message_from_stdin then begin
    let stdin = Lazy.force Reader.stdin in
    Smtp_server.read_bsmtp stdin
    |> Pipe.map ~f:Or_error.ok_exn
    |> Pipe.to_list
    >>| function
    | [e] -> e
    | _ -> failwith "Input must contain a single message."
  end else begin
      let recipients = [ Email_address.of_string_exn "test@example.com" ] in
      let email =
        Email.Simple.create ~from:(Email_address.local_address ()) ~subject:"Stress test" ~to_:recipients
          (Email.Simple.Content.text "Stress Test")
      in
      let sender = `Null in
      return (Smtp_envelope.create ~sender ~recipients ~email ())
    end
  end
  >>= fun envelope ->
  Config.server_and_client_config config
  >>= fun (server_config,client_config) ->
  let module Callbacks : Smtp_server.Callbacks.S = struct
    include Smtp_server.Callbacks.Simple
    let process_envelope ~log:_ ~session:_ envelope =
      begin
        if !counter = Config.send_n_messages config
        then Ivar.fill finished ()
        else send ~config ~client_config envelope
      end;
      return (`Consume (sprintf "stress-test:%d" !counter))
  end in
  Smtp_server.start ~log ~config:server_config (module Callbacks)
  >>| Or_error.ok_exn
  >>= fun server ->
  send ~config ~client_config envelope;
  Ivar.read finished
  >>= fun () ->
  (* Allow the last session to finish. This won't be necessary once Server.flush
     waits for incoming sessions to copmlete. *)
  Clock.after (sec 0.1)
  >>= fun () ->
  Smtp_server.close server
  >>= function
  | Error e -> Error.raise e
  | Ok () ->
    Deferred.return ()


let command =
  Command.async
    ~summary:("Stress-test an smtp server by repeatedly sending and receiving a message read from stdin")
    Command.Spec.(
      empty
      ++ step (fun m v -> m ?dir:v)
      +> flag "-dir" (optional string) ~doc:" Working dir"
      ++ step (fun m v -> m ~host:v)
      +> flag "-host" (optional_with_default "localhost" string) ~doc:" Hostname to listen on"
      ++ step (fun m v -> m ~port:v)
      +> flag "-port" (optional_with_default 2525 int) ~doc:" Port to listen on"
      ++ step (fun m v -> m ~tls:v)
      +> flag "-tls" no_arg ~doc:" Run the stress test with TLS enabled"
      ++ step (fun m v -> m ~send_n_messages:v)
      +> flag "-send-n-messages" ~aliases:["-n"] (optional_with_default 1000 int) ~doc:" Number of messages to send"
      ++ step (fun m v -> m ~message_from_stdin:v)
      +> flag "-message-from-stdin" no_arg ~doc:" Read the message from stdin, otherwise generate a simple message"
      ++ step (fun m v -> Option.iter ~f:Log.Global.set_level v; m)
      +> flag "-log-level" (optional Log.Level.arg) ~doc:" Log level"
    )
    main
;;
