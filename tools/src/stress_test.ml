open Core
open Async
open Async_smtp

let log =
  Lazy.force Async.Log.Global.log |> Smtp_mail_log.adjust_log_levels ~remap_info_to:`Debug
;;

module Config = struct
  type t =
    { dir : string
    ; host : string
    ; port : int
    ; tls : bool
    ; send_n_messages : int
    ; client_allowed_ciphers : [ `Secure | `Openssl_default | `Only of string list ]
    ; server_allowed_ciphers : [ `Secure | `Openssl_default | `Only of string list ]
    ; key_type : [ `rsa of int | `ecdsa of string | `dsa of int ]
    }
  [@@deriving fields ~getters, sexp]

  let make_tls_certificates t =
    let openssl args = Async_shell.run "openssl" args in
    let%bind () =
      openssl
        [ "req"
        ; "-new"
        ; "-x509" (* generate the request and sign in one step *)
        ; "-newkey"
        ; "rsa:512" (* generate key *)
        ; "-nodes" (* don't encrypt the key *)
        ; "-batch" (* non interactive *)
        ; "-keyout"
        ; t.dir ^/ "ca.key"
        ; "-out"
        ; t.dir ^/ "ca.crt"
        ; "-days"
        ; "1" (* short shelf life is good for testing *)
        ; "-subj"
        ; "/CN=stress-test-CA/"
        ]
    in
    let%bind () =
      match t.key_type with
      | `rsa bits ->
        openssl [ "genrsa"; "-out"; t.dir ^/ "server.key"; Int.to_string bits ]
      | `dsa bits ->
        openssl
          [ "dsaparam"; "-genkey"; "-out"; t.dir ^/ "server.key"; Int.to_string bits ]
      | `ecdsa curve ->
        openssl [ "ecparam"; "-name"; curve; "-genkey"; "-out"; t.dir ^/ "server.key" ]
    in
    let%bind () =
      openssl
        [ "req"
        ; "-new"
        ; "-key"
        ; t.dir ^/ "server.key"
        ; "-nodes" (* don't encrypt the key *)
        ; "-batch" (* non interactive *)
        ; "-out"
        ; t.dir ^/ "server.csr"
        ; "-subj"
        ; sprintf "/CN=%s/" t.host
        ]
    in
    let%map () =
      openssl
        [ "x509"
        ; "-req"
        ; "-days"
        ; "1" (* short shelf life is good for testing *)
        ; "-CA"
        ; t.dir ^/ "ca.crt"
        ; "-CAkey"
        ; t.dir ^/ "ca.key"
        ; "-in"
        ; t.dir ^/ "server.csr"
        ; "-out"
        ; t.dir ^/ "server.crt"
        ; "-set_serial"
        ; "1"
        ]
    in
    let server =
      { Smtp_server.Config.Tls_options.version = None
      ; options = None
      ; name = None
      ; crt_file = t.dir ^/ "server.crt"
      ; key_file = t.dir ^/ "server.key"
      ; ca_file = Some (t.dir ^/ "ca.crt")
      ; ca_path = None
      ; allowed_ciphers = t.server_allowed_ciphers
      }
    in
    let client =
      [ ( Smtp_client.Config.Domain_suffix.of_string t.host
        , { Smtp_client.Config.Tls.version = None
          ; options = None
          ; name = None
          ; ca_file = Some (t.dir ^/ "ca.crt")
          ; ca_path = None
          ; mode = `Required
          ; certificate_mode = `Verify
          ; allowed_ciphers = t.client_allowed_ciphers
          } )
      ; ( Smtp_client.Config.Domain_suffix.of_string ""
        , { Smtp_client.Config.Tls.version = None
          ; options = None
          ; name = None
          ; ca_file = None
          ; ca_path = None
          ; mode = `Required
          ; certificate_mode =
              `Verify (* Causes the message to fail if we have to wrong host *)
          ; allowed_ciphers = t.client_allowed_ciphers
          } )
      ]
    in
    server, client
  ;;

  let server_and_client_config ~concurrent_receivers t =
    let%bind tls_options =
      if t.tls
      then (
        let%map tls_options = make_tls_certificates t in
        Some tls_options)
      else return None
    in
    let spool_dir = t.dir ^/ "spool-not-used" in
    let%map () = Deferred.all_unit [ Unix.mkdir ~p:() spool_dir ] in
    let client =
      { Smtp_client.Config.greeting = Some "stress-test"
      ; tls = Option.value_map ~f:snd tls_options ~default:[]
      ; send_receive_timeout = `This (Time_float.Span.of_sec 2.)
      ; final_ok_timeout = `This (Time_float.Span.of_sec 5.)
      }
    in
    let server =
      { Smtp_server.Config.where_to_listen = [ Localhost_on_port t.port ]
      ; max_concurrent_receive_jobs_per_port = concurrent_receivers
      ; timeouts = Smtp_server.Config.Timeouts.default
      ; rpc_port = 0 (* not used *)
      ; rpc_heartbeat_config = None
      ; malformed_emails = `Reject
      ; max_message_size = Byte_units.of_megabytes 1.
      ; tls_options = Option.map ~f:fst tls_options
      ; tcp_options = None
      }
    in
    server, client
  ;;
end

let counter = ref 0
let finished = Ivar.create ()
let throttle = ref (Throttle.create ~continue_on_error:true ~max_concurrent_jobs:1)

let send ~config ~client_config envelope =
  incr counter;
  let port = Config.port config in
  let host = Config.host config in
  don't_wait_for
    (Throttle.enqueue !throttle (fun () ->
       Deferred.Or_error.try_with_join ~run:`Schedule ~rest:`Log (fun () ->
         Smtp_client.Tcp.with_
           ~log:(Lazy.force Log.Global.log)
           (Host_and_port.create ~host ~port)
           ~config:client_config
           ~f:(fun client ->
             Smtp_client.send_envelope client ~log envelope
             >>|? Smtp_client.Envelope_status.ok_or_error ~allow_rejected_recipients:false
             >>| Or_error.join
             >>|? ignore)))
     >>| Result.iter_error ~f:(fun log_arg ->
       [%log.error_format !"buh???: %{Error#hum}" log_arg]))
;;

let main
  ~dir
  ~host
  ~port
  ~tls
  ~send_n_messages
  ~num_copies
  ~concurrent_senders
  ~concurrent_receivers
  ~message_from_stdin
  ~client_allowed_ciphers
  ~server_allowed_ciphers
  ~key_type
  ()
  =
  let config =
    { Config.dir
    ; host
    ; port
    ; tls
    ; send_n_messages
    ; client_allowed_ciphers
    ; server_allowed_ciphers
    ; key_type
    }
  in
  let%bind envelopes =
    if message_from_stdin
    then (
      let stdin = Lazy.force Reader.stdin in
      Smtp_server.read_bsmtp stdin |> Pipe.map ~f:Or_error.ok_exn |> Pipe.to_list)
    else (
      let recipients = [ Email_address.of_string_exn "test@example.com" ] in
      let email =
        Email.Simple.create
          ~from:(Async_smtp.Simplemail.local_address ())
          ~subject:"Stress test"
          ~to_:recipients
          (Email.Simple.Content.text_utf8 "Stress Test")
      in
      let sender = `Null in
      return [ Smtp_envelope.create ~sender ~recipients ~email () ])
  in
  let envelopes = List.init num_copies ~f:(fun _ -> envelopes) |> List.concat in
  throttle
  := Throttle.create ~continue_on_error:true ~max_concurrent_jobs:concurrent_senders;
  let%bind server_config, client_config =
    Config.server_and_client_config ~concurrent_receivers config
  in
  let module Server =
    Smtp_server.Make (struct
      open Smtp_monad.Let_syntax
      module State = Smtp_server.Plugin.Simple.State

      module Session = struct
        include Smtp_server.Plugin.Simple.Session

        let extensions ~state:_ _ =
          [ Smtp_server.Plugin.Extension.Start_tls
              (module struct
                type session = t

                let upgrade_to_tls ~log:_ t = return { t with tls = true }
              end : Smtp_server.Plugin.Start_tls
                with type session = t)
          ]
        ;;
      end

      module Envelope = struct
        include Smtp_server.Plugin.Simple.Envelope

        let process ~state:_ ~log:_ ~flows:_ _session t email =
          let envelope = smtp_envelope t email in
          if !counter >= Config.send_n_messages config
          then Ivar.fill_if_empty finished ()
          else send ~config ~client_config envelope;
          return (sprintf "stress-test:%d" !counter)
        ;;
      end

      let rpcs () = []
    end)
  in
  let%bind server =
    Server.start ~server_state:() ~log ~config:server_config >>| Or_error.ok_exn
  in
  List.iter envelopes ~f:(send ~config ~client_config);
  let%bind () = Ivar.read finished in
  (* Wait for all pending messages to clear *)
  let%bind () = Throttle.prior_jobs_done !throttle in
  let%bind () = Clock.after (sec 0.1) in
  match%bind Server.close server with
  | Error e -> Error.raise e
  | Ok () -> Deferred.return ()
;;

let cipher_list = Command.Arg_type.create (fun s -> `Only (String.split ~on:':' s))

let key_type =
  Command.Arg_type.create (fun s ->
    match String.split ~on:':' s with
    | [ "rsa"; bits ] -> `rsa (Int.of_string bits)
    | [ "dsa"; bits ] -> `dsa (Int.of_string bits)
    | [ "ecdsa"; curve ] -> `ecdsa curve
    | _ -> failwith "not a recognized key type. Supported rsa:BITS, dsa:BITS, ecdsa:CURVE")
;;

let command =
  let open Command.Let_syntax in
  Command.async
    ~summary:
      "Stress-test an smtp server by repeatedly sending and receiving a message read \
       from stdin"
    [%map_open
      let dir =
        flag "-dir" (optional string) ~doc:" Working dir"
        |> map ~f:(function
          | Some dir -> dir
          | None -> Core_unix.mkdtemp "/tmp/stress-test-")
      and host =
        flag
          "-host"
          (optional_with_default "localhost" string)
          ~doc:" Hostname to listen on"
      and port = flag "-port" (optional_with_default 2525 int) ~doc:" Port to listen on"
      and tls = flag "-tls" no_arg ~doc:" Run the stress test with TLS enabled"
      and send_n_messages =
        flag
          "-send-n-messages"
          ~aliases:[ "-n" ]
          (optional_with_default 1000 int)
          ~doc:" Number of messages to send"
      and num_copies =
        flag
          "-num-copies"
          (optional_with_default 1 int)
          ~doc:" Number of copies of each (the) message to have in circulation"
      and concurrent_senders =
        flag
          "-concurrent-senders"
          (optional_with_default 1 int)
          ~doc:" Number of concurrent senders"
      and concurrent_receivers =
        flag
          "-concurrent-receivers"
          (optional_with_default 1 int)
          ~doc:" Number of concurrent receivers"
      and message_from_stdin =
        flag
          "-message-from-stdin"
          no_arg
          ~doc:" Read the message from stdin, otherwise generate a simple message"
      and () =
        flag "-log-level" (optional Log.Level.arg) ~doc:" Log level"
        |> map ~f:(Option.iter ~f:Log.Global.set_level)
      and client_allowed_ciphers =
        flag
          "-client-allowed-ciphers"
          (optional_with_default `Secure cipher_list)
          ~doc:" Restrict client side SSL ciphers"
      and server_allowed_ciphers =
        flag
          "-server-allowed-ciphers"
          (optional_with_default `Secure cipher_list)
          ~doc:" Restrict server side SSL ciphers"
      and key_type =
        flag
          "-key-type"
          (optional_with_default (`rsa 2048) key_type)
          ~doc:" TLS Key type to use/generate"
      in
      fun () ->
        main
          ~dir
          ~host
          ~port
          ~tls
          ~send_n_messages
          ~num_copies
          ~concurrent_senders
          ~concurrent_receivers
          ~message_from_stdin
          ~client_allowed_ciphers
          ~server_allowed_ciphers
          ~key_type
          ()]
    ~behave_nicely_in_pipeline:false
;;
