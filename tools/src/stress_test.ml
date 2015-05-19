open Core.Std
open Async.Std
open Async_smtp.Std

module Config = struct
  type t =
    { sending_port : int
    ; listening_ports : int list
    ; send_n_messages : int
    } with fields, sexp

  let server_config t =
    { Smtp_config.
      spool_dir = "/tmp/stress-test-spool-unused"
    ; tmp_dir = None
    ; ports = t.listening_ports
    ; max_concurrent_send_jobs = 1 (* not used *)
    ; max_concurrent_receive_jobs_per_port = 1
    ; rpc_port = 1 (* not used *)
    ; malformed_emails = `Reject
    ; max_message_size = Byte_units.create `Megabytes 1.
    }

  let load_exn file =
    Reader.load_sexp file t_of_sexp
    >>| Or_error.ok_exn
end

let counter = ref 0
let finished = Ivar.create ()

let send ~config envelope =
  incr counter;
  Printf.printf "%d\n%!" !counter;
  let port = Config.sending_port config in
  don't_wait_for (
    Smtp_client.send (Host_and_port.create ~host:"localhost" ~port) envelope
    |> Smtp_client.Smtp_result.ok_exn)

let main ~config () =
  Config.load_exn config
  >>= fun config ->
  let stdin = Lazy.force Reader.stdin in
  Smtp_server.read_bsmtp stdin
  |> Pipe.map ~f:Or_error.ok_exn
  |> Pipe.to_list
  >>= begin function
  | [e] -> return e
  | _ -> failwith "Input must contain a single message."
  end
  >>= fun envelope ->
  let server_config = Config.server_config config in
  let module Callbacks : Smtp_server.Callbacks.S = struct
    include Smtp_server.Callbacks.Simple
    let process_envelope ~session:_ envelope =
      begin
        if !counter = Config.send_n_messages config
        then Ivar.fill finished ()
        else send ~config envelope
      end;
      return (`Consume "")
  end in
  Smtp_server.start ~config:server_config (module Callbacks)
  >>= fun server ->
  let server = Or_error.ok_exn server in
  send ~config envelope;
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
      ++ step (fun m config -> m ~config)
      +> flag "-config" (required string) ~doc:"CONFIG"
    )
    main
;;
