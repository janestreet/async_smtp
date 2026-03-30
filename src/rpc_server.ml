open Core
open Async

let implementations smtp_events =
  [ List.map
      Rpc_impl.Smtp_events.rpcs
      ~f:(Rpc.Implementation.lift ~f:(fun () -> smtp_events))
  ; Rpc_impl.Gc.rpcs
  ; Rpc_impl.Monitor.rpcs ()
  ; Rpc_impl.Process.rpcs
  ]
  |> List.concat
;;

let where_to_listen config =
  let rpc_port = Server_config.rpc_port config in
  if Server_config.rpc_listen_on_all_interfaces config
  then Tcp.Where_to_listen.of_port rpc_port
  else Tcp.Where_to_listen.bind_to Localhost (On_port rpc_port)
;;

let create_implementations implementations =
  Rpc.Implementations.create_exn
    ~implementations
    ~on_unknown_rpc:`Raise
    ~on_exception:Log_on_background_exn
;;

let log_listening ~log ~config =
  [%log.t.info_format log "RPC server listening on %d" (Server_config.rpc_port config)]
;;

let default_start_rpc_server ?where_to_listen_override ~config ~log ~implementations () =
  let where_to_listen =
    Option.value where_to_listen_override ~default:(where_to_listen config)
  in
  let heartbeat_config = Server_config.rpc_heartbeat_config config in
  let implementations = create_implementations implementations in
  [%bind
    let _tcp_server =
      Rpc.Connection.serve
        ~implementations
        ~where_to_listen
        ~initial_connection_state:(fun _socket_addr _connection -> ())
        ?heartbeat_config
        ()
    in
    log_listening ~log ~config;
    Deferred.Or_error.return ()]
;;

let start (config, server_events) ~log ~plugin_rpcs =
  let implementations = implementations server_events @ plugin_rpcs in
  default_start_rpc_server ~config ~log ~implementations () >>| Or_error.ok_exn
;;
