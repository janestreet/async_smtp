open Core
open Async

let implementations () =
  [ List.map Rpc_impl.Smtp_events.rpcs ~f:(Rpc.Implementation.lift ~f:snd)
  ; List.map Rpc_impl.Gc.rpcs ~f:(Rpc.Implementation.lift ~f:ignore)
  ; List.map (Rpc_impl.Monitor.rpcs ()) ~f:(Rpc.Implementation.lift ~f:ignore)
  ; List.map Rpc_impl.Process.rpcs ~f:(Rpc.Implementation.lift ~f:ignore)
  ]
  |> List.concat
;;

let start (config, server_events) ~log ~plugin_rpcs =
  let initial_connection_state _socket_addr _connection = config, server_events in
  let where_to_listen = Tcp.Where_to_listen.of_port (Server_config.rpc_port config) in
  let heartbeat_config = Server_config.rpc_heartbeat_config config in
  let implementations =
    implementations () @ List.map plugin_rpcs ~f:(Rpc.Implementation.lift ~f:ignore)
  in
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations
      ~on_unknown_rpc:`Raise
      ~on_exception:Log_on_background_exn
  in
  let%bind _tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~where_to_listen
      ~initial_connection_state
      ?heartbeat_config
      ()
  in
  [%log.t.info_format log "RPC server listening on %d" (Server_config.rpc_port config)];
  Deferred.unit
;;
