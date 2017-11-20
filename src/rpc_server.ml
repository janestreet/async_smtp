open Core
open Async

let implementations () =
  let open Rpc_impl in
  [ Smtp_events.events ()
  ; Spool.status ()
  ; Spool.freeze ()
  ; Spool.send ()
  ; Spool.remove ()
  ; Spool.recover ()
  ; Spool.events ()
  ; Gc.stat ()
  ; Gc.quick_stat ()
  ; Gc.full_major ()
  ; Gc.major ()
  ; Gc.minor ()
  ; Gc.compact ()
  ; Gc.stat_pipe ()
  ; Monitor.errors ()
  ; Process.pid ()
  ; Cache.status ()
  ; Cache.config ()
  ]
;;

let start (config, spool, server_events) ~log ~plugin_rpcs =
  let initial_connection_state =
    (fun _socket_addr _connection -> (config, spool, server_events))
  in
  let where_to_listen = Tcp.Where_to_listen.of_port (Server_config.rpc_port config) in
  let implementations =
    implementations ()
    @ List.map plugin_rpcs ~f:(Rpc.Implementation.lift ~f:ignore)
  in
  let implementations =
    Rpc.Implementations.create_exn ~implementations ~on_unknown_rpc:`Raise
  in
  Rpc.Connection.serve ~implementations ~where_to_listen ~initial_connection_state ()
  >>= fun _tcp_server ->
  Log.info log "RPC server listening on %d" (Server_config.rpc_port config);
  Deferred.unit
;;
