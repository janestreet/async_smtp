open Core.Std
open Async.Std

let implementations =
  let open Rpc_impl in
  [ Spool.status ()
  ; Spool.freeze ()
  ; Spool.send_now ()
  ; Spool.events ()
  ; Spool.set_max_concurrent_send_jobs ()
  ; Gc.stat ()
  ; Gc.quick_stat ()
  ; Gc.full_major ()
  ; Gc.major ()
  ; Gc.minor ()
  ; Gc.compact ()
  ; Gc.stat_pipe ()
  ; Process.pid ()
  ]
;;

let start (config, spool) ~plugin_rpcs =
  let initial_connection_state = (fun _socket_addr _connection -> (config, spool)) in
  let where_to_listen = Tcp.on_port (Config.rpc_port config) in
  let implementations =
    implementations
    @ List.map plugin_rpcs ~f:(Rpc.Implementation.lift ~f:ignore)
  in
  let implementations =
    Rpc.Implementations.create_exn ~implementations ~on_unknown_rpc:`Raise
  in
  Rpc.Connection.serve ~implementations ~where_to_listen ~initial_connection_state ()
  >>= fun _tcp_server ->
  Log.Global.info "RPC server connection closed";
  Deferred.unit
;;
