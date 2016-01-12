open Async.Std

val start
  :  (Server_config.t * Spool.t)
  -> plugin_rpcs:unit Rpc.Implementation.t list
  -> unit Deferred.t
