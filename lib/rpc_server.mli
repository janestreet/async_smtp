open Async.Std

val start
  :  (Config.t * Spool.t)
  -> plugin_rpcs:unit Rpc.Implementation.t list
  -> unit Deferred.t
