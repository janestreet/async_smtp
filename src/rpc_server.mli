open! Async.Std

val start
  :  (Server_config.t * Spool.t * Smtp_events.t)
  -> plugin_rpcs:unit Rpc.Implementation.t list
  -> unit Deferred.t
