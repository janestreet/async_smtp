open! Async

val start
  :  Server_config.t * Smtp_events.t
  -> log:Log.t
  -> plugin_rpcs:unit Rpc.Implementation.t list
  -> unit Deferred.t
