open! Core
open! Async

(** The built-in async_smtp RPC implementations (smtp_events, gc, monitor, process).

    The [Smtp_events.t] argument provides the event stream for the smtp_events RPCs. The
    resulting implementations use [unit] as their connection state, so they can be
    combined with plugin RPCs and served with any custom RPC serving mechanism. *)
val implementations : Smtp_events.t -> unit Rpc.Implementation.t list

val default_start_rpc_server
  :  ?where_to_listen_override:Tcp.Where_to_listen.inet
  -> config:Server_config.t
  -> log:Log.t
  -> implementations:unit Rpc.Implementation.t list
  -> unit
  -> unit Or_error.t Deferred.t

(** Convenience wrapper: assembles the built-in implementations for the given
    [Smtp_events.t] together with [plugin_rpcs] and serves them using
    [default_start_rpc_server]. *)
val start
  :  Server_config.t * Smtp_events.t
  -> log:Log.t
  -> plugin_rpcs:unit Rpc.Implementation.t list
  -> unit Deferred.t
