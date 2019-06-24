open! Core
open! Async
open! Async_smtp

module Command : sig
  include module type of Command

  val rpc
    :  summary:string
    -> ?readme:(unit -> string)
    -> (Rpc.Connection.t -> unit Deferred.t) Param.t
    -> t

  val configs_or_rpc
    :  summary:string
    -> ?readme:(unit -> string)
    -> ([ `Configs of Smtp_server.Config.t * Smtp_spool.Config.t
        | `Rpc of Rpc.Connection.t
        ]
        -> unit Deferred.t)
         Param.t
    -> t
end
