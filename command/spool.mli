open! Core
open! Async
open Async_smtp
open Common

module Client_side_filter : sig
  type t =
    { next_hop : Re2.t option
    ; sender : Re2.t option
    ; recipient : Re2.t option
    ; queue : Smtp_spool_message.Queue.t option
    ; younger_than : Time_float_unix.Span.t option
    ; older_than : Time_float_unix.Span.t option
    }

  val param_opt : t option Command.Param.t
end

module Status : sig
  module Format : sig
    type t =
      [ `Ascii_table
      | `Ascii_table_with_max_width of int
      | `Exim
      | `Sexp
      | `Id
      ]
    [@@deriving sexp]

    val arg_type : t Command.Param.Arg_type.t
    val param : t Command.Param.t
  end

  val dispatch
    :  format:Format.t
    -> ?client_side_filter:Client_side_filter.t
    -> Rpc.Connection.t
    -> unit Deferred.t

  val on_disk
    :  format:Format.t
    -> ?client_side_filter:Client_side_filter.t
    -> Smtp_spool.Config.t
    -> unit Deferred.t
end

module Count : sig
  val which : [ `Only_frozen | `Only_active | `All ] Command.Param.t

  val dispatch
    :  which:[ `Only_frozen | `Only_active | `All ]
    -> ?client_side_filter:Client_side_filter.t
    -> Rpc.Connection.t
    -> int Deferred.t

  val on_disk
    :  ?client_side_filter:Client_side_filter.t
    -> Smtp_spool.Config.t
    -> unit Deferred.t
end

module Freeze : sig
  val msgids : Smtp_spool.Message_id.t list Command.Param.t

  val dispatch
    :  msgids:Smtp_spool.Message_id.t list
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Send : sig
  val retry_intervals : Smtp_envelope.Retry_interval.t list Command.Param.t
  val param : Smtp_spool.Send_info.t Command.Param.t

  val dispatch
    :  ?retry_intervals:Smtp_envelope.Retry_interval.t list
    -> Smtp_spool.Send_info.t
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Remove : sig
  val msgids : Smtp_spool.Message_id.t list Command.Param.t

  val dispatch
    :  msgids:Smtp_spool.Message_id.t list
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Recover : sig
  val param : Smtp_spool.Recover_info.t Command.Param.t
  val dispatch : Smtp_spool.Recover_info.t -> Rpc.Connection.t -> unit Deferred.t
end

module Events : sig
  val dispatch : Rpc.Connection.t -> unit Deferred.t
end

val command : Command.t
