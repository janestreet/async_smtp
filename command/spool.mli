open! Core
open! Async
open Async_smtp
open Common

module Status : sig
  module Format : sig
    type t =
      [ `Ascii_table
      | `Ascii_table_with_max_width of int
      | `Exim
      | `Sexp
      ] [@@deriving sexp]

    val arg_type : t Command.Param.Arg_type.t
    val param : t Command.Param.t
  end

  val dispatch
    :  format : [ `Ascii_table | `Ascii_table_with_max_width of int | `Exim | `Sexp ]
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Count : sig
  val which : [`Only_frozen | `Only_active | `All] Command.Param.t

  val dispatch
    :  which:[`Only_frozen | `Only_active | `All]
    -> Rpc.Connection.t
    -> int Deferred.t
end

module Set_max_send_jobs : sig
  val num : int Command.Param.t

  val dispatch
    :  num:int
    -> Rpc.Connection.t
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

  val dispatch
    :  Smtp_spool.Recover_info.t
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Events : sig
  val dispatch
    :  Rpc.Connection.t
    -> unit Deferred.t
end

val command : Command.t
