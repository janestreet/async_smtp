open! Core
open! Async
open Async_smtp.Std
open Common

module Status : sig
  type format = [ `Ascii_table | `Ascii_table_with_max_width of int | `Exim | `Sexp ] [@@deriving sexp]

  val spec : unit -> (format:format -> 'a, 'a) Command.Spec.t
  val dispatch
    :  format : [ `Ascii_table | `Ascii_table_with_max_width of int | `Exim | `Sexp ]
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Count : sig
  val spec : unit -> ('a, 'a) Command.Spec.t

  val dispatch
    :  Rpc.Connection.t
    -> unit Deferred.t
end

module Set_max_send_jobs : sig
  val spec : unit -> (num:int -> 'a, 'a) Command.Spec.t

  val dispatch
    :  num:int
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Freeze : sig
  val spec : unit -> (msgids:Smtp_spool.Message_id.t list -> 'a, 'a) Command.Spec.t

  val dispatch
    :  msgids:Smtp_spool.Message_id.t list
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Send : sig
  val spec :
    unit ->
    (?retry_intervals:Time.Span.t list -> Smtp_spool.Send_info.t -> 'a, 'a)
      Common.Command.Spec.t

  val dispatch
    :  ?retry_intervals:Time.Span.t list
    -> Smtp_spool.Send_info.t
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Remove : sig
  val spec : unit -> (msgids:Smtp_spool.Message_id.t list -> 'a, 'a) Command.Spec.t

  val dispatch
    :  msgids:Smtp_spool.Message_id.t list
    -> Rpc.Connection.t
    -> unit Deferred.t
end


module Recover : sig
  val spec : unit -> (Smtp_spool.Recover_info.t -> 'a, 'a) Command.Spec.t

  val dispatch
    :  Smtp_spool.Recover_info.t
    -> Rpc.Connection.t
    -> unit Deferred.t
end

module Events : sig
  val spec : unit -> ('a, 'a) Command.Spec.t

  val dispatch
    :  Rpc.Connection.t
    -> unit Deferred.t
end

val command : Command.t
