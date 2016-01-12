open Core.Std
open Async.Std
open Async_smtp.Std
open Common

val msgid  : Smtp_spool.Spooled_message_id.t Command.Spec.Arg_type.t
val format : [ `Ascii_table | `Ascii_table_with_max_width of int | `Exim | `Sexp ] Command.Spec.Arg_type.t

val status
  :  format : [ `Ascii_table | `Ascii_table_with_max_width of int | `Exim | `Sexp ]
  -> Rpc.Connection.t
  -> unit Deferred.t

val count
  :  Rpc.Connection.t
  -> unit Deferred.t

val set_max_send_jobs
  :  num:int
  -> Rpc.Connection.t
  -> unit Deferred.t

val freeze
  :  msgids:Smtp_spool.Spooled_message_id.t list
  -> Rpc.Connection.t
  -> unit Deferred.t

val send
  :  ?retry_intervals:Time.Span.t list
  -> Smtp_spool.Send_info.t
  -> Rpc.Connection.t
  -> unit Deferred.t

val remove
  : msgids:Smtp_spool.Spooled_message_id.t list
  -> Rpc.Connection.t
  -> unit Deferred.t

val recover
  : msgids:Smtp_spool.Spooled_message_id.t list
  -> Rpc.Connection.t
  -> unit Deferred.t

val events
  :  Rpc.Connection.t
  -> unit Deferred.t

val command : Command.t
