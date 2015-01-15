open Core.Std
open Async.Std

type t =
  { spool_dir                            : string
  ; tmp_dir                              : string option
  ; ports                                : int list
  ; max_concurrent_send_jobs             : int
  ; max_concurrent_receive_jobs_per_port : int
  ; rpc_port                             : int
  ; malformed_emails                     : [ `Reject | `Wrap ]
  ; max_message_size                     : Byte_units.t
  } with fields, sexp

val load_exn : string -> t Deferred.t

val tmp_dir   : t -> string

val empty : t
