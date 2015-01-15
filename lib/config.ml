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

let load_exn file =
  Reader.load_sexp file t_of_sexp
  >>| Or_error.ok_exn
;;

let tmp_dir t = Option.value ~default:(spool_dir t ^/ "temp") (tmp_dir t)

let empty =
  { spool_dir = "."
  ; tmp_dir = None
  ; ports = []
  ; max_concurrent_send_jobs = 0
  ; max_concurrent_receive_jobs_per_port = 0
  ; rpc_port = 0
  ; malformed_emails = `Reject
  ; max_message_size = Byte_units.create `Megabytes 24.
  }
