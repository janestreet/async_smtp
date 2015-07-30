open Core.Std
open Async.Std
open Async_ssl.Std

module Tls = struct
  type t =
    { version:Ssl.Version.t option
    ; name:string option
    ; crt_file:string
    ; key_file:string
    ; ca_file:string option
    ; ca_path : string option
    } with fields, sexp
end

type t =
  { spool_dir                            : string
  ; tmp_dir                              : string option
  ; ports                                : int list
  ; max_concurrent_send_jobs             : int
  ; max_concurrent_receive_jobs_per_port : int
  ; rpc_port                             : int
  ; malformed_emails                     : [ `Reject | `Wrap ]
  ; max_message_size                     : Byte_units.t
  ; tls_options                          : Tls.t sexp_option
  ; client                               : Client_config.t
  } with fields, sexp

let load_exn file =
  Reader.load_sexp file t_of_sexp ~expand_macros:true
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
  ; tls_options = None
  ; client = Client_config.default
  }
