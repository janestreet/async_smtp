open Core
open Async
open Async_ssl.Std

module Tls_options = struct
  type t =
    { version:Ssl.Version.t option
    ; options:Ssl.Opt.t list option
    ; name:string option
    ; allowed_ciphers : [ `Secure | `Openssl_default | `Only of string list ]
    ; crt_file:string
    ; key_file:string
    ; ca_file:string option
    ; ca_path : string option
    } [@@deriving fields, sexp]
end

module Tcp_options = struct
  type t =
    { max_accepts_per_batch : int sexp_option
    ; backlog               : int sexp_option
    } [@@deriving fields, sexp]
end

module Where_to_listen = struct
  type t = [ `Port of int | `File of string ] [@@deriving sexp]
end

type t =
  { spool_dir                            : string
  ; tmp_dir                              : string option
  ; where_to_listen                      : Where_to_listen.t list
  ; max_concurrent_send_jobs             : int
  ; max_concurrent_receive_jobs_per_port : int
  ; rpc_port                             : int
  ; malformed_emails                     : [ `Reject | `Wrap ]
  ; max_message_size                     : Byte_units.t
  ; tls_options                          : Tls_options.t sexp_option
  ; tcp_options                          : Tcp_options.t sexp_option
  ; client                               : Client_config.t
  } [@@deriving fields, sexp]

let load_exn file =
  Reader.load_sexp file t_of_sexp ~expand_macros:true
  >>| Or_error.ok_exn
;;

let tmp_dir t = Option.value ~default:(spool_dir t ^/ "temp") (tmp_dir t)

let default =
  { spool_dir = "."
  ; tmp_dir = None
  ; where_to_listen = []
  ; max_concurrent_send_jobs = 0
  ; max_concurrent_receive_jobs_per_port = 0
  ; rpc_port = 0
  ; malformed_emails = `Reject
  ; max_message_size = Byte_units.create `Megabytes 24.
  ; tls_options = None
  ; tcp_options = None
  ; client = Client_config.default
  }
