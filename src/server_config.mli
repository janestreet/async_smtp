open! Core
open! Async
open Async_ssl.Std

module Tls_options : sig
  type t =
    { version : Ssl.Version.t option
    ; options : Ssl.Opt.t list option
    ; name : string option
    ; allowed_ciphers : [ `Secure | `Openssl_default | `Only of string list ]
    ; crt_file : string
    ; key_file : string
    ; ca_file : string option
    ; ca_path : string option
    } [@@deriving fields, sexp]
end

module Tcp_options : sig
  type t =
    { max_accepts_per_batch : int sexp_option
    ; backlog               : int sexp_option
    } [@@deriving fields, sexp]
end

module Where_to_listen : sig
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
  ; tls_options                          : Tls_options.t option
  ; tcp_options                          : Tcp_options.t option
  ; client                               : Client_config.t
  } [@@deriving fields, sexp]

val load_exn : string -> t Deferred.t

val tmp_dir   : t -> string

val default : t
