open! Core.Std
open! Async.Std
open Async_ssl.Std

module Tls : sig
  type t =
    { version : Ssl.Version.t option
    ; options : Ssl.Opt.t list option
    ; name : string option
    ; crt_file : string
    ; key_file : string
    ; ca_file : string option
    ; ca_path : string option
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
  ; tls_options                          : Tls.t option
  ; client                               : Client_config.t
  } [@@deriving fields, sexp]

val load_exn : string -> t Deferred.t

val tmp_dir   : t -> string

val empty : t
