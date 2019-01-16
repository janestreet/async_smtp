open! Core
open! Async

type t =
  { spool_dir : string
  ; tmp_dir : string option
  ; max_concurrent_send_jobs : int
  ; client : Client_config.t
  }
[@@deriving fields, sexp]

val tmp_dir : t -> string
val default : t
val load_exn : string -> t Deferred.t
