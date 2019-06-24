open! Core
open! Async

type t =
  { spool_dir : string
  ; tmp_dir : string option
  ; connection_cache : Resource_cache.Address_config.Stable.V1.t
  ; client : Client_config.t
  ; load_balance : bool
  }
[@@deriving fields, sexp]

val tmp_dir : t -> string
val default : t
val load_exn : string -> t Deferred.t
