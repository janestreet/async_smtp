open! Core
open! Async.Std

type t = string [@@deriving bin_io, sexp, compare]

val init : path:string -> t Or_error.t Deferred.t

(* List of full paths. *)
val ls : t -> string list Or_error.t Deferred.t

val active_dir     : t -> string
val tmp_dir        : t -> string
val frozen_dir     : t -> string
val removed_dir    : t -> string
val quarantine_dir : t -> string
