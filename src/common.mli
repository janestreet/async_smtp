open! Core
open! Async

val is_accessible_directory
  :  ?create_if_missing:unit
  -> string
  -> unit Deferred.Or_error.t

val safely_ls_dir : string -> string list Deferred.Or_error.t
val unlink : string -> unit Deferred.Or_error.t
val rename : src:string -> dst:string -> unit Deferred.Or_error.t
