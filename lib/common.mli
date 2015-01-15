open Core.Std
open Async.Std
open Types

val eof_error : Error.t

val mlog :
  severity:[ `Debug | `Error | `Info ]
  -> ?message:Envelope.t
  -> ?dir:[ `In | `Out ]
  -> ('a, unit, string, unit) format4
  -> 'a

val exchange
  :  Reader.t
  -> Writer.t
  -> string
  -> [ `Ok of string | `Eof ] Deferred.t

val is_accessible_directory
  :  ?create_if_missing:unit
  -> string
  -> unit Deferred.Or_error.t

val safely_ls_dir
  :  string
  -> string list Deferred.Or_error.t

val unlink
  :  string
  -> unit Deferred.Or_error.t

val rename
  :  src:string
  -> dst:string
  -> unit Deferred.Or_error.t
