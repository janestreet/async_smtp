open! Core

type t =
  { username : string
  ; password : string
  } [@@deriving sexp]

val create : username:string -> password:string -> t

val username : t -> string
val password : t -> string
