open! Core

type t =
  | Start_tls
  | Auth of string list
  | Mime_8bit_transport
  | Other of string
[@@deriving compare, sexp, enumerate]

include Equal.S with type t := t

val of_string : string -> t
val to_string : t -> string
