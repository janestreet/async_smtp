open! Core

type t =
  | Hello of string
  | Extended_hello of string
  | Sender of string
  | Recipient of string
  | Auth of string * string option
  | Data
  | Reset
  | Quit
  | Help
  | Noop
  | Start_tls

val to_string : t -> string
val of_string : string -> t
