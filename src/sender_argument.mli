open! Core
open Email_message.Std

(* These arguments become optional parameters to the "MAIL FROM" smtp command depending on
   certain advertised smtp extensions. *)
type t =
  | Auth of Email_address.t option
  | Body of [`Mime_8bit|`Mime_7bit]
[@@deriving bin_io, sexp, compare, hash]

val of_string : string -> t Or_error.t
val to_string : t -> string

val list_of_string
  :  allowed_extensions:Smtp_extension.t list
  -> string
  -> t list Or_error.t
