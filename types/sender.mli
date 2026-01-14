open! Core

(* From 3.7 Relaying:

   One way to prevent loops in error reporting is to specify a null reverse-path in the
   MAIL command of a notification message. When such a message is transmitted the
   reverse-path MUST be set to null (see section 4.5.5 for additional discussion). A MAIL
   command with a null reverse-path appears as follows:

   MAIL FROM:<>
*)

type t =
  [ `Null
  | `Email of Email_address.t
  ]
[@@deriving compare, hash, sexp_of]

val of_string : ?default_domain:string -> string -> t Or_error.t
val of_string_exn : ?default_domain:string -> string -> t

val of_string_with_arguments_string
  :  ?default_domain:string
  -> string
  -> (t * string) Or_error.t

val of_string_with_arguments
  :  ?default_domain:string
  -> allowed_extensions:Smtp_extension.t list (* default: [] *)
  -> string
  -> (t * Sender_argument.t list) Or_error.t

val to_string : t -> string
val to_string_with_arguments : t * Sender_argument.t list -> string
val map : t -> f:(Email_address.t -> Email_address.t) -> t

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

module Caseless : sig
  type nonrec t = t

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module For_test : sig
  type nonrec t = t [@@deriving sexp_of]
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, hash, stable_witness]
  end
end
