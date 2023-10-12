open! Core
open Async_smtp_types

type t [@@deriving sexp_of, compare]

val description : t -> string
val envelope_sender : t -> Smtp_envelope.Sender.t
val from_headers : t -> string
val to_string : t -> string
val of_envelope : description:string -> Smtp_envelope.Bodiless.t -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare]

    include Stringable.S with type t := t
  end
end
