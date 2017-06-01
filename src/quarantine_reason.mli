open! Core

type t [@@deriving bin_io, compare, sexp]

val of_envelope
  :  description:string
  -> Envelope.t
  -> t

include Stringable.S with type t := t
