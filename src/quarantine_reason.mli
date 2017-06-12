open! Core

type t [@@deriving bin_io, compare, sexp]

val description     : t -> string
val envelope_sender : t -> Sender.t
val from_headers    : t -> string

val of_envelope
  :  description:string
  -> Envelope.t
  -> t

include Stringable.S with type t := t
