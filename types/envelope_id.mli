open! Core

type t [@@deriving sexp_of]

val to_string : t -> string
val of_string : string -> t
val create : unit -> t
val urlbase64_encode_float : ?length:int -> float -> t

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
