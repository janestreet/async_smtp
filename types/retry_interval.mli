open! Core

type t [@@deriving compare, hash, sexp_of]

(** [create ?jitter span] creates a retry interval of a uniformly random time span in the
    range [span - jitter, span + jitter] *)
val create : ?jitter:Time_float.Span.t -> Time_float.Span.t -> t

val to_span : t -> Time_float.Span.t

module Stable : sig
  module V2 : sig
    type nonrec t = t [@@deriving sexp, bin_io]
  end
end
