open! Core

type t [@@deriving bin_io, compare, hash, sexp]

(** [create ?jitter span] creates a retry interval of a uniformly random time span in the
    range [span - jitter, span + jitter] *)
val create : ?jitter:Time.Span.t -> Time.Span.t -> t

val to_span : t -> Time.Span.t
