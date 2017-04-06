open! Core

type t = [`Inet of Host_and_port.t | `Unix of string]
[@@deriving sexp, compare, bin_io, hash]

include Stringable.S with type t := t
