open! Core

type t = [`Inet of Host_and_port.t | `Unix of string]
[@@deriving sexp_of, compare, hash]

val to_string : t -> string
include Comparable.S_plain with type t := t
include Hashable.S_plain   with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = [ | t ] [@@deriving sexp, bin_io]
    include Stringable.S with type t:=t
  end
end
