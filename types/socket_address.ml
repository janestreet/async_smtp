module Stable = struct
  open Core.Core_stable
  module V1_no_string = struct
    type t = [`Inet of Host_and_port.V1.t | `Unix of string]
    [@@deriving sexp, bin_io]
  end
  module V1 = struct
    include V1_no_string
    include Core.Sexpable.To_stringable(V1_no_string)
  end
end

open! Core

module T = struct
  type t = [`Inet of Host_and_port.t | `Unix of string]
  [@@deriving sexp_of, compare, hash]
end

include T
let to_string t = Sexp.to_string (sexp_of_t t)
include Comparable.Make_plain(T)
include Hashable.Make_plain(T)
