open! Core
open! Async

type t =
  { address : Host_and_port.t
  ; credentials : Credentials.t option
  ; route : string option
  }
[@@deriving fields ~getters, sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

module Stable : sig
  module V1 : sig
    type t [@@deriving sexp, bin_io]
  end

  module V2 : sig
    type nonrec t = t [@@deriving sexp, bin_io]

    val of_v1 : V1.t -> t
    val to_v1 : t -> V1.t
  end
end
