module Stable = struct
  open Core.Core_stable
  module Sender = Sender.Stable

  module V1_no_string = struct
    type t =
      { description     : string
      ; envelope_sender : Sender.V1.t
      ; from_headers    : string
      } [@@deriving bin_io, sexp]
  end

  module V1 = struct
    include V1_no_string
    include Core.Sexpable.To_stringable(V1_no_string)
  end
end

open! Core

type t = Stable.V1.t =
  { description     : string
  ; envelope_sender : Sender.t
  ; from_headers    : string
  } [@@deriving sexp_of, compare, fields]

let to_string t = Sexp.to_string (sexp_of_t t)

let of_envelope ~description envelope =
  let from_headers =
    Envelope.find_all_headers envelope "From"
    |> String.concat ~sep:", "
  in
  { description
  ; envelope_sender = Envelope.sender envelope
  ; from_headers
  }
;;
