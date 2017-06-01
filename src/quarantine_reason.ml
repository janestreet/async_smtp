open! Core

module T = struct
  type t =
    { description     : string
    ; envelope_sender : Sender.t
    ; from_headers    : string
    } [@@deriving bin_io, compare, sexp]
end

include T
include Sexpable.To_stringable (T)

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
