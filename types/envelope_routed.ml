open! Core

module T = struct
  type t =
    { envelope : Envelope.t
    ; next_hop_choices : Socket_address.t list
    ; retry_intervals  : Retry_interval.t list
    } [@@deriving fields, sexp_of, compare, hash]

  let info t = Envelope.info t.envelope
end

include T
include Envelope_container.Make_with_info(T)
include Comparable.Make_plain(T)
include Hashable.Make_plain(T)

let create ~envelope ~next_hop_choices ~retry_intervals =
  Fields.create ~envelope ~next_hop_choices ~retry_intervals
;;

let email t = Envelope.email t.envelope

let set t ?sender ?sender_args ?recipients () =
  { t with envelope = Envelope.set t.envelope ?sender ?sender_args ?recipients () }
;;
