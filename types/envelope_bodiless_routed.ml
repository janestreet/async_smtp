open! Core

module T = struct
  type t = Envelope_bodiless.t Routed.t [@@deriving sexp_of, compare, hash]

  let create ~envelope ~next_hop_choices ~retry_intervals =
    Routed.Fields.create ~envelope ~next_hop_choices ~retry_intervals
  ;;

  let next_hop_choices = Routed.next_hop_choices
  let retry_intervals = Routed.retry_intervals
  let envelope_bodiless = Routed.envelope
  let envelope_info t = Envelope_bodiless.info (envelope_bodiless t)
  let headers t = Envelope_bodiless.headers (envelope_bodiless t)
  let info t = Envelope_bodiless.info t.Routed.envelope

  let set
    ?sender
    ?sender_args
    ?recipients
    ?rejected_recipients
    ?route
    ?next_hop_choices
    ?retry_intervals
    ?headers
    t
    ()
    =
    { Routed.envelope =
        Envelope_bodiless.set
          (envelope_bodiless t)
          ?sender
          ?sender_args
          ?recipients
          ?rejected_recipients
          ?route
          ?headers
          ()
    ; retry_intervals = Option.value retry_intervals ~default:t.Routed.retry_intervals
    ; next_hop_choices = Option.value next_hop_choices ~default:t.Routed.next_hop_choices
    }
  ;;

  let set_headers t headers = set ~headers t ()
end

include T
include Envelope_container.Make_with_headers (T)
include Envelope_container.Make_with_info (T)
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)
