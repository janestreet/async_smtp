open! Core
open Email_message

module T = struct
  type t = Envelope.t Routed.t [@@deriving sexp_of, compare, hash]

  let create ~envelope ~next_hop_choices ~retry_intervals =
    Routed.Fields.create ~envelope ~next_hop_choices ~retry_intervals
  ;;

  let next_hop_choices = Routed.next_hop_choices
  let retry_intervals = Routed.retry_intervals
  let envelope = Routed.envelope
  let email t = Envelope.email (envelope t)
  let info t = Envelope.info t.Routed.envelope
  let headers t = Email.headers (email t)

  let set
    ?sender
    ?sender_args
    ?recipients
    ?rejected_recipients
    ?route
    ?next_hop_choices
    ?retry_intervals
    ?email
    t
    ()
    =
    { Routed.envelope =
        Envelope.set
          (envelope t)
          ?sender
          ?sender_args
          ?recipients
          ?rejected_recipients
          ?route
          ?email
          ()
    ; retry_intervals = Option.value retry_intervals ~default:t.Routed.retry_intervals
    ; next_hop_choices = Option.value next_hop_choices ~default:t.Routed.next_hop_choices
    }
  ;;

  let set_headers t headers =
    let email = Email.set_headers (email t) headers in
    set ~email t ()
  ;;

  let of_bodiless bodiless body =
    let envelope =
      Envelope.of_bodiless (Envelope_bodiless_routed.envelope_bodiless bodiless) body
    in
    create
      ~envelope
      ~next_hop_choices:(Envelope_bodiless_routed.next_hop_choices bodiless)
      ~retry_intervals:(Envelope_bodiless_routed.retry_intervals bodiless)
  ;;

  let split_bodiless { Routed.envelope; next_hop_choices; retry_intervals } =
    let bodiless, email_body = Envelope.split_bodiless envelope in
    let bodiless =
      Envelope_bodiless_routed.create
        ~next_hop_choices
        ~retry_intervals
        ~envelope:bodiless
    in
    bodiless, email_body
  ;;

  let with_bodiless t f =
    let bodiless, body = split_bodiless t in
    of_bodiless (f bodiless) body
  ;;
end

include T
include Envelope_container.Make_with_headers (T)
include Envelope_container.Make_with_info (T)
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)
