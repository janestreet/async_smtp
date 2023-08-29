open! Core
open Email_message

(** Allow for sharing on an [email_body] across multiple [Envelope_with_next_hop.t]'s *)

type t =
  { envelopes : Envelope_bodiless_routed.t list
  ; email_body : Email.Raw_content.t
  }
[@@deriving fields ~getters, sexp_of]

val single_envelope : Envelope_routed.t -> t
val to_envelopes : t -> Envelope_routed.t list
