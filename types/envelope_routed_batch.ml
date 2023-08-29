open! Core
open Email_message

type t =
  { envelopes : Envelope_bodiless_routed.t list
  ; email_body : Email.Raw_content.t
  }
[@@deriving fields ~getters, sexp_of]

let single_envelope envelope =
  let bodiless, email_body = Envelope_routed.split_bodiless envelope in
  { envelopes = [ bodiless ]; email_body }
;;

let to_envelopes t =
  List.map t.envelopes ~f:(fun bodiless ->
    Envelope_routed.of_bodiless bodiless t.email_body)
;;
