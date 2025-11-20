open! Core
open Email_message

(* Two envelopes are equal when they produce the same SMTP output. In particular, ids are
   ignored for comparison. Same is true for hashing. *)

type t [@@deriving sexp_of]
type envelope = t [@@deriving sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
include Envelope_container.With_headers with type t := t
include Envelope_container.With_info with type t := t

val create : (email:Email.t -> unit -> t) Envelope_info.create
val create' : info:Envelope_info.t -> email:Email.t -> t
val info : t -> Envelope_info.t
val email : t -> Email.t
val set : (?email:Email.t -> t -> unit -> t) Envelope_info.set
val set' : t -> ?info:Envelope_info.t -> ?email:Email.t -> unit -> t

(** Extracts sender and recipients from the headers. The recipients are parsed from the
    "To", "Cc" and "Bcc" headers. If [~ignore_unparseable_recipient_header=true], then an
    unparseable header will be ignored rather than erroring. *)
val of_email
  :  ?ignore_unparseable_recipient_header:bool (** default: false *)
  -> Email.t
  -> t Or_error.t

val modify_email : t -> f:(Email.t -> Email.t) -> t
val of_bodiless : Envelope_bodiless.t -> Email.Raw_content.t -> t
val split_bodiless : t -> Envelope_bodiless.t * Email.Raw_content.t
val with_bodiless : t -> (Envelope_bodiless.t -> Envelope_bodiless.t) -> t

module Stable : sig
  module V1 : sig
    type t [@@deriving bin_io, sexp]
  end

  module V2 : sig
    include Stable_without_comparator with type t = t

    val of_v1 : V1.t -> t
  end
end
