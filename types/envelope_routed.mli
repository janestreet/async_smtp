open! Core
open! Email_message

type t = Envelope.t Routed.t [@@deriving sexp_of]

include Envelope_container.With_headers with type t := t
include Envelope_container.With_info with type t := t
include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

val next_hop_choices : t -> Host_and_port.t list
val retry_intervals : t -> Retry_interval.t list
val envelope : t -> Envelope.t
val email : t -> Email.t
val create : Envelope.t Routed.create
val set : (?email:Email.t -> t -> unit -> t) Routed.set
val of_bodiless : Envelope_bodiless_routed.t -> Email.Raw_content.t -> t
val split_bodiless : t -> Envelope_bodiless_routed.t * Email.Raw_content.t
val with_bodiless : t -> (Envelope_bodiless_routed.t -> Envelope_bodiless_routed.t) -> t
