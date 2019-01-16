open! Core
open Email_message

type t = Envelope_bodiless.t Routed.t [@@deriving sexp_of]

include Envelope_container.With_headers with type t := t
include Envelope_container.With_info with type t := t

val next_hop_choices : t -> Host_and_port.t list
val retry_intervals : t -> Retry_interval.t list
val envelope_bodiless : t -> Envelope_bodiless.t
val envelope_info : t -> Envelope_info.t
val headers : t -> Email_headers.t
val create : Envelope_bodiless.t Routed.create
val set : (?headers:Email_headers.t -> t -> unit -> t) Routed.set
