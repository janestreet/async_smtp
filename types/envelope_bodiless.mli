open! Core
open Email_message

type t [@@deriving sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

val create : (headers:Email_headers.t -> unit -> t) Envelope_info.create
val create' : info:Envelope_info.t -> headers:Email_headers.t -> t
val set : (?headers:Email_headers.t -> t -> unit -> t) Envelope_info.set

include Envelope_container.With_headers with type t := t
include Envelope_container.With_info with type t := t
