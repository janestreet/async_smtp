open! Core
open Email_message

(* Two envelopes are equal when they produce the same SMTP output. In
   particular, ids are ignored for comparison. Same is true for hashing. *)
type t [@@deriving sexp_of]
type envelope = t [@@deriving sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain   with type t := t

include Envelope_container.With_headers with type t := t
include Envelope_container.With_info    with type t := t

val create
  :  ?id:Envelope_id.t
  -> sender:Sender.t
  -> ?sender_args:Sender_argument.t list
  -> recipients:Email_address.t list
  -> ?rejected_recipients:Email_address.t list
  -> ?route:string
  -> email:Email.t
  -> unit
  -> t

val create' : info:Envelope_info.t -> email:Email.t -> t

val info  : t -> Envelope_info.t
val email : t -> Email.t

val set
  :  t
  -> ?sender:Sender.t
  -> ?sender_args:Sender_argument.t list
  -> ?recipients:Email_address.t list
  -> ?rejected_recipients:Email_address.t list
  -> ?route:string option
  -> ?email:Email.t
  -> unit
  -> t

val set' : t -> ?info:Envelope_info.t -> ?email:Email.t -> unit -> t

(* Extracts sender and recipients from the headers. *)
val of_email : Email.t -> t Or_error.t

val modify_email
  : t -> f:(Email.t -> Email.t) -> t

module Stable : sig
  module V1 : sig type t [@@deriving bin_io, sexp] end
  module V2 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
    val of_v1 : V1.t -> t
  end
end
