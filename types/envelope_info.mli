open! Core
open Email_message

type t [@@deriving sexp_of]

type 'a create =
  ?id:Envelope_id.t
  -> sender:Sender.t
  -> ?sender_args:Sender_argument.t list
  -> recipients:Email_address.t list
  -> ?rejected_recipients:Email_address.t list
  -> ?route:string
  -> 'a

val create : (unit -> t) create

type 'a set =
  ?sender:Sender.t
  -> ?sender_args:Sender_argument.t list
  -> ?recipients:Email_address.t list
  -> ?rejected_recipients:Email_address.t list
  -> ?route:string option
  -> 'a

val set : (t -> unit -> t) set

(* Extracts sender and recipients from the headers. *)

val of_email : ?ignore_unparseable_recipient_header:bool -> Email.t -> t Or_error.t

include Envelope_container_intf.With_info with type t := t and type envelope_info := t
include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

module Stable : sig
  module V1 : sig
    type t [@@deriving bin_io, sexp]
  end

  module V2 : sig
    include Stable_without_comparator with type t = t

    val of_v1 : V1.t -> t
  end
end
