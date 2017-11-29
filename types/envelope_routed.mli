open! Core
open! Email_message

type t =
  { envelope  : Envelope.t
  (* Next hops to try. If the first one fails, we are done, otherwise try
     sending to the second one, etc. *)
  ; next_hop_choices : Socket_address.t list
  ; retry_intervals  : Retry_interval.t list
  } [@@deriving fields, sexp_of]

include Envelope_container.With_info with type t := t

include Comparable.S_plain with type t := t
include Hashable.S_plain   with type t := t

val create
  :  envelope : Envelope.t
  -> next_hop_choices : Socket_address.t list
  -> retry_intervals : Retry_interval.t list
  -> t

val email : t -> Email.t

val set
  :  t
  -> ?sender:Sender.t
  -> ?sender_args:Sender_argument.t list
  -> ?recipients:Email_address.t list
  -> unit
  -> t
