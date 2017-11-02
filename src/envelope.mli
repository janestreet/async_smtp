open! Core
open Email_message

module Id : sig
  type t [@@deriving sexp_of]
  val to_string : t -> string
  val of_string : string -> t

  val create : unit -> t

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module Infoable : sig
  module type S = sig
    type t

    val sender              : t -> Sender.t
    val sender_args         : t -> Sender_argument.t list
    val string_sender       : t -> string
    val recipients          : t -> Email_address.t list
    val rejected_recipients : t -> Email_address.t list
    val string_recipients   : t -> string list
    val route               : t -> string option
    val id                  : t -> Id.t
  end
end

module Info : sig
  type t [@@deriving sexp_of]

  val create
    :  ?id:Id.t
    -> sender:Sender.t
    -> ?sender_args:Sender_argument.t list
    -> recipients:Email_address.t list
    -> ?rejected_recipients:Email_address.t list
    -> ?route:string
    -> unit
    -> t

  val set
    :  t
    -> ?sender:Sender.t
    -> ?sender_args:Sender_argument.t list
    -> ?recipients:Email_address.t list
    -> ?rejected_recipients:Email_address.t list
    -> ?route:string option
    -> unit
    -> t

  (* Extracts sender and recipients from the headers. *)
  val of_email : Email.t -> t Or_error.t

  include Infoable.S   with type t := t
  include Comparable.S_plain with type t := t
  include Hashable.S_plain   with type t := t
end

(* Two envelopes are equal when they produce the same SMTP output. In
   particular, ids are ignored for comparison. Same is true for hashing. *)
type t [@@deriving sexp_of]
type envelope = t [@@deriving sexp_of]

include Infoable.S   with type t := t
include Comparable.S_plain with type t := t
include Hashable.S_plain   with type t := t

val create
  :  ?id:Id.t
  -> sender:Sender.t
  -> ?sender_args:Sender_argument.t list
  -> recipients:Email_address.t list
  -> ?rejected_recipients:Email_address.t list
  -> ?route:string
  -> email:Email.t
  -> unit
  -> t

val create' : info:Info.t -> email:Email.t -> t

val info  : t -> Info.t
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

val set' : t -> ?info:Info.t -> ?email:Email.t -> unit -> t

(* Extracts sender and recipients from the headers. *)
val of_email : Email.t -> t Or_error.t

val last_header : ?whitespace:Email_headers.Whitespace.t -> t -> Email_headers.Name.t -> Email_headers.Value.t option
val find_all_headers : ?whitespace:Email_headers.Whitespace.t -> t -> Email_headers.Name.t -> Email_headers.Value.t list
(* See [Field_list.*] *)
val add_header : ?whitespace:Email_headers.Whitespace.t -> t -> name:string -> value:string -> t
val add_headers : ?whitespace:Email_headers.Whitespace.t -> t -> (string * string) list -> t
val set_header : ?whitespace:Email_headers.Whitespace.t -> t -> name:string -> value:string -> t
val add_header_at_bottom : ?whitespace:Email_headers.Whitespace.t -> t -> name:string -> value:string -> t
val add_headers_at_bottom : ?whitespace:Email_headers.Whitespace.t -> t -> (string * string) list -> t
val set_header_at_bottom : ?whitespace:Email_headers.Whitespace.t -> t -> name:string -> value:string -> t
val smash_and_add_header : ?whitespace:Email_headers.Whitespace.t -> t -> name:string -> value:string -> t

val modify_headers
  : t -> f:(Email_headers.t -> Email_headers.t) -> t
val filter_headers
  : ?whitespace:Email_headers.Whitespace.t -> t -> f:(name:Email_headers.Name.t -> value:Email_headers.Value.t -> bool) -> t
val map_headers
  : ?whitespace:Email_headers.Whitespace.t -> t -> f:(name:Email_headers.Name.t -> value:Email_headers.Value.t -> string) -> t

val modify_email
  : t -> f:(Email.t -> Email.t) -> t

module With_next_hop : sig
  type t =
    { envelope  : envelope
    (* Next hops to try. If the first one fails, we are done, otherwise try
       sending to the second one, etc. *)
    ; next_hop_choices : Address.t list
    ; retry_intervals  : Retry_interval.t list
    } [@@deriving fields, sexp_of]


  include Infoable.S         with type t := t
  include Comparable.S_plain with type t := t
  include Hashable.S_plain   with type t := t

  val create
    :  envelope : envelope
    -> next_hop_choices : Address.t list
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
end

module Stable : sig
  module Id : sig
    module V1 : sig type t = Id.t [@@deriving bin_io, sexp] end
  end

  module Info : sig
    module V1 : sig type t [@@deriving bin_io, sexp] end
    module V2 : sig
      type t = Info.t [@@deriving bin_io, sexp]
      val of_v1 : V1.t -> t
    end
  end

  module V1 : sig type t [@@deriving bin_io, sexp] end
  module V2 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
    val of_v1 : V1.t -> t
  end
end
