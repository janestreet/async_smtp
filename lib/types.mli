open Core.Std
open Async.Std

module Email_address : sig
  (* Hash and comparisons are based on the address part (local_part + domain)
     only. *)
  type t with bin_io, sexp, compare

  (* Case-insensitive. *)
  module Domain : Email_message.Mimestring.S

  val of_string : ?default_domain:string -> string -> t Or_error.t
  val of_string_exn : ?default_domain:string -> string -> t
  val list_of_string : ?default_domain:string -> string -> t list Or_error.t
  val list_of_string_exn : ?default_domain:string -> string -> t list
  val to_string : t -> string
  val list_to_header_value : t list -> string

  val local_part : t -> string

  val domain : t -> Domain.t option

  (* local@domain, default brackets = false *)
  val address_part
    : ?brackets:bool -> ?lowercase_domain:bool -> t -> t
  val address_part_string
    : ?brackets:bool -> ?lowercase_domain:bool -> t -> string
  (* Expects address part without brackets. *)
  val set_address_part
    : t -> string -> t Or_error.t

  (* Setting prefix to none removes the angular brackets. *)
  val set_prefix : t -> string option -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

(*
   From 3.7 Relaying:

   One way to prevent loops in error reporting is to specify a null reverse-path
   in the MAIL command of a notification message.  When such a message is
   transmitted the reverse-path MUST be set to null (see section 4.5.5 for
   additional discussion).  A MAIL command with a null reverse-path appears as
   follows:

   MAIL FROM:<>
*)
module Sender : sig
  type t =
    | Null
    | Email of Email_address.t
  with sexp, compare

  val of_string : ?default_domain:string -> string -> t Or_error.t
  val to_string : t -> string

  val map : t -> f:(Email_address.t -> Email_address.t) -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Envelope : sig
  module Id : sig
    type t with sexp, bin_io
    val to_string : t -> string
    val of_string : string -> t

    val create : unit -> t

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  (* Two envelopes are equal when they produce the same SMTP output. In
     particular, ids are ignored for comparison. Same is true for hashing. *)
  type t with sexp, bin_io, compare

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val create :
    ?id:Id.t
    -> sender:Sender.t
    -> recipients:Email_address.t list
    -> ?rejected_recipients:Email_address.t list
    -> email:Email_message.Email.t
    -> unit
    -> t

  val set
    :  t
    -> ?sender:Sender.t
    -> ?recipients:Email_address.t list
    -> ?rejected_recipients:Email_address.t list
    -> ?email:Email_message.Email.t
    -> unit
    -> t

  (* Extracts sender and recipients from the headers. *)
  val of_email : Email_message.Email.t -> t Or_error.t

  val sender            : t -> Sender.t
  val string_sender     : t -> string
  val recipients        : t -> Email_address.t list
  val rejected_recipients : t -> Email_address.t list
  val string_recipients : t -> string list
  val email             : t -> Email_message.Email.t
  val id                : t -> Id.t

  (* Header names are case-insensitive. *)
  val get_headers : t -> name:string -> string list

  (* See [Field_list.*] *)
  val add_header : t -> name:string -> value:string -> t
  val set_header : t -> name:string -> value:string -> t
  val add_header_at_bottom : t -> name:string -> value:string -> t
  val set_header_at_bottom : t -> name:string -> value:string -> t

  val modify_headers
    : t -> f:(Email_message.Headers.t -> Email_message.Headers.t) -> t
  val filter_headers
    : t -> f:(name:Email_message.Field_name.t -> value:string -> bool) -> t

  val modify_email
    : t -> f:(Email_message.Email.t -> Email_message.Email.t) -> t
end

module Envelope_with_next_hop : sig
  type t =
    { envelope  : Envelope.t
    (* Next hops to try. If the first one fails, we are done, otherwise try
       sending to the second one, etc. *)
    ; next_hop_choices : Host_and_port.t list
    ; retry_intervals  : Time.Span.t list
    } with fields, sexp, bin_io, compare

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val create
    :  envelope : Envelope.t
    -> next_hop_choices : Host_and_port.t list
    -> retry_intervals : Time.Span.t list
    -> t

  (* Accessors for the message at [t.message]. *)
  val sender            : t -> Sender.t
  val string_sender     : t -> string
  val recipients        : t -> Email_address.t list
  val string_recipients : t -> string list
  val email             : t -> Email_message.Email.t
  val id                : t -> Envelope.Id.t

  val set
    :  t
    -> ?sender:Sender.t
    -> ?recipients:Email_address.t list
    -> unit
    -> t
end

module Session : sig
  type t =
    { id : string
    ; remote : Host_and_port.t
    ; local : Host_and_port.t
    ; helo : string option
    } with sexp, fields

  val create
    :  ?id:string
    -> remote:Host_and_port.t
    -> local:Host_and_port.t
    -> ?helo:string
    -> unit
    -> t
end

module Command : sig
  include (module type of Comm)
  val commands : string list
  val to_string : t -> string
  val of_string_opt: string -> t option
end

module Reply : sig
  type forward_path = string

  type t =
    (* Ok *)
    | System_status_211 of string
    | Help_214
    | Service_ready_220 of string
    | Closing_connection_221
    | Ok_completed_250 of string
    | Will_forward_251 of forward_path
    | Will_attempt_252
    | Start_mail_input_354

    (* Transient Errors *)
    | Service_unavailable_421
    | Mailbox_unavailable_450 of string
    | Local_error_451 of string
    | Insufficient_storage_452
    | Unable_to_accommodate_455 of string

    (* Permanent Errors *)
    | Command_not_recognized_500 of string
    | Syntax_error_501 of string
    | Command_not_implemented_502 of string
    | Bad_sequence_of_commands_503 of string
    | Parameter_not_implemented_504 of string
    | Mailbox_unavailable_550 of string
    | User_not_local_551 of string
    | Exceeded_storage_allocation_552
    | Mailbox_name_not_allowed_553 of string
    | Transaction_failed_554 of string
    | From_to_parameters_bad_555 of string
  with sexp

  val code : t -> int
  val is_ok : t -> bool
  val is_permanent_error : t -> bool

  (* No roundtrip. *)
  val to_string    : t -> string
  val of_string    : string -> t
  val of_bigstring : Bigstring.t -> t
end
