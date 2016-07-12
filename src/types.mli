open! Core.Std
open! Async.Std
open Async_ssl.Std
open Email_message.Std

module Email_address = Email_address

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
    [ `Null
    | `Email of Email_address.t
    ] [@@deriving sexp, compare]

  val of_string : ?default_domain:string -> string -> t Or_error.t
  val to_string : t -> string

  val map : t -> f:(Email_address.t -> Email_address.t) -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  module Caseless : sig
    type nonrec t = t
    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end
end

module Envelope : sig
  module Id : sig
    type t [@@deriving sexp, bin_io]
    val to_string : t -> string
    val of_string : string -> t

    val create : unit -> t

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  (* Two envelopes are equal when they produce the same SMTP output. In
     particular, ids are ignored for comparison. Same is true for hashing. *)
  type t [@@deriving sexp, bin_io, compare]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val create :
    ?id:Id.t
    -> sender:Sender.t
    -> recipients:Email_address.t list
    -> ?rejected_recipients:Email_address.t list
    -> email:Email.t
    -> unit
    -> t

  val set
    :  t
    -> ?sender:Sender.t
    -> ?recipients:Email_address.t list
    -> ?rejected_recipients:Email_address.t list
    -> ?email:Email.t
    -> unit
    -> t

  (* Extracts sender and recipients from the headers. *)
  val of_email : Email.t -> t Or_error.t

  val sender            : t -> Sender.t
  val string_sender     : t -> string
  val recipients        : t -> Email_address.t list
  val rejected_recipients : t -> Email_address.t list
  val string_recipients : t -> string list
  val email             : t -> Email.t
  val id                : t -> Id.t

  val last_header : ?whitespace:Email_headers.Whitespace.t -> t -> Email_headers.Name.t -> Email_headers.Value.t option
  val find_all_headers : ?whitespace:Email_headers.Whitespace.t -> t -> Email_headers.Name.t -> Email_headers.Value.t list
  (* See [Field_list.*] *)
  val add_header : ?whitespace:Email_headers.Whitespace.t -> t -> name:string -> value:string -> t
  val add_headers : ?whitespace:Email_headers.Whitespace.t -> t -> (string * string) list -> t
  val set_header : ?whitespace:Email_headers.Whitespace.t -> t -> name:string -> value:string -> t
  val add_header_at_bottom : ?whitespace:Email_headers.Whitespace.t -> t -> name:string -> value:string -> t
  val add_headers_at_bottom : ?whitespace:Email_headers.Whitespace.t -> t -> (string * string) list -> t
  val set_header_at_bottom : ?whitespace:Email_headers.Whitespace.t -> t -> name:string -> value:string -> t

  val modify_headers
    : t -> f:(Email_headers.t -> Email_headers.t) -> t
  val filter_headers
    : ?whitespace:Email_headers.Whitespace.t -> t -> f:(name:Email_headers.Name.t -> value:Email_headers.Value.t -> bool) -> t
  val map_headers
    : ?whitespace:Email_headers.Whitespace.t -> t -> f:(name:Email_headers.Name.t -> value:Email_headers.Value.t -> string) -> t

  val modify_email
    : t -> f:(Email.t -> Email.t) -> t
end

module Address : sig
  type t = [`Inet of Host_and_port.t | `Unix of string] [@@deriving sexp, compare, bin_io]

  include Stringable.S with type t := t
end

module Envelope_with_next_hop : sig
  type t =
    { envelope  : Envelope.t
    (* Next hops to try. If the first one fails, we are done, otherwise try
       sending to the second one, etc. *)
    ; next_hop_choices : Address.t list
    ; retry_intervals  : Time.Span.t list
    } [@@deriving fields, sexp, bin_io, compare]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val create
    :  envelope : Envelope.t
    -> next_hop_choices : Address.t list
    -> retry_intervals : Time.Span.t list
    -> t

  (* Accessors for the message at [t.message]. *)
  val sender            : t -> Sender.t
  val string_sender     : t -> string
  val recipients        : t -> Email_address.t list
  val string_recipients : t -> string list
  val email             : t -> Email.t
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
    { remote : Address.t
    ; local : Address.t
    ; helo : string option
    ; tls : Ssl.Connection.t option
    } [@@deriving sexp_of, fields]

  val create
    :  remote : Address.t
    -> local : Address.t
    -> ?helo : string
    -> ?tls : Ssl.Connection.t
    -> unit
    -> t

  val cleanup : t -> unit Deferred.Or_error.t
end

module Command : sig
  type t =
    | Hello of string
    | Extended_hello of string
    | Sender of string
    | Recipient of string
    | Data
    | Reset
    | Quit
    | Help
    | Noop
    | Start_tls
  val to_string : t -> string
  val of_string : string -> t
end

module Reply : sig

  type t = private
    { code :
        [ `Service_ready_220
        | `Closing_connection_221
        | `Ok_completed_250
        | `Start_mail_input_354
        | `Service_unavailable_421
        | `Local_error_451
        | `Message_rate_exceeded_452
        | `Tls_temporarily_unavailable_454
        | `Unable_to_accommodate_455
        | `Command_not_recognized_500
        | `Syntax_error_501
        | `Command_not_implemented_502
        | `Bad_sequence_of_commands_503
        | `Parameter_not_implemented_504
        | `Mailbox_unavailable_550
        | `Exceeded_storage_allocation_552
        | `Transaction_failed_554
        | `From_to_parameters_bad_555
        | `Other of int
        ]
    ; raw_message : string list
    } [@@deriving sexp]

  val code : t -> int

  val service_ready_220 : string -> t
  val closing_connection_221 : t
  val ok_completed_250 : string -> t
  val start_mail_input_354 : t
  val service_unavailable_421 : t
  val local_error_451 : string -> t
  val message_rate_exceeded_452 : t
  val unable_to_accommodate_455 : string -> t
  val command_not_recognized_500 : string -> t
  val syntax_error_501 : string -> t
  val command_not_implemented_502 : Command.t -> t
  val bad_sequence_of_commands_503 : Command.t -> t
  val mailbox_unavailable_550 : string -> t
  val exceeded_storage_allocation_552 : t
  val transaction_failed_554 : string -> t
  val from_to_parameters_bad_555 : string -> t

  val is_ok : t -> bool
  val is_permanent_error : t -> bool

  (* No roundtrip. *)
  val to_string    : t -> string
  val of_string    : string -> t
  val of_bigstring : Bigstring.t -> t

  type partial
  val parse : ?partial:partial -> string -> [`Done of t | `Partial of partial]
end

module Extension : sig
  type t =
    | Start_tls
    | Other of string
  [@@deriving sexp]

  val of_string : string -> t
  val to_string : t -> string
end
