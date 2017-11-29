open! Core
open Email_message

module type With_headers = sig
  type t

  val headers : t -> Email_headers.t

  val last_header
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> Email_headers.Name.t
    -> Email_headers.Value.t option

  val find_all_headers
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> Email_headers.Name.t
    -> Email_headers.Value.t list

  val add_header
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> name:string
    -> value:string
    -> t

  val add_headers
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> (string * string) list
    -> t

  val set_header
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> name:string
    -> value:string
    -> t

  val add_header_at_bottom
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> name:string
    -> value:string
    -> t

  val add_headers_at_bottom
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> (string * string) list
    -> t

  val set_headers : t -> Email_headers.t -> t

  val set_header_at_bottom
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> name:string
    -> value:string
    -> t

  val smash_and_add_header
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> name:string
    -> value:string
    -> t

  val modify_headers : t -> f:(Email_headers.t -> Email_headers.t) -> t

  val filter_headers
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> f:(name:Email_headers.Name.t -> value:Email_headers.Value.t -> bool)
    -> t

  val map_headers
    :  ?whitespace:Email_headers.Whitespace.t
    -> t
    -> f:(name:Email_headers.Name.t -> value:Email_headers.Value.t -> string)
    -> t
end

module type With_info = sig
  type t
  type envelope_info

  val sender              : t -> Sender.t
  val sender_args         : t -> Sender_argument.t list
  val string_sender       : t -> string
  val recipients          : t -> Email_address.t list
  val rejected_recipients : t -> Email_address.t list
  val string_recipients   : t -> string list
  val route               : t -> string option
  val id                  : t -> Envelope_id.t
  val info                : t -> envelope_info
end
