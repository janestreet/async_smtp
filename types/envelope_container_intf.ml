open! Core
open Email_message

module type With_headers = sig
  type t

  val headers : t -> Email_headers.t

  val last_header
    :  ?normalize:Email_headers.Normalize.decode
    -> t
    -> Email_headers.Name.t
    -> Email_headers.Value.t option

  val find_all_headers
    :  ?normalize:Email_headers.Normalize.decode
    -> t
    -> Email_headers.Name.t
    -> Email_headers.Value.t list

  val add_header
    :  ?normalize:Email_headers.Normalize.encode
    -> t
    -> name:string
    -> value:string
    -> t

  val add_headers
    :  ?normalize:Email_headers.Normalize.encode
    -> t
    -> (string * string) list
    -> t

  val set_header
    :  ?normalize:Email_headers.Normalize.encode
    -> t
    -> name:string
    -> value:string
    -> t

  val add_header_at_bottom
    :  ?normalize:Email_headers.Normalize.encode
    -> t
    -> name:string
    -> value:string
    -> t

  val add_headers_at_bottom
    :  ?normalize:Email_headers.Normalize.encode
    -> t
    -> (string * string) list
    -> t

  val set_headers : t -> Email_headers.t -> t

  val set_header_at_bottom
    :  ?normalize:Email_headers.Normalize.encode
    -> t
    -> name:string
    -> value:string
    -> t

  val smash_and_add_header
    :  ?normalize:Email_headers.Normalize.encode
    -> t
    -> name:string
    -> value:string
    -> t

  val modify_headers : t -> f:(Email_headers.t -> Email_headers.t) -> t

  val filter_headers
    :  ?normalize:Email_headers.Normalize.decode
    -> t
    -> f:(name:Email_headers.Name.t -> value:Email_headers.Value.t -> bool)
    -> t

  val map_headers
    :  ?normalize:Email_headers.Normalize.decode
    -> t
    -> f:(name:Email_headers.Name.t -> value:Email_headers.Value.t -> string)
    -> t

  (** Equivalent to
      [last_header t "Subject" ~normalize:(`Whitespace_and_encoding (`Any_charset, `Pretend_all_charsets_are_same))] *)
  val subject_decoded : t -> string option
end

module type With_info = sig
  type t
  type envelope_info

  val sender : t -> Sender.t
  val sender_args : t -> Sender_argument.t list
  val string_sender : t -> string
  val recipients : t -> Email_address.t list
  val rejected_recipients : t -> Email_address.t list
  val string_recipients : t -> string list
  val route : t -> string option
  val id : t -> Envelope_id.t
  val info : t -> envelope_info
end
