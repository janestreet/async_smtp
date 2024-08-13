open! Core
open Email_message

module type With_headers = Envelope_container_intf.With_headers

module type With_info =
  Envelope_container_intf.With_info with type envelope_info := Envelope_info.t

module Make_with_headers (S : sig
    type t

    val headers : t -> Email_headers.t
    val set_headers : t -> Email_headers.t -> t
  end) : With_headers with type t := S.t

module Make_with_info (S : sig
    type t

    val info : t -> Envelope_info.t
  end) : With_info with type t := S.t
