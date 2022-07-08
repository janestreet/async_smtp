open! Core
open! Async
open! Async_smtp

module Outbound_envelope : sig
  type t

  val recipients : t -> string list
end

module Inbound_envelope : sig
  type t

  val envelope_sender : t -> string option
  val envelope_recipients : t -> string list
  val data : t -> Smtp_mail_log.Mail_fingerprint.t option
  val outbound_envelopes : t -> Outbound_envelope.t list
end

type t [@@deriving sexp_of]

val compare : t -> t -> int

(** build up a [t] *)
val create : Smtp_mail_log.Flows.Id.t -> t

val update : t -> Smtp_mail_log.Message.t -> unit

(** get information from a [t] *)
val inbound_envelopes : t -> Inbound_envelope.t list

val session_connect : t -> Time_float.t option
val raw_messages : t -> Smtp_mail_log.Message.t list

module Summary : sig
  type t [@@deriving sexp_of]
end

val summary : t -> Summary.t
