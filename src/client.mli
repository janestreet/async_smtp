open! Core
open! Async
open Async_smtp_types

(** SMTP client API. Includes TLS support: http://tools.ietf.org/html/rfc3207

    Client.t should only be used in the callback of [Tcp.with_]. The behaviour outside of
    this functions is undefined, though its safe to assume that nothing good will come of
    such misuse.

    Several of the with functions also change the underlying Readers/Writers in a
    non-revertable way. In general if a reader/writer are passed to a function, ownership
    is also transferred; similarly if an smtp client is passed to a with* function it
    should not be used thereafter.

    This client logs aggressively; while this produces a lot of garbage it is extremely
    helpful when debugging issues down the line. The client includes a [session_id] that
    can be set when creating the client. The [session_id] is extended with additional
    information about the client state.

    See [Client_raw] if you need a lower-level interface. *)

module Peer_info : sig
  type t

  val greeting : t -> string option

  val hello
    :  t
    -> [ `Simple of string | `Extended of string * Smtp_extension.t list ] option

  val supports_extension : t -> Smtp_extension.t -> bool
end

type t = Client_raw.t

val is_using_tls : t -> bool

module Envelope_status : sig
  type envelope_id = string
  type rejected_recipients = (Email_address.t * Smtp_reply.t) list
  type ok = envelope_id * rejected_recipients

  type err =
    [ `Rejected_sender of Smtp_reply.t
    | `Rejected_all_recipients of rejected_recipients
    | `Rejected_sender_and_recipients of Smtp_reply.t * rejected_recipients
    | `Rejected_body of Smtp_reply.t * rejected_recipients
    ]
  [@@deriving sexp_of]

  type t = (ok, err) Result.t [@@deriving sexp_of]

  val to_string : t -> string
  val ok_or_error : allow_rejected_recipients:bool -> t -> string Or_error.t
  val ok_exn : allow_rejected_recipients:bool -> t -> string
end

(** Perform all required commands to send an SMTP evelope *)
val send_envelope
  :  t
  -> log:Mail_log.t
  -> ?flows:Mail_log.Flows.t
  -> ?component:Mail_log.Component.t
  -> Smtp_envelope.t
  -> Envelope_status.t Deferred.Or_error.t

(** Standard SMTP over tcp *)
module Tcp : sig
  (** Establish a connection to the given address and perform the appropriate SMTP
      handshake. Use [send_envelope] to (attempt) to deliver messages. *)
  val with_
    : (?config:Client_config.t
       -> ?credentials:Credentials.t (** default: [Credentials.Anon] *)
       -> log:Mail_log.t
       -> ?flows:Mail_log.Flows.t
       -> ?component:Mail_log.Component.t
       -> Host_and_port.t
       -> f:(t -> 'a Deferred.Or_error.t)
       -> 'a Deferred.Or_error.t)
        Tcp.Aliases.with_connect_options
end

module Expert : sig
  val send_envelope
    :  t
    -> log:Mail_log.t
    -> ?flows:Mail_log.Flows.t
    -> ?component:Mail_log.Component.t
         (** Send the raw data of the [Email.t].

             [send_data] must:
             - not include a trailing "\r\n.\r\n"
             - dot escape the content
             - use \r\n as the end of line marker *)
    -> ?spool_date:Time_float.t
         (** The time when this message was spooled. If provided, a
             "time-on-spool-seconds" tag will be added to the "sending" and "sent" log
             lines. *)
    -> send_data:(t -> unit Deferred.Or_error.t)
    -> Smtp_envelope.Info.t
    -> Envelope_status.t Deferred.Or_error.t
end

module For_test : sig
  val with_
    :  ?config:Client_config.t
    -> ?credentials:Credentials.t
    -> log:Mail_log.t
    -> ?flows:Mail_log.Flows.t
    -> ?component:Mail_log.Component.t
    -> ?emulate_tls:bool
    -> ?local_ip_address:Socket.Address.Inet.t
    -> ?remote_ip_address:Socket.Address.Inet.t
    -> remote_address:Host_and_port.t
    -> Reader.t
    -> Writer.t
    -> f:(t -> 'a Deferred.Or_error.t)
    -> 'a Deferred.Or_error.t
end

(** BSMTP writing *)
module Bsmtp : sig
  val write
    :  ?skip_prelude_and_prologue:bool
    -> ?log:Mail_log.t (* = Mail_log.quiet *)
    -> ?flows:Mail_log.Flows.t
    -> ?component:Mail_log.Component.t
    -> Writer.t
    -> Smtp_envelope.t Pipe.Reader.t
    -> unit Deferred.Or_error.t

  val to_string
    :  ?skip_prelude_and_prologue:bool
    -> ?log:Mail_log.t (* = Mail_log.quiet *)
    -> ?flows:Mail_log.Flows.t
    -> ?component:Mail_log.Component.t
    -> Smtp_envelope.t list
    -> string Deferred.Or_error.t
end
