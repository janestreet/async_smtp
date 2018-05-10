open! Core
open! Async
open Async_smtp_types

module Config = Server_config
module Plugin = Server_plugin

module type S = sig
  type t

  val start : config:Config.t -> log:Mail_log.t -> t Deferred.Or_error.t

  val config : t -> Config.t
  val ports  : t -> int list

  val close  : ?timeout:unit Deferred.t -> t -> unit Deferred.Or_error.t
end

module Make(P : Plugin.S) : S

(* Read messages from a bsmtp session transcript. *)
val read_bsmtp
  :  ?log:Mail_log.t
  -> Reader.t
  -> Smtp_envelope.t Or_error.t Pipe.Reader.t

val read_mbox
  :  ?log:Mail_log.t
  -> Reader.t
  -> Smtp_envelope.t Or_error.t Pipe.Reader.t

module type For_test = sig
  val session
    (** [send] and [quarantine] default to raising an exception *)
    :  ?send:(Smtp_envelope.Routed.Batch.t list -> string Deferred.Or_error.t)
    -> ?quarantine:(
      reason:Quarantine_reason.t
      -> Smtp_envelope.Routed.Batch.t list
      -> unit Deferred.Or_error.t)
    -> log:Mail_log.t
    -> ?max_message_size:Byte_units.t
    -> ?tls_options:Config.Tls_options.t
    -> ?emulate_tls:bool
    -> ?malformed_emails:[`Reject|`Wrap]
    -> ?local:Smtp_socket_address.t
    -> remote:Smtp_socket_address.t
    -> Reader.t
    -> Writer.t
    -> unit Deferred.t
end

module For_test(P : Plugin.S) : For_test
