open! Core
open! Async
open Async_smtp_types

type t = Message.On_disk_spool.t
type spool = t

val load   : string -> t Deferred.Or_error.t
val create : string -> t Deferred.Or_error.t

module Entry : sig
  type t [@@deriving sexp_of]

  val create : spool -> Message.Queue.t -> name:string -> t

  val to_message : t -> Message.t Or_error.t Deferred.t
  val to_message_with_envelope : t -> (Message.t * Smtp_envelope.t) Or_error.t Deferred.t
end

val ls : t -> Message.Queue.t list -> Entry.t list Or_error.t Deferred.t

val size_of_file : Message.t -> Byte_units.t Or_error.t Deferred.t

(** The Deferred becomes determined once the messages have been synced to disk. *)
val enqueue
  :  t
  -> log:Mail_log.t
  -> initial_status:Message.Status.t (** should usually be [`Send_now]*)
  -> Smtp_envelope.Routed.Batch.t
  -> flows:Mail_log.Flows.t
  -> original_msg : Smtp_envelope.t
  -> Message.t list Or_error.t Deferred.t

(** [send message ~log ~client_cache] attempts delivery of [message] using the supplied
    [client_cache] for connections.

    The outer [Error.t] in the result is used for unexpected errors.
    [`Delivered | `Failed of Error.t] describes the delivery attempt. *)
val send
  :  Message.t
  -> log:Mail_log.t
  -> client_cache:Client_cache.t
  -> [ `Delivered | `Failed of Error.t ] Or_error.t Deferred.t

(** Map an email that is saved on disk. *)
val map_email : Message.t -> f:(Email.t -> Email.t) -> unit Or_error.t Deferred.t

val freeze
  :  Message.t
  -> log:Mail_log.t
  -> unit Or_error.t Deferred.t

(** Change a message's status to [`Send_now]. [retry_intervals] are added in front of the
    existing ones. *)
val mark_for_send_now
  :  retry_intervals : Smtp_envelope.Retry_interval.t list
  -> Message.t
  -> log:Mail_log.t
  -> unit Or_error.t Deferred.t

val remove : Message.t -> log:Mail_log.t -> unit Or_error.t Deferred.t

module On_disk_monitor : sig
  include Multispool_intf.Monitor.S with module Spoolable := Message.On_disk
end
