open! Core
open! Async
open Async_smtp_types

type t = Message.On_disk_spool.t
type spool = t

val load : string -> t Deferred.Or_error.t
val create : string -> t Deferred.Or_error.t

module Entry : sig
  type t [@@deriving sexp_of]

  val create : spool -> Message.Queue.t -> name:string -> t
  val to_message : t -> Message.t Or_error.t Deferred.t
  val to_message_with_envelope : t -> (Message.t * Smtp_envelope.t) Or_error.t Deferred.t
  val size : t -> Byte_units.t Or_error.t Deferred.t
end

(** Move all checked out entries back to their original queues. This command is useful to
    run on startup to recover from an unclean shutdown. Returns the list of entries that
    were recovered along with any errors that occurred. *)
val uncheckout_all_entries
  :  t
  -> ([ `Recovered of string list ] * [ `Errors of Error.t option ]) Deferred.t

val ls : t -> Message.Queue.t list -> Entry.t list Or_error.t Deferred.t

(** The Deferred becomes determined once the messages have been synced to disk. *)
val enqueue
  :  t
  -> log:Mail_log.t
  -> initial_status:Message.Status.t (** should usually be [`Send_now] *)
  -> set_related_ids:bool (** Set related ids to be the set of messages in this batch. *)
  -> Smtp_envelope.Routed.Batch.t
  -> flows:Mail_log.Flows.t
  -> original_msg:Smtp_envelope.t
  -> (Message.t * Smtp_envelope.Routed.t) list Or_error.t Deferred.t

(** [send message ~log ~client_cache] attempts delivery of [message] using the supplied
    [client_cache] for connections.

    The outer [Error.t] in the result is used for unexpected errors. ([...] * Error.t
    option) describes the delivery attempt. *)
val send
  :  Message.t
  -> log:Mail_log.t
  -> client_cache:Client_cache.t
  -> presend:
       (log:Mail_log.t
        -> [ `Send_now | `Send_at of Time_float.t | `Freeze | `Remove ] Deferred.t)
  -> on_error:
       (log:Mail_log.t
        -> load_envelope:(unit -> Smtp_envelope.t Deferred.Or_error.t)
        -> Smtp_reply.t
        -> [ `Fail_permanently | `Try_later | `Try_later_rate_limited | `Done ] Deferred.t)
  -> ([> `Delivered
      | `Frozen
      | `Removed
      | `Delayed_to of Time_float.t
      | `Delayed_to_rate_limited of Time_float.t
      ]
     * Error.t option)
       Or_error.t
       Deferred.t

val freeze : Message.t -> log:Mail_log.t -> unit Or_error.t Deferred.t

(** Change a message's status to [`Send_now]. [retry_intervals] are added in front of the
    existing ones. *)
val mark_for_send_now
  :  retry_intervals:Smtp_envelope.Retry_interval.t list
  -> Message.t
  -> log:Mail_log.t
  -> unit Or_error.t Deferred.t

val mark_for_send_at
  :  Message.t
  -> at:Time_float.t
  -> log:Mail_log.t
  -> unit Or_error.t Deferred.t

val remove : Message.t -> log:Mail_log.t -> unit Or_error.t Deferred.t

module On_disk_monitor : sig
  include Multispool_intf.Monitor.S with module Spoolable := Message.On_disk
end
