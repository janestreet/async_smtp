(** Spool directory structure:

    Async_smtp uses a spool directory structure heavily inspired by that of Exim (see [1]
    and [2] for details on that). On startup, async_smtp takes out a lock on the spool
    directory (using the [Lock_file] module with the file $spool_dir/lock) and assumes
    that no other process will be manipulating the files or directories below it without
    using the async_smtp RPC interface.

    The lifetime of a message looks like this:

    When async_smtp accepts a message it immediately processes it, possibly expanding it
    to multiple additional messages (at least one per recipient). Each of the expanded
    messages are added to the spool (by calling [Spool.add]), writing it first to
    $spool_dir/tmp/$msgid (with a timestamp, as well as information about whether the
    message is frozen or not, the last relay attempt, and the parent message ID if any)
    and then renaming it to $spool_dir/active/$msgid (to minimize the chance of a message
    being sent multiple times in case of a crash).

    Newly spooled messages are also immediately written to a queue. A background loop
    iterates over this queue, processing and relaying messages in accordance with the
    [max_concurrent_send_jobs] configuration option. Async_smtp attempts to send each
    message in turn.

    On success, the message is removed from the active directory.

    On failure, the [last_relay_attempt_date] is immediately updated in the on disk spool
    file (again using tmp to make the change as atomic as possible). If the message has
    any remaining retry intervals in [envelope_routed.retry_intervals] then async_smtp
    schedules a retry for after the interval has elapsed (rewriting the spooled message
    with the interval popped off the list of remaining retry intervals only after the
    interval has elapsed). If there are no remaining retry intervals then the message is
    marked as frozen and moved into $spool_dir/frozen/$msgid (and no further attempts to
    send it are made).

    If async_smtp crashes (or is shutdown) and the spool has contents then it is reloaded
    as follows:

    If there are any contents of $spool_dir/tmp then async_smtp will refuse to start. Such
    messages indicate that mailcore died while changing a message on disk, which is a
    serious problem.

    The contents of $spool_dir/active are read in and re-queued based on the last
    attempted relay time and the remaining retry intervals as above.

    [1]
    http://www.exim.org/exim-html-current/doc/html/spec_html/ch-how_exim_receives_and_delivers_mail.html
    [2]
    http://www.exim.org/exim-html-current/doc/html/spec_html/ch-format_of_spool_files.html *)
open! Core

open! Async
open Async_smtp_types
module Config = Spool_config
module Message_id = Message.Id

type t

(** Lock the spool directory and load all the files that are already present there. Note
    that for the purposes of locking, the spool directory assumed to NOT be on an NFS file
    system. *)
val create
  :  ?presend:
       (log:Mail_log.t
        -> Message.t
        -> [< `Send_now | `Send_at of Time_float.t | `Freeze | `Remove ] Deferred.t)
       (** Immediately prior to sending a message, [presend] is called and can decide to
           delay it to a later time. Example use case is rate limiting outbound messages.
           default: `Send_now for all messages *)
  -> ?on_error:
       (log:Mail_log.t
        -> load_envelope:(unit -> Smtp_envelope.t Deferred.Or_error.t)
        -> Message.t
        -> Smtp_reply.t
        -> [< `Fail_permanently | `Try_later | `Try_later_rate_limited | `Done ]
             Deferred.Or_error.t)
       (** Upon receiving an error reply for a SMTP relay attempt, [on_error] is called to
           determine how to handle the error. This callback can make decisions based on
           the contents of the SMTP reply, the history of previous relay attempts, and
           other information. `Fail_permanently will cause the message to be frozen (saved
           on disk but not attempted again), while `Try_later will cause the message to be
           retried according to [Message.retry_intervals]. `Try_later_rate_limited is
           similar but indicates the retry is specifically due to rate limiting by the
           recipient server; the distinction between `Try_later and
           `Try_later_rate_limited is primarily observable in the spool-events stream's
           `Delayed and `Delayed_rate_limited events. `Done will cause the spool to treat
           the message as handled. `Done can be used for [on-error] to let the spool know
           that it has enqueued a bounce message to the sender of the failed message, and
           the spool should stop worrying about the failed message.

           Example use case is detecting and handling emails rate-limited or deemed
           malicious by the recipient email server.

           default: use [Smtp_reply.is_permanent_error] on the reply to choose between
           `Fail_permanently and `Try_later, sends no bounce messages *)
  -> config:Config.t
  -> log:Mail_log.t
  -> unit
  -> t Deferred.Or_error.t

(** Immediately write the message to disk and queue it for sending. The
    [Smtp_envelope.Routed.Batch.t list] represents the different "sections" of one
    message. We make no guarantees about the order of delivery of messages. *)
val add
  :  t
  -> ?initial_status:[ `Frozen | `Send_now ] (** default: `Send_now *)
  -> ?set_related_ids:bool
       (** Set the [related_ids] field for each message. Defaults to [false].

           If [true], [related_ids] will contain the IDs of all messages in the same
           batch. This allows tracking relationships between messages. *)
  -> flows:Mail_log.Flows.t
  -> original_msg:Smtp_envelope.t
  -> Smtp_envelope.Routed.Batch.t list
  -> (Message_id.t * Smtp_envelope.Routed.t) list Deferred.Or_error.t

(* Move the message into a special quarantine directory where it can be manually inspected
   and optionally manually injected back into the spool *)

val quarantine
  :  t
  -> reason:Quarantine_reason.t
  -> flows:Mail_log.Flows.t
  -> original_msg:Smtp_envelope.t
  -> Smtp_envelope.Routed.Batch.t list
  -> (Message_id.t * Smtp_envelope.Routed.t) list Deferred.Or_error.t

(** [kill_and_flush t] makes sure no new delivery sessions are being started and waits
    until all the currently running sessions have finished. It will not affect frozen
    messages or those waiting for retry intervals to elapse. *)
val kill_and_flush : ?timeout:unit Deferred.t -> t -> unit Deferred.Or_error.t

val freeze : t -> Message_id.t list -> unit Deferred.Or_error.t

module Send_info : sig
  type t =
    [ `All_messages
    | `Frozen_only
    | `Some_messages of Message_id.t list
    ]
end

val send
  :  ?retry_intervals:Smtp_envelope.Retry_interval.t list
  -> t
  -> Send_info.t
  -> unit Deferred.Or_error.t

(* removing a message will move it to spool's removed directory and remove it from the
   hash table so the spool has no awareness of it. It will not be shown in a status
   command. *)

val remove : t -> Message_id.t list -> unit Deferred.Or_error.t

module Recover_info : sig
  type t =
    { msgs : Message_id.t list
    ; from : [ `Removed | `Quarantined ]
    }
end

val recover : t -> Recover_info.t -> unit Deferred.Or_error.t

module Spooled_message_info : sig
  type t [@@deriving sexp_of]

  val id : t -> Message_id.t
  val spool_date : t -> Time_float.t
  val last_relay_attempt : t -> (Time_float.t * Error.t) option
  val parent_id : t -> Smtp_envelope.Id.t
  val envelope_info : t -> Smtp_envelope.Info.t
  val next_hop_choices : t -> Host_and_port.t list

  val status
    :  t
    -> [ `Send_now
       | `Send_at of Time_float.t
       | `Sending
       | `Frozen
       | `Removed
       | `Delivered
       | `Quarantined of Quarantine_reason.t
       ]

  (* These will not be populated for information obtained using [status]. Use
     [status_from_disk] if you want to see envelopes. Part of the reason is that we don't
     hold envelopes in memory, so we can return status much faster if we don't read the
     disk. A bigger part is that [status] is used to implement the rpc call, and we don't
     want the result to contain sensitive information. *)

  val file_size : t -> Byte_units.t option
  val envelope : t -> Smtp_envelope.t option
end

module Status : sig
  type t = Spooled_message_info.t list [@@deriving sexp_of]

  val to_formatted_string
    :  t
    -> format:[ `Ascii_table | `Ascii_table_with_max_width of int | `Exim | `Sexp | `Id ]
    -> string
end

val status : t -> Status.t
val get_message : t -> Message_id.t -> Message.t option

(** This is not necessarily a snapshot of the spool at any given point in time. The only
    way to obtain such a snapshot would be to pause the server and we don't want to do
    that. However, this status will include emails that are stuck on the spool, and those
    are the ones we care about.

    You should not try to work out the total number of unsent messages by counting the
    messages in the status. You should use the [count_from_disk] function instead. *)
val status_from_disk : Config.t -> Status.t Deferred.Or_error.t

val count_from_disk : Config.t -> int Or_error.t Deferred.t
val client_cache : t -> Client_cache.t

module Event : sig
  type spool_event =
    [ `Presend of [ `Send_now | `Send_at of Time_float.t | `Freeze | `Remove ]
    | `Spooled
    | `Delivered
    | `Frozen
    | `Removed
    | `Unfrozen
    | `Recovered of [ `From_quarantined | `From_removed ]
    | `Quarantined of [ `Reason of Quarantine_reason.t ]
    | `Delayed of Time_float.t
    | `Delayed_rate_limited of Time_float.t
    ]
    * Message_id.t
    * Smtp_envelope.Info.t
  [@@deriving sexp_of]

  type t = Time_float.t * [ `Spool_event of spool_event | `Ping ] [@@deriving sexp_of]

  include Comparable.S_plain with type t := t

  val to_string : t -> string
end

val event_stream : t -> Event.t Pipe.Reader.t

module Stable : sig
  module Message_id = Message.Stable.Id

  module Status : sig
    module V3 : sig
      type t = Status.t [@@deriving bin_io]

      include Streamable.S_rpc with type t := t
    end

    module V2 : sig
      type t [@@deriving bin_io]

      val to_v3 : t -> V3.t
      val of_v3 : V3.t -> t
    end
  end

  module Send_info : sig
    module V1 : sig
      type t = Send_info.t [@@deriving bin_io]
    end
  end

  module Recover_info : sig
    module V2 : sig
      type t = Recover_info.t [@@deriving bin_io]
    end
  end

  module Event : sig
    module V1 : sig
      type t = Event.t [@@deriving bin_io, sexp]
    end
  end
end
