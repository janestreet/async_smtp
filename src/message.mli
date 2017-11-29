open! Core
open! Async
open Async_smtp_types

module Id : sig
  type t [@@deriving sexp_of]
  val to_string : t -> string
  include Hashable.S_plain with type t:=t
  include Comparable.S_plain with type t:=t
end

module Status : sig
  type t =
    [ `Send_now
    | `Send_at of Time.t
    | `Sending
    | `Frozen
    | `Removed
    | `Quarantined of Quarantine_reason.t
    | `Delivered
    ] [@@deriving sexp_of]
end

module Queue : sig
  type t =
    | Active
    | Frozen
    | Removed
    | Quarantine
  [@@deriving sexp, enumerate, compare]

  val to_dirname : t -> string
end

module On_disk_spool : sig
  type t
  type spool = t

  module Entry : sig
    type t [@@deriving sexp_of]

    val create : spool -> Queue.t -> name:string -> t
  end

  val load   : string -> t Deferred.Or_error.t
  val create : string -> t Deferred.Or_error.t
  val ls     : t -> Queue.t list -> Entry.t list Deferred.Or_error.t
end

(* Message.t does not contain the full envelope. The envelope is loaded from disk and then
   stored back to disk only when changes need to be made. *)
type t [@@deriving sexp_of]

(* Comparison is based on ids. *)
include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

val spool_date         : t -> Time.t
val last_relay_attempt : t -> (Time.t * Error.t) option
val parent_id          : t -> Smtp_envelope.Id.t
val id                 : t -> Id.t
val flows              : t -> Mail_log.Flows.t
val time_on_spool      : t -> Time.Span.t
val status             : t -> Status.t
val next_hop_choices   : t -> Smtp_socket_address.t list
val envelope_info      : t -> Smtp_envelope.Info.t

val size_of_file       : t -> Byte_units.t Or_error.t Deferred.t

(* The Deferreds become determined once the message has been synced to disk. *)
val create
  :  On_disk_spool.t
  -> log:Mail_log.t
  -> initial_status:Status.t (* should usually be [`Send_now]*)
  -> Smtp_envelope.Routed.t
  -> flows:Mail_log.Flows.t
  -> original_msg : Smtp_envelope.t
  -> t Or_error.t Deferred.t

val load : On_disk_spool.Entry.t -> t Or_error.t Deferred.t
val load_with_envelope : On_disk_spool.Entry.t -> (t * Smtp_envelope.t) Or_error.t Deferred.t

(* It is an error to call [send] on a message that is currently being sent or
   for which the call previously returned [`Done]. *)
val send : t -> log:Mail_log.t -> client_cache:Client_cache.t -> unit Or_error.t Deferred.t

(* Map an email that is saved on disk. *)
val map_email : t -> f:(Email.t -> Email.t) -> unit Or_error.t Deferred.t

val freeze
  :  t
  -> log:Mail_log.t
  -> unit Or_error.t Deferred.t

(* Change a message's status to [`Send_now].
   [retry_intervals] are added in front of the existing ones. *)
val mark_for_send_now
  :  retry_intervals : Smtp_envelope.Retry_interval.t list
  -> t
  -> log:Mail_log.t
  -> unit Or_error.t Deferred.t

val remove : t -> log:Mail_log.t -> unit Or_error.t Deferred.t

module On_disk : sig
  include Multispool_intf.Spoolable.S with module Queue = Queue
end

module On_disk_monitor : sig
  include Multispool_intf.Monitor.S with module Spoolable := On_disk
end

module Stable : sig
  module Id : sig
    module V1 : sig
      type t = Id.t [@@deriving sexp, bin_io]
      include Stringable.S with type t:=t
    end
  end
  module V1 : sig type t [@@deriving sexp, bin_io] end
  module V2 : sig
    type nonrec t = t [@@deriving sexp, bin_io]
    val of_v1 : V1.t -> t
  end
end
