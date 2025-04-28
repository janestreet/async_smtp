open! Core
open Async
open Async_smtp_types

module Id : sig
  type t [@@deriving sexp_of]

  val to_string : t -> string

  include Hashable.S_plain with type t := t
  include Comparable.S_plain with type t := t
end

module Status : sig
  type t =
    [ `Send_now
    | `Send_at of Time_float.t
    | `Sending
    | `Frozen
    | `Removed
    | `Quarantined of Quarantine_reason.t
    | `Delivered
    ]
  [@@deriving sexp_of]
end

module Queue : sig
  type t =
    | Active
    | Frozen
    | Removed
    | Quarantine
  [@@deriving sexp, enumerate, compare]

  val to_dirname : t -> string
  val to_string : t -> string
  val of_status : Status.t -> t option
  val of_status' : Status.t -> t Or_error.t
end

(** [t] does not contain the full envelope. The envelope is loaded from disk and then
    stored back to disk only when changes need to be made. *)
type t [@@deriving compare, sexp_of]

val spool_dir : t -> string
val id : t -> Id.t
val flows : t -> Mail_log.Flows.t
val parent_id : t -> Smtp_envelope.Id.t
val related_ids : t -> Id.t list
val spool_date : t -> Time_float.t
val next_hop_choices : t -> Host_and_port.t list
val envelope_info : t -> Smtp_envelope.Info.t
val time_on_spool : t -> Time_float.Span.t
val status : t -> Status.t
val set_status : t -> Status.t -> unit

(** The head of this list is the time at which we should attempt a delivery after the next
    time a delivery fails. *)
val retry_intervals : t -> Smtp_envelope.Retry_interval.t list

val set_retry_intervals : t -> Smtp_envelope.Retry_interval.t list -> unit
val add_retry_intervals : t -> Smtp_envelope.Retry_interval.t list -> unit
val remaining_recipients : t -> Email_address.Stable.V1.t list
val set_remaining_recipients : t -> Email_address.t list -> unit

(** Currently not used, but saved to disk to aid in triaging frozen messages and failed
    deliveries. Addresses on this list will not be included in remaining_recipients, and
    would otherwise be lost to the ether. *)
val failed_recipients : t -> Email_address.Stable.V1.t list

val set_failed_recipients : t -> Email_address.t list -> unit
val move_failed_recipients_to_remaining_recipients : t -> unit
val relay_attempts : t -> (Time_float.t * Error.t) list
val add_relay_attempt : t -> Time_float.t * Error.t -> unit
val last_relay_attempt : t -> (Time_float.t * Error.t) option

(** [Data.t] is an on-disk [Email.t]. The type is abstract because we store a dot-encoded
    email on disk. *)
module Data : sig
  type t

  val to_email : t -> Email.t
  val of_email : Email.t -> t
  val load : string -> t Deferred.Or_error.t
  val save : ?temp_file:string -> t -> string -> unit Deferred.Or_error.t
end

val of_envelope_batch
  :  Smtp_envelope.Routed.Batch.t
  -> gen_id:(unit -> Id.t Deferred.Or_error.t)
  -> spool_dir:string
  -> spool_date:Time_float.t
  -> failed_recipients:Email_address.t list
  -> relay_attempts:(Time_float.t * Error.t) list
  -> parent_id:Smtp_envelope.Id.t
  -> set_related_ids:bool
  -> status:Status.t
  -> flows:Mail_log.Flows.t
  -> (t * Data.t * Smtp_envelope.Routed.t) list Deferred.Or_error.t

module On_disk :
  Multispool_intf.Spoolable.S
  with type Metadata.t = t
   and type Name_generator.t = Smtp_envelope.t
   and module Name_generator.Unique_name = Id
   and module Queue = Queue
   and module Data = Data

module On_disk_spool : Multispool_intf.S with module Spoolable := On_disk

module Stable : sig
  module Id : sig
    module V1 : sig
      include
        Stable_comparable.V1
        with type t = Id.t
        with type comparator_witness = Id.comparator_witness

      include Stringable.S with type t := t
    end
  end

  module V1 : sig
    type t [@@deriving sexp, bin_io]
  end

  module V2 : sig
    type t [@@deriving sexp, bin_io]
  end

  module V3 : sig
    type t [@@deriving sexp, bin_io]
  end

  module V4 : sig
    type nonrec t = t [@@deriving sexp, bin_io]

    val of_v3 : V3.t -> t
    val to_v3 : t -> V3.t
  end
end
