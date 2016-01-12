open Core.Std
open Async.Std
open Types

module Id : Identifiable

module Status : sig
  type t =
    [ `Send_now
    | `Send_at of Time.t
    | `Sending
    | `Frozen
    | `Removed
    | `Quarantined of string
    | `Delivered
    ] [@@deriving sexp, bin_io]
end

(* Spooled_message.t does not contain the full envelope. The envelope is loaded
   from disk and then stored back to disk only when changes need to be made. *)
type t [@@deriving sexp, bin_io]

(* Comparison is based on ids. *)
include Comparable.S with type t := t
include Hashable.S with type t := t

val spool_date         : t -> Time.t
val last_relay_attempt : t -> (Time.t * Error.t) option
val parent_id          : t -> Envelope.Id.t
val id                 : t -> Id.t
val time_on_spool      : t -> Time.Span.t
val status             : t -> Status.t
val next_hop_choices   : t -> Host_and_port.t list

val size_of_file       : t -> Byte_units.t Or_error.t Deferred.t

(* The Deferreds become determined once the message has been synced to disk. *)
val create
  :  Spool_directory.t
  -> initial_status:Status.t (* should usually be [`Send_now]*)
  -> Envelope_with_next_hop.t
  -> original_msg : Envelope.t
  -> t Or_error.t Deferred.t

val load : string -> t Or_error.t Deferred.t
val load_with_envelope : string -> (t * Envelope.t) Or_error.t Deferred.t

(* It is an error to call [send] on a message that is currently being sent or
   for which the call previously returned [`Done]. *)
val send : t -> config:Client_config.t -> unit Or_error.t Deferred.t

val freeze : t -> unit Or_error.t Deferred.t

(* Change a message's status to [`Send_now].
   [retry_intervals] are added in front of the existing ones. *)
val mark_for_send_now
  :  retry_intervals : Time.Span.t list
  -> t
  -> unit Or_error.t Deferred.t

val remove : t -> unit Or_error.t Deferred.t
