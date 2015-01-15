open Core.Std
open Async.Std
open Types

module Id : Identifiable

module Status : sig
  type t = [ `Waiting | `Sending | `Frozen | `Delivered ] with sexp, bin_io
end

(* Spooled_message.t does not contain the full envelope. The envelope is loaded
   from disk and then stored back to disk only when changes need to be made. *)
type t with sexp, bin_io

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

(* The deferreds become determined once the message has been synced to disk. *)

val create
  :  Spool_directory.t
  -> Envelope_with_next_hop.t
  -> original_msg : Envelope.t
  -> t Or_error.t Deferred.t

val load
  : string -> t Or_error.t Deferred.t
val load_with_envelope
  : string -> (t * Envelope.t) Or_error.t Deferred.t

(* It is an error to call [send] on a message that is currently being sent or
   for which the call previously returned [`Done]. *)
val send
  :  t
  -> [ `Done
     | `Retry_at of Time.t
     | `Give_up
     ] Or_error.t Deferred.t

val next_action
  :  t
  -> [ `Send_at of Time.t
     | `Send_now
     | `Frozen ]

val freeze : t -> unit Or_error.t Deferred.t

(* [new_retry_intervals] are added in front of the existing ones. *)
val unfreeze
  :  new_retry_intervals : Time.Span.t list
  -> t
  -> unit Or_error.t Deferred.t
