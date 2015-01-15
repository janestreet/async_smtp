open Core.Std
open Async.Std
open Types

type t

(* Lock the spool directory. Load all the files that are already present
   there. *)
val create
  :  config:Config.t
  -> send:(Envelope_with_next_hop.t -> unit Or_error.t Deferred.t)
  -> t Or_error.t Deferred.t

(* Write the message to disk.
   The list represents the different "sections" of one message
   We guarantee that the messages will be processed in the same order as they are in the
   list
*)
val add :
  t
  -> original_msg:Envelope.t
  -> Envelope_with_next_hop.t list
  -> Envelope.Id.t Or_error.t Deferred.t

val flush
  : ?timeout:unit Deferred.t
  -> t
  -> bool Deferred.t
