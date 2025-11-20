open! Core
open! Async
open Async_smtp_types

type t

(* Lock the spool directory. Load all the files that are already present there. *)

val create
  :  config:Server_config.t
  -> send:(Smtp_envelope.Routed.t -> unit Or_error.t Deferred.t)
  -> t Or_error.t Deferred.t

(* Write the message to disk. The list represents the different "sections" of one message
   We guarantee that the messages will be processed in the same order as they are in the
   list
*)

val add
  :  t
  -> original_msg:Smtp_envelope.t
  -> Smtp_envelope.Routed.t list
  -> Smtp_envelope.Id.t Or_error.t Deferred.t

val flush : ?timeout:unit Deferred.t -> t -> bool Deferred.t
