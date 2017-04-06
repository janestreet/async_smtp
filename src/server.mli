open! Core
open! Async

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
  -> Envelope.t Or_error.t Pipe.Reader.t

val read_mbox
  :  ?log:Mail_log.t
  -> Reader.t
  -> Envelope.t Or_error.t Pipe.Reader.t
