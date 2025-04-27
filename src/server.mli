open! Core
open! Async
open Async_smtp_types
module Config = Server_config
module Plugin = Server_plugin_intf

module type S = sig
  type server_state
  type t

  val start
    :  server_state:server_state
    -> config:Config.t
    -> log:Mail_log.t
    -> t Deferred.Or_error.t

  val config : t -> Config.t
  val ports : t -> int list
  val close : ?timeout:unit Deferred.t -> t -> unit Deferred.Or_error.t
end

module Make (P : Plugin.S) : S with type server_state := P.State.t

(* Read messages from a bsmtp session transcript. *)

val read_bsmtp : ?log:Mail_log.t -> Reader.t -> Smtp_envelope.t Or_error.t Pipe.Reader.t

module type For_test = sig
  type server_state

  val session
    :  server_state:server_state
    -> log:Mail_log.t
    -> ?max_message_size:Byte_units.t
    -> ?timeouts:Config.Timeouts.t
    -> ?tls_options:Config.Tls_options.t
    -> ?emulate_tls:bool
    -> ?malformed_emails:[ `Reject | `Wrap ]
    -> ?local_ip_address:Socket.Address.Inet.t
    -> ?remote_ip_address:Socket.Address.Inet.t
    -> Reader.t
    -> Writer.t
    -> unit Deferred.t
end

module For_test (P : Plugin.S) : For_test with type server_state := P.State.t
