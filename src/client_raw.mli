open! Core
open! Async
open Async_smtp_types


module Peer_info : sig
  type t

  val greeting : t -> string option
  val hello
    :  t
    -> [`Simple of string
       | `Extended of string * Smtp_extension.t list
       ] option
  val supports_extension : t -> Smtp_extension.t -> bool
  val dest : t -> Smtp_socket_address.t
end

type t

val create
  :  ?flows:Mail_log.Flows.t
  -> emulate_tls_for_test:bool
  -> dest:Smtp_socket_address.t
  -> Reader.t
  -> Writer.t
  -> Client_config.t
  -> t

val create_bsmtp
  :  ?flows:Mail_log.Flows.t
  -> Writer.t
  (* Config must be set to not use TLS. *)
  -> Client_config.t
  -> t

val config : t -> Client_config.t
val info : t -> Peer_info.t option

val is_using_tls : t -> bool

val with_session
  :  t
  -> log:Mail_log.t
  -> component:Mail_log.Component.t
  -> credentials:Credentials.t
  -> f:(t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.Or_error.t

val do_helo
  :  t
  -> log:Mail_log.t
  -> component:Mail_log.Component.t
  -> unit Deferred.Or_error.t

val send
  :  t
  -> log:Mail_log.t
  -> ?flows:Mail_log.Flows.t
  -> component:Mail_log.Component.t
  -> here:Lexing.position
  -> Smtp_command.t
  -> unit Deferred.Or_error.t

val receive
  :  ?on_eof:(?partial:Smtp_reply.partial -> unit -> Smtp_reply.t Deferred.Or_error.t)
  -> ?timeout:Time.Span.t
  -> ?flows:Mail_log.Flows.t
  -> t
  -> log:Mail_log.t
  -> component:Mail_log.Component.t
  -> here:Lexing.position
  -> [ `Bsmtp | `Received of Smtp_reply.t] Deferred.Or_error.t

val send_receive
  :  ?on_eof:(?partial:Smtp_reply.partial -> unit -> Smtp_reply.t Deferred.Or_error.t)
  -> ?timeout:Time.Span.t
  -> t
  -> log:Mail_log.t
  -> ?flows:Mail_log.Flows.t
  -> component:Mail_log.Component.t
  -> here:Lexing.position
  -> Smtp_command.t
  -> [ `Bsmtp | `Received of Smtp_reply.t] Deferred.Or_error.t

val send_string
  :  t
  -> log:Mail_log.t
  -> ?flows:Mail_log.Flows.t
  -> component:Mail_log.Component.t
  -> here:Lexing.position
  -> string
  -> unit Deferred.Or_error.t

val send_receive_string
  :  ?on_eof:(?partial:Smtp_reply.partial -> unit -> Smtp_reply.t Deferred.Or_error.t)
  -> ?timeout:Time.Span.t
  -> t
  -> log:Mail_log.t
  -> ?flows:Mail_log.Flows.t
  -> component:Mail_log.Component.t
  -> here:Lexing.position
  -> string
  -> [ `Bsmtp | `Received of Smtp_reply.t] Deferred.Or_error.t

(* Low level access *)
val writer : t -> Writer.t
val reader : t -> Reader.t option
val remote_address : t -> Smtp_socket_address.t option
