open! Core
open! Async.Std
open Types


module Peer_info : sig
  type t

  val greeting : t -> string option
  val hello    : t -> [`Simple of string | `Extended of string * Smtp_extension.t list] option
  val supports_extension : t -> Smtp_extension.t -> bool
  val dest : t -> Address.t
end

type t

val create
  :  ?flows:Mail_log.Flows.t
  -> dest:Address.t
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
val is_using_auth_login : t -> bool

val with_session
  :  t
  -> log:Mail_log.t
  -> component:Mail_log.Component.t
  -> credentials:Credentials.t option
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
  -> Command.t
  -> unit Deferred.Or_error.t

val receive
  :  ?on_eof:(?partial:Reply.partial -> unit -> Reply.t Deferred.Or_error.t)
  -> ?timeout:Time.Span.t
  -> ?flows:Mail_log.Flows.t
  -> t
  -> log:Mail_log.t
  -> component:Mail_log.Component.t
  -> here:Lexing.position
  -> [ `Bsmtp | `Received of Reply.t] Deferred.Or_error.t

val send_receive
  :  ?on_eof:(?partial:Reply.partial -> unit -> Reply.t Deferred.Or_error.t)
  -> ?timeout:Time.Span.t
  -> t
  -> log:Mail_log.t
  -> ?flows:Mail_log.Flows.t
  -> component:Mail_log.Component.t
  -> here:Lexing.position
  -> Command.t
  -> [ `Bsmtp | `Received of Reply.t] Deferred.Or_error.t

val send_string
  :  t
  -> log:Mail_log.t
  -> ?flows:Mail_log.Flows.t
  -> component:Mail_log.Component.t
  -> here:Lexing.position
  -> string
  -> unit Deferred.Or_error.t

val send_receive_string
  :  ?on_eof:(?partial:Reply.partial -> unit -> Reply.t Deferred.Or_error.t)
  -> ?timeout:Time.Span.t
  -> t
  -> log:Mail_log.t
  -> ?flows:Mail_log.Flows.t
  -> component:Mail_log.Component.t
  -> here:Lexing.position
  -> string
  -> [ `Bsmtp | `Received of Reply.t] Deferred.Or_error.t

(* Low level access *)
val writer : t -> Writer.t
val reader : t -> Reader.t option
val remote_address : t -> Address.t option
