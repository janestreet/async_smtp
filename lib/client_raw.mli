open Core.Std
open Async.Std
open Async_ssl.Std
open Types

module Peer_info : sig
  type t

  val greeting : t -> string option
  val hello    : t -> [`Simple of string | `Extended of string * Extension.t list] option
  val supports_extension : t -> Extension.t -> bool
end

type t

val create
  :  ?log:Log.t
  -> session_id:string
  -> dest:Host_and_port.t
  -> Reader.t
  -> Writer.t
  -> Client_config.t
  -> t

val create_bsmtp
  :  ?log:Log.t
  -> session_id:string
  -> Writer.t
  (* Config must be set to not use TLS. *)
  -> Client_config.t
  -> t

val config : t -> Client_config.t
val info : t -> Peer_info.t option

val is_using_tls : t -> bool
val has_log : t -> bool

val with_session
  :  t
  -> f:(t -> 'a Deferred.Or_error.t)
  -> 'a Deferred.Or_error.t

val do_helo : t -> unit Deferred.Or_error.t

val send : t -> Command.t -> unit Deferred.Or_error.t

val receive
  :  ?timeout:Time.Span.t
  -> t
  -> [ `Bsmtp | `Received of Reply.t] Deferred.Or_error.t

val send_receive
  :  ?timeout:Time.Span.t
  -> t
  -> Command.t
  -> [ `Bsmtp | `Received of Reply.t] Deferred.Or_error.t

(* Low level access *)
val writer : t -> Writer.t
val reader : t -> Reader.t option

module Log : sig
  val debug : t -> ('a, unit, string, unit) format4 -> 'a
  val error : t -> ('a, unit, string, unit) format4 -> 'a
end
