open! Core
open Async

module Config : sig
  type t =
    { max_open_connections          : int
    ; cleanup_idle_connection_after : Time.Span.t
    ; max_connections_per_address   : int
    ; max_connection_reuse          : int
    } [@@deriving sexp]

  val create
    :  max_open_connections:int
    -> cleanup_idle_connection_after:Time.Span.t
    -> max_connections_per_address:int
    -> max_connection_reuse:int
    -> t
end

type t

val init
  : ( ?component : Mail_log.Component.t
      -> log : Mail_log.t
      -> cache_config : Config.t
      -> client_config : Client_config.t
      -> unit
      -> t
    ) Tcp.with_connect_options

val close_and_flush : t -> unit Deferred.t

val close_started  : t -> bool
val close_finished : t -> unit Deferred.t

(* NOTE: Make sure not to reuse connections when using SMTP authentication *)
module Tcp : sig
  (** [with_'] concurrently tries to get a cached connection for one of [addresses].
      [`Ok] and [`Error_opening_resource] return back the [Address.t] that was used. *)
  val with_'
    :  ?give_up:unit Deferred.t
    -> f:(flows:Mail_log.Flows.t -> Client.t -> 'a Deferred.Or_error.t)
    -> cache:t
    -> Address.t list
    -> [ `Ok of Address.t * 'a Or_error.t
       | `Error_opening_resource of (Address.t * Error.t) list
       | `Gave_up_waiting_for_resource
       | `Cache_is_closed
       ] Deferred.t
end

