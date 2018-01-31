open! Core
open Async
open Resource_cache
open Async_smtp_types

module Config = Cache.Address_config

module Address_and_route : sig
  type t =
    { address : Smtp_socket_address.t
    ; route   : string option
    } [@@deriving fields]
  include Identifiable.S with type t := t
end

module Status : Cache.Status_intf with type Key.t = Address_and_route.t

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

val status : t -> Status.t
val config : t -> Config.t

(* NOTE: Make sure not to reuse connections when using SMTP authentication *)
module Tcp : sig
  (** [with_'] concurrently tries to get a cached connection for one of [addresses].
      [`Ok] and [`Error_opening_resource] return back the [Smtp_socket_address.t] that was
      used. *)
  val with_'
    :  ?give_up:unit Deferred.t
    -> f:(flows:Mail_log.Flows.t -> Client.t -> 'a Deferred.Or_error.t)
    -> cache:t
    -> ?route:string
    -> Smtp_socket_address.t list
    -> [ `Ok of Smtp_socket_address.t * 'a Or_error.t
       | `Error_opening_all_addresses of (Smtp_socket_address.t * Error.t) list
       | `Gave_up_waiting_for_address
       | `Cache_is_closed
       ] Deferred.t
end

