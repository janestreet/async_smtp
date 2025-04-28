open! Core
open Async
module Config = Resource_cache.Address_config

module Status : sig
  include Resource_cache.Status.S with type Key.t = Address_and_route.t

  module Stable : sig
    module V2 : sig
      type nonrec t = t [@@deriving sexp, bin_io]
    end
  end
end

type t

val init
  : (?component:Mail_log.Component.t
     -> log:Mail_log.t
     -> cache_config:Config.t
     -> client_config:Client_config.t
     -> connection_cache_warming:Spool_config.Connection_cache_warming.t option
     -> unit
     -> t)
      Tcp.Aliases.with_connect_options

val close_and_flush : t -> unit Deferred.t
val close_started : t -> bool
val close_finished : t -> unit Deferred.t
val status : t -> Status.t
val config : t -> Config.t

module Tcp : sig
  (** [with_'] concurrently tries to get a cached connection for one of [addresses]. [`Ok]
      and [`Error_opening_resource] return back the [Smtp_socket_address.t] that was used. *)
  val with_'
    :  ?give_up:unit Deferred.t
    -> f:(flows:Mail_log.Flows.t -> Client.t -> 'a Deferred.Or_error.t)
    -> cache:t
    -> ?route:string
    -> ?credentials:Credentials.t
    -> Host_and_port.t list
    -> [ `Ok of Host_and_port.t * 'a Or_error.t
       | `Error_opening_all_addresses of (Host_and_port.t * Error.t) list
       | `Gave_up_waiting_for_address
       | `Cache_is_closed
       ]
         Deferred.t
end
