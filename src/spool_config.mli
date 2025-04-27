open! Core
open! Async

module Connection_cache_warming : sig
  (** When provided, background async jobs will connect to [addresses_to_keep_warm]
      whenever there are less than [num_connections_per_address] open connections to that
      address. This can be used, in conjunction with supplying randomized addresses to
      [Client_cache.Tcp.with_'] to spread load roughly evenly across all addresses. *)

  type t =
    { addresses_to_keep_warm : Address_and_route.t list
    ; num_connections_per_address : int
    }
end

type t =
  { spool_dir : string
  ; tmp_dir : string option
  ; connection_cache : Resource_cache.Address_config.Stable.V1.t
  ; connection_cache_warming : Connection_cache_warming.t option
  ; client : Client_config.t
  }
[@@deriving fields ~getters, sexp]

val tmp_dir : t -> string
val default : t
val load_exn : string -> t Deferred.t
