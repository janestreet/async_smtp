open Core.Core_stable

module Stable = struct
  module Connection_cache_warming = struct
    module V1 = struct
      type t =
        { addresses_to_keep_warm : Address_and_route.Stable.V2.t list
        ; num_connections_per_address : int
        }
      [@@deriving sexp]
    end
  end

  module V1 = struct
    type t =
      { spool_dir : string
      ; tmp_dir : string option
      ; max_concurrent_send_jobs : int
      ; connection_cache : Resource_cache.Address_config.Stable.V1.t
           [@default
             Resource_cache.Address_config.default
             |> Resource_cache.Address_config.Stable.V2.of_v3
             |> Resource_cache.Address_config.Stable.V1.of_v2]
      ; client : Client_config.t
      }
    [@@deriving sexp]
  end

  module V2 = struct
    type t =
      { spool_dir : string
      ; tmp_dir : string option
      ; connection_cache : Resource_cache.Address_config.Stable.V1.t
      ; client : Client_config.t
      }
    [@@deriving sexp, stable_record ~version:V1.t ~add:[ max_concurrent_send_jobs ]]
  end

  module V3 = struct
    type t =
      { spool_dir : string
      ; tmp_dir : string option
      ; connection_cache : Resource_cache.Address_config.Stable.V1.t
      ; client : Client_config.t
      ; load_balance : bool
      }
    [@@deriving sexp, stable_record ~version:V2.t ~remove:[ load_balance ]]

    let of_V2_t = of_V2_t ~load_balance:false
  end

  module V4 = struct
    type t =
      { spool_dir : string
      ; tmp_dir : string option
      ; connection_cache : Resource_cache.Address_config.Stable.V1.t
      ; connection_cache_warming : Connection_cache_warming.V1.t option
      ; client : Client_config.t
      }
    [@@deriving
      sexp
      , stable_record
          ~version:V3.t
          ~remove:[ connection_cache_warming ]
          ~add:[ load_balance ]]

    let of_V3_t = of_V3_t ~connection_cache_warming:None
  end
end

open Core
open! Async
module Connection_cache_warming = Stable.Connection_cache_warming.V1

type t = Stable.V4.t =
  { spool_dir : string
  ; tmp_dir : string option
  ; connection_cache : Resource_cache.Address_config.Stable.V1.t
  ; connection_cache_warming : Connection_cache_warming.t option
  ; client : Client_config.t
  }
[@@deriving fields ~getters, sexp_of]

let t_of_sexp sexp =
  try Stable.V4.t_of_sexp sexp with
  | v4_exn ->
    (try Stable.V3.t_of_sexp sexp |> Stable.V4.of_V3_t with
     | v3_exn ->
       (try Stable.V2.t_of_sexp sexp |> Stable.V3.of_V2_t |> Stable.V4.of_V3_t with
        | v2_exn ->
          (try
             Stable.V1.t_of_sexp sexp
             |> Stable.V2.of_V1_t
             |> Stable.V3.of_V2_t
             |> Stable.V4.of_V3_t
           with
           | v1_exn ->
             raise_s
               [%message
                 "Failed to parse spool config"
                   (v4_exn : Exn.t)
                   (v3_exn : Exn.t)
                   (v2_exn : Exn.t)
                   (v1_exn : Exn.t)])))
;;

let tmp_dir t = Option.value ~default:(spool_dir t ^/ "temp") (tmp_dir t)

let default =
  { spool_dir = "."
  ; tmp_dir = None
  ; connection_cache =
      Resource_cache.Address_config.default
      |> Resource_cache.Address_config.Stable.V2.of_v3
      |> Resource_cache.Address_config.Stable.V1.of_v2
  ; connection_cache_warming = None
  ; client = Client_config.default
  }
;;

let load_exn file = Sexp_macro.load_sexp file t_of_sexp >>| Or_error.ok_exn
