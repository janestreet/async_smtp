open Core.Core_stable

module Stable = struct
  module V1 = struct
    type t =
      { spool_dir : string
      ; tmp_dir : string option
      ; max_concurrent_send_jobs : int
      ; connection_cache : Resource_cache.Address_config.Stable.V1.t
                             [@default Resource_cache.Address_config.default]
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
end

open Core
open! Async

type t = Stable.V2.t =
  { spool_dir : string
  ; tmp_dir : string option
  ; connection_cache : Resource_cache.Address_config.Stable.V1.t
  ; client : Client_config.t
  }
[@@deriving fields, sexp_of]

let t_of_sexp sexp =
  try Stable.V2.t_of_sexp sexp with
  | v2_exn ->
    (try Stable.V1.t_of_sexp sexp |> Stable.V2.of_V1_t with
     | v1_exn ->
       raise_s [%message "Failed to parse spool config" (v2_exn : Exn.t) (v1_exn : Exn.t)])
;;

let tmp_dir t = Option.value ~default:(spool_dir t ^/ "temp") (tmp_dir t)

let default =
  { spool_dir = "."
  ; tmp_dir = None
  ; connection_cache = Resource_cache.Address_config.default
  ; client = Client_config.default
  }
;;

let load_exn file = Sexp_macro.load_sexp file t_of_sexp >>| Or_error.ok_exn
