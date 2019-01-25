open Core
open! Async

type t =
  { spool_dir : string
  ; tmp_dir : string option
  ; max_concurrent_send_jobs : int
  ; connection_cache : Resource_cache.Address_config.Stable.V1.t
                         [@default Resource_cache.Address_config.default]
  ; client : Client_config.t
  }
[@@deriving fields, sexp]

let tmp_dir t = Option.value ~default:(spool_dir t ^/ "temp") (tmp_dir t)

let default =
  { spool_dir = "."
  ; tmp_dir = None
  ; max_concurrent_send_jobs = 0
  ; connection_cache = Resource_cache.Address_config.default
  ; client = Client_config.default
  }
;;

let load_exn file = Sexp_macro.load_sexp file t_of_sexp >>| Or_error.ok_exn
