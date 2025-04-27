module Stable = struct
  open Core.Core_stable

  module V1 = struct
    type t =
      { address : Host_and_port.V1.t
      ; route : string option
      }
    [@@deriving sexp, bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 72331b37f209d3a3e9f43e2a7ce58395 |}]
    ;;
  end

  module V2 = struct
    type t =
      { address : Host_and_port.V1.t
      ; credentials : Credentials.Stable.V3.t option
      ; route : string option
      }
    [@@deriving sexp, bin_io, stable_record ~version:V1.t ~remove:[ credentials ]]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 760560ddfd777807461844e9cf47ad0d |}]
    ;;

    let of_v1 = of_V1_t ~credentials:None
    let to_v1 = to_V1_t
  end
end

open! Core
open! Async

module T = struct
  type t = Stable.V2.t =
    { address : Host_and_port.t
    ; credentials : Credentials.t option
    ; route : string option
    }
  [@@deriving compare, hash, sexp_of, fields ~getters]
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)
