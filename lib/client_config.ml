open Core.Std
open Async.Std
open Async_ssl.Std

module Tls = struct
  type t =
    { version : Ssl.Version.t option
    ; name : string option
    ; ca_file : string option
    ; ca_path : string option
    ; mode :
        [ `Required
        | `Always_try
        | `If_available
        ]
    ; certificate_mode :
        [ `Ignore
        | `Verify
        ]
    } with sexp, fields

  let default =
    { version = None
    ; name = None
    ; ca_file = None
    ; ca_path = None
    ; mode = `If_available
    ; certificate_mode = `Ignore
    }
end

module Domain_suffix = String

type t =
  { greeting : string sexp_option
  ; tls : (Domain_suffix.t * Tls.t) list
  } with sexp, fields

let default =
  { tls = ["", Tls.default]
  ; greeting = None
  }

let load_exn file =
  Reader.load_sexp file t_of_sexp ~expand_macros:true
  >>| Or_error.ok_exn
;;

let match_tls_domain t domain =
  List.find t.tls ~f:(fun (suffix, _) ->
    String.is_suffix domain ~suffix)
  |> Option.map ~f:snd

let has_tls t =
  not (List.is_empty t.tls)
