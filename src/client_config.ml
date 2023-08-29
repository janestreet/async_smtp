open Core
open Async
open Async_ssl.Std

module Tls = struct
  type t =
    { version : Ssl.Version.t option
    ; options : Ssl.Opt.t list option
    ; name : string option
    ; allowed_ciphers : [ `Secure | `Openssl_default | `Only of string list ]
    ; ca_file : string option
    ; ca_path : string option
    ; mode : [ `Required | `Always_try | `If_available ]
    ; certificate_mode : [ `Ignore | `Verify ]
    }
  [@@deriving sexp, fields ~getters]

  let default =
    { version = None
    ; options = None
    ; name = None
    ; allowed_ciphers = `Secure
    ; ca_file = None
    ; ca_path = None
    ; mode = `If_available
    ; certificate_mode = `Ignore
    }
  ;;
end

module Domain_suffix = String

type t =
  { greeting : string option [@sexp.option]
  ; tls : (Domain_suffix.t * Tls.t) list
  ; send_receive_timeout : [ `Default | `This of Time_float.Span.t ]
  ; final_ok_timeout : [ `Default | `This of Time_float.Span.t ]
  }
[@@deriving sexp]

let default =
  { tls = [ "", Tls.default ]
  ; greeting = None
  ; send_receive_timeout = `Default
  ; final_ok_timeout = `Default
  }
;;

let load_exn file = Sexp_macro.load_sexp file t_of_sexp >>| Or_error.ok_exn

let match_tls_domain t domain =
  List.find t.tls ~f:(fun (suffix, _) -> String.is_suffix domain ~suffix)
  |> Option.map ~f:snd
;;

let has_tls t = not (List.is_empty t.tls)

let send_receive_timeout t =
  match t.send_receive_timeout with
  | `This send_receive_timeout -> send_receive_timeout
  | `Default -> Time_float.Span.of_sec 300.
;;

let final_ok_timeout t =
  match t.final_ok_timeout with
  | `This final_ok_timeout -> final_ok_timeout
  | `Default -> Time_float.Span.scale (send_receive_timeout t) 2.
;;
