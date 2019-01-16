open Core
open! Async
open Async_ssl.Std

module Tls_options = struct
  type t =
    { version : Ssl.Version.t option
    ; options : Ssl.Opt.t list option
    ; name : string option
    ; allowed_ciphers : [`Secure | `Openssl_default | `Only of string list]
    ; crt_file : string
    ; key_file : string
    ; ca_file : string option
    ; ca_path : string option
    }
  [@@deriving fields, sexp]
end

module Tcp_options = struct
  type t =
    { max_accepts_per_batch : int sexp_option
    ; backlog : int sexp_option
    }
  [@@deriving fields, sexp]
end

module Where_to_listen = struct
  type t = [`Port of int] [@@deriving sexp]
end

module Timeouts = struct
  type t =
    { receive : Time.Span.t
    ; receive_after_close : Time.Span.t
    }
  [@@deriving sexp]

  let default =
    (* Recommendation from RFC 5321 section 4.5.3.2.7 *)
    { receive = Time.Span.of_min 5.; receive_after_close = Time.Span.of_sec 10. }
  ;;
end

type t =
  { where_to_listen : Where_to_listen.t list
  ; max_concurrent_receive_jobs_per_port : int
  ; timeouts : Timeouts.t
  ; rpc_port : int
  ; malformed_emails : [`Reject | `Wrap]
  ; max_message_size : Byte_units.t
  ; tls_options : Tls_options.t sexp_option
  ; tcp_options : Tcp_options.t sexp_option
  }
[@@deriving fields, sexp]

let load_exn file = Sexp_macro.load_sexp file t_of_sexp >>| Or_error.ok_exn

let default =
  { where_to_listen = []
  ; max_concurrent_receive_jobs_per_port = 0
  ; timeouts = Timeouts.default
  ; rpc_port = 0
  ; malformed_emails = `Reject
  ; max_message_size = Byte_units.create `Megabytes 24.
  ; tls_options = None
  ; tcp_options = None
  }
;;
