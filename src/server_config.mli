open! Core
open! Async
open Async_ssl.Std

module Tls_options : sig
  type t =
    { version : Ssl.Version.t option
    ; options : Ssl.Opt.t list option
    ; name : string option
    ; allowed_ciphers : [ `Secure | `Openssl_default | `Only of string list ]
    ; crt_file : string
    ; key_file : string
    ; ca_file : string option
    ; ca_path : string option
    }
  [@@deriving sexp]
end

module Tcp_options : sig
  type t =
    { max_accepts_per_batch : int option
    ; backlog : int option
    }
  [@@deriving fields ~getters, sexp]
end

module Where_to_listen : sig
  type t =
    | All_interfaces_on_port of int
    | Localhost_on_port of int
    | Localhost_on_port_chosen_by_os
    | Ip_on_port of Unix.Inet_addr.t * int
  [@@deriving sexp]

  val to_tcp_where_to_listen : t -> Tcp.Where_to_listen.inet
  val socket_address : t -> Socket.Address.Inet.t
end

module Timeouts : sig
  (** Server timeouts while reading SMTP input.

      If a line of input (either an SMTP command or a data line) is not received within
      [receive], the SMTP connection is dropped and the message is abandoned.

      If [Smtp_server.close] has been called, [receive_after_close] is used for the read
      timeout. This can be set to a time span smaller than [receive] to speed up server
      shutdown. *)
  type t =
    { receive : Time_float.Span.t
    ; receive_after_close : Time_float.Span.t
    }
  [@@deriving sexp]

  val default : t
end

type t =
  { where_to_listen : Where_to_listen.t list
  ; max_concurrent_receive_jobs_per_port : int
  ; timeouts : Timeouts.t
  ; rpc_port : int
  ; rpc_heartbeat_config : Rpc.Connection.Heartbeat_config.t option
  ; malformed_emails : [ `Reject | `Wrap ]
  ; max_message_size : Byte_units.t
  ; tls_options : Tls_options.t option
  ; tcp_options : Tcp_options.t option
  }
[@@deriving fields ~getters, sexp]

val load_exn : string -> t Deferred.t
val default : t
