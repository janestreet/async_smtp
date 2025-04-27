open Core
open! Async
open Async_ssl.Std

module Tls_options = struct
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

module Tcp_options = struct
  type t =
    { max_accepts_per_batch : int option [@sexp.option]
    ; backlog : int option [@sexp.option]
    }
  [@@deriving fields ~getters, sexp]
end

module Where_to_listen = struct
  module V2 = struct
    type t =
      | All_interfaces_on_port of int
      | Localhost_on_port of int
      | Localhost_on_port_chosen_by_os
      | Ip_on_port of Unix.Inet_addr.Stable.V1.t * int
    [@@deriving sexp]

    let to_tcp_where_to_listen = function
      | All_interfaces_on_port port -> Tcp.Where_to_listen.of_port port
      | Localhost_on_port port -> Tcp.Where_to_listen.bind_to Localhost (On_port port)
      | Localhost_on_port_chosen_by_os ->
        Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os
      | Ip_on_port (addr, port) ->
        Tcp.Where_to_listen.bind_to (Address addr) (On_port port)
    ;;

    let interface = function
      | All_interfaces_on_port _ -> `All
      | Localhost_on_port _ -> `Ip Unix.Inet_addr.localhost
      | Localhost_on_port_chosen_by_os -> `Ip Unix.Inet_addr.localhost
      | Ip_on_port (addr, _) -> `Ip addr
    ;;

    let port = function
      | All_interfaces_on_port port -> port
      | Localhost_on_port port -> port
      | Localhost_on_port_chosen_by_os -> 0
      | Ip_on_port (_, port) -> port
    ;;

    let socket_address t =
      let port = port t in
      match interface t with
      | `All -> Socket.Address.Inet.create_bind_any ~port
      | `Ip ip -> Socket.Address.Inet.create ip ~port
    ;;
  end

  module V1 = struct
    type t = [ `Port of int ] [@@deriving sexp]

    let to_v2 (`Port port) = V2.All_interfaces_on_port port
  end

  include V2

  let t_of_sexp sexp =
    try t_of_sexp sexp with
    | v2_exn ->
      (try V1.t_of_sexp sexp |> V1.to_v2 with
       | v1_exn ->
         raise_s
           [%message
             "Failed to parse [Server_config.Where_to_listen.t]"
               (v2_exn : Exn.t)
               (v1_exn : Exn.t)])
  ;;
end

module Timeouts = struct
  type t =
    { receive : Time_float.Span.t
    ; receive_after_close : Time_float.Span.t
    }
  [@@deriving sexp]

  let default =
    (* Recommendation from RFC 5321 section 4.5.3.2.7 *)
    { receive = Time_float.Span.of_min 5.
    ; receive_after_close = Time_float.Span.of_sec 10.
    }
  ;;
end

type t =
  { where_to_listen : Where_to_listen.t list
  ; max_concurrent_receive_jobs_per_port : int
  ; timeouts : Timeouts.t
  ; rpc_port : int
  ; rpc_heartbeat_config : Rpc.Connection.Heartbeat_config.t option [@sexp.option]
  ; malformed_emails : [ `Reject | `Wrap ]
  ; max_message_size : Byte_units.Stable.V1.t
  ; tls_options : Tls_options.t option [@sexp.option]
  ; tcp_options : Tcp_options.t option [@sexp.option]
  }
[@@deriving fields ~getters, sexp]

let load_exn file = Sexp_macro.load_sexp file t_of_sexp >>| Or_error.ok_exn

let default =
  { where_to_listen = []
  ; max_concurrent_receive_jobs_per_port = 0
  ; timeouts = Timeouts.default
  ; rpc_port = 0
  ; rpc_heartbeat_config = None
  ; malformed_emails = `Reject
  ; max_message_size = Byte_units.of_megabytes 35.
  ; tls_options = None
  ; tcp_options = None
  }
;;
