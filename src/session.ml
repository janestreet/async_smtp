open! Core
open! Async
open Async_ssl.Std
open Async_smtp_types

type t =
  { remote                : Smtp_socket_address.t
  ; local                 : Smtp_socket_address.t
  ; helo                  : string option
  ; tls                   : Ssl.Connection.t option
  ; authenticated         : string option
  ; advertised_extensions : Smtp_extension.t list
  } [@@deriving sexp_of, fields]

let create ~remote ~local ?helo ?tls ?authenticated ?(advertised_extensions=[]) () =
  { remote; local; helo; tls; authenticated; advertised_extensions; }
;;

let cleanup t =
  match t.tls with
  | None -> return (Ok ())
  | Some tls ->
    Ssl.Connection.close tls;
    Ssl.Connection.closed tls
;;
