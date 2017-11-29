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

val create
  :  remote : Smtp_socket_address.t
  -> local : Smtp_socket_address.t
  -> ?helo : string
  -> ?tls : Ssl.Connection.t
  -> ?authenticated : string
  -> ?advertised_extensions : Smtp_extension.t list
  -> unit
  -> t

val cleanup : t -> unit Deferred.Or_error.t
