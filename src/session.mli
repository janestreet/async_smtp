open! Core
open! Async
open Async_ssl.Std
open Async_smtp_types

type t =
  { remote : Socket.Address.Inet.t
  ; local : Socket.Address.Inet.t
  ; helo : string option
  ; tls : Ssl.Connection.t option
  ; authenticated : string option
  ; advertised_extensions : Smtp_extension.t list
  }
[@@deriving sexp_of]

val create
  :  remote:Socket.Address.Inet.t
  -> local:Socket.Address.Inet.t
  -> ?helo:string
  -> ?tls:Ssl.Connection.t
  -> ?authenticated:string
  -> ?advertised_extensions:Smtp_extension.t list
  -> unit
  -> t

val cleanup : t -> unit Deferred.Or_error.t
