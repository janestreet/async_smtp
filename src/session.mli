open! Core
open! Async
open Async_ssl.Std

type t =
  { remote                    : Address.t
  ; local                     : Address.t
  ; helo                      : string option
  ; tls                       : Ssl.Connection.t option
  ; authenticated             : string option
  ; advertised_extensions     : Smtp_extension.t list
  } [@@deriving sexp_of, fields]

val create
  :  remote : Address.t
  -> local : Address.t
  -> ?helo : string
  -> ?tls : Ssl.Connection.t
  -> ?authenticated : string
  -> ?advertised_extensions : Smtp_extension.t list
  -> unit
  -> t

val cleanup : t -> unit Deferred.Or_error.t
