open! Core
open! Async
open Async_smtp.Std

module Config : sig
  type t [@@deriving sexp]

  val default : t

  val load : string -> t Deferred.t
end

val transform_without_sort
  : Config.t -> Smtp_envelope.t -> Smtp_envelope.t
val sort
  : Config.t -> Smtp_envelope.t Pipe.Reader.t -> Smtp_envelope.t Pipe.Reader.t
