open! Core
open! Async
open Async_smtp_types

module Envelopes : sig
  type t =
    { sort :
        [ `Envelope_id
        | `Sender
        | `Recipients
        | `Subject
        | `Body
        | `Headers
        ] sexp_list
    } [@@deriving sexp]
end

module Bodies : sig
  module Rewrite : sig
    type t =
      { if_contains : Re2.t
      ; rewrite_to  : string
      }
  end

  type t =
    { rewrites : Rewrite.t sexp_list
    ; hash : [ `whole | `parts ] sexp_option
    } [@@deriving sexp]
end

module Config : sig
  type t =
    { headers  : Headers.Config.t
    ; bodies   : Bodies.t
    ; messages : Envelopes.t
    } [@@deriving sexp]

  val default : t

  val load : string -> t Deferred.t
end

val transform_without_sort
  : Config.t -> Smtp_envelope.t -> Smtp_envelope.t
val sort
  : Config.t -> Smtp_envelope.t Pipe.Reader.t -> Smtp_envelope.t Pipe.Reader.t
