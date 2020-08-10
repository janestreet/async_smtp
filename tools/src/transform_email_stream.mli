open! Core
open! Async
open Async_smtp_types

module Envelopes : sig
  type t =
    { sort : [ `Envelope_id | `Sender | `Recipients | `Subject | `Body | `Headers ] list }
  [@@deriving sexp]
end

module Bodies : sig
  module Rewrite : sig
    type t =
      Re2.t * [ `Rewrite_entire_body_to of string | `Rewrite_all_matches_to of string ]
  end

  type t =
    { rewrites : Rewrite.t list
    ; hash : [ `whole | `parts ] option
    }
  [@@deriving sexp]
end

module Config : sig
  type t =
    { headers : Headers.Config.t
    ; bodies : Bodies.t
    ; messages : Envelopes.t
    }
  [@@deriving sexp]

  val default : t
  val load : string -> t Deferred.t
end

val transform_without_sort : Config.t -> Smtp_envelope.t -> Smtp_envelope.t
val sort : Config.t -> Smtp_envelope.t Pipe.Reader.t -> Smtp_envelope.t Pipe.Reader.t
