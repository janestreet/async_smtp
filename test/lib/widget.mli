open! Core
open! Async

module Metadata : sig
  type t =
    | Sprocket of string
    | Cog of int
  [@@deriving sexp_of]

  include Stringable.S with type t := t
end

module Data : sig
  type t =
    { serial_number : int
    ; customer : string
    }
  [@@deriving sexp_of, fields ~iterators:create]

  val load : string -> t Deferred.Or_error.t
  val save : ?temp_file:string -> t -> string -> unit Deferred.Or_error.t
  val to_string : t -> string
end

module Queue : sig
  type t =
    | Queue1
    | Queue2
    | Queue3
  [@@deriving sexp, enumerate, compare]

  val to_dirname : t -> string
end

module Throttle : sig
  val enqueue : (unit -> 'a Deferred.t) -> 'a Deferred.t
end
