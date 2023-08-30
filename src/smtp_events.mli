open Core
open Async
open Async_smtp_types

(* Use this module to track any smtp events that are interesting for monitoring *)

type t

module Event : sig
  module Envelope_received : sig
    type t =
      { sender : string
      ; recipients : string list
      }
    [@@deriving sexp, bin_io]
  end

  type t = Time_float.t * [ `Envelope_received of Envelope_received.t ]
  [@@deriving sexp, bin_io]
end

val envelope_received : t -> Smtp_envelope.t -> unit
val create : unit -> t
val event_stream : t -> Event.t Pipe.Reader.t
