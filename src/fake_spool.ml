open Core
open Async.Std
open Types

type t = {
  send : Envelope_with_next_hop.t -> unit Or_error.t Deferred.t
}

let create ~config:_ ~send = return (Ok {send})

let add {send} ~original_msg msgs =
  Deferred.Or_error.List.iter msgs ~f:send
  >>= function
  | Error e -> return (Error e)
  | Ok () -> return (Ok (Envelope.id original_msg))

let flush ?timeout:_ _t =
  return true

