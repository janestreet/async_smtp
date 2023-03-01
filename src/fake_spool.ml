open Core
open Async
open Async_smtp_types

type t = { send : Smtp_envelope.Routed.t -> unit Or_error.t Deferred.t }

let create ~config:_ ~send = return (Ok { send })

let add { send } ~original_msg msgs =
  match%bind Deferred.Or_error.List.iter ~how:`Sequential msgs ~f:send with
  | Error e -> return (Error e)
  | Ok () -> return (Ok (Smtp_envelope.id original_msg))
;;

let flush ?timeout:_ _t = return true
