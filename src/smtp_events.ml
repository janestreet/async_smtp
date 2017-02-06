open Core
open Async

module Event = struct
  module Envelope_received = struct
    type t =
      { sender : string
      ; recipients : string list}
    [@@deriving sexp, bin_io]
  end

  type t = Time.t *
           [ `Envelope_received of Envelope_received.t ]
  [@@deriving sexp, bin_io]
end

type t = { event_stream : (Event.t -> unit) Bus.Read_write.t }

let envelope_received t envelope =
  let sender = Types.Envelope.string_sender envelope in
  let recipients = Types.Envelope.string_recipients envelope in
  let event =
    Time.now (), `Envelope_received { Event.Envelope_received. sender; recipients }
  in
  Bus.write t.event_stream event
;;

let create () =
  let event_stream =
    Bus.create [%here] Arity1 ~allow_subscription_after_first_write:true
      ~on_callback_raise:Error.raise
  in
  { event_stream }
;;

let event_stream t = Bus.pipe1_exn (Bus.read_only t.event_stream) [%here]
;;
