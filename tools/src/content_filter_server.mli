open Core
open Async
open Async_smtp

(** It is common for some external content scanning software to accept messages over smtp,
    scan them, then return them back over smtp with some added headers. This module
    coordinates this exchange by starting an smtp server to accept the returned message.
    The server that is sent to (using [send_receive]) must be configured to send responses
    to the correct address (as specified by [start_exn]). *)

type t

(** start an smtp server that will be accepting messages back *)
val start_exn : Smtp_server.Config.t -> log:Log.t -> t Deferred.t

(** [send_receive ?timeout addr message] will attempt to send [message] to [addr]. It
    waits up to [timeout] for a response message. [addr] must send a response that has the
    same headers as [message] so it can recognized as a response. *)
val send_receive
  :  t
  -> ?timeout:Time_float.Span.t (** defaults to 10 seconds *)
  -> Host_and_port.t
  -> Smtp_envelope.t
  -> Smtp_envelope.t Or_error.t Deferred.t
