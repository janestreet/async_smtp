open Core.Std
open Async.Std
open Async_smtp.Std

(* It is common for some external content scanning software to accept messages over smtp,
   scan them, then return them back over smtp with some added headers. This module
   coordinates this exchange by starting an smtp server to accept the returned message. *)

(* The config of the external content filter will have some options for [socket_in] and
   [socket_out] (or something named close to that)

   [socket_in] must match [content_filter_in]
   [socket_out] must point to the [where_to_listen] of [smtp_server_config] *)
module Config : sig
  type t =
    { content_filter_in    : Address.t
    ; smtp_server_config   : Smtp_server.Config.t
    } [@@deriving sexp, fields]
end

type t

(* This starts the local smtp server that will be accepting the messages back from the
   content filter. The server will stop at shutdown. *)
val initialize_exn : Config.t -> log : Log.t -> t Deferred.t

val send
  :  t
  -> ?timeout : Core.Span.t (* defaults to 10 seconds *)
  -> Smtp_envelope.t
  -> Smtp_envelope.t Or_error.t Deferred.t
