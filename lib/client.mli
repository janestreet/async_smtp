open Core.Std
open Async.Std
open Types

module Smtp_error : sig
  type t =
  [ `Bad_sender of Reply.t
  | `Bad_data of Reply.t
  (* According to the standard, if some recipients fail, the message is still
     accepted and forwarded to the remaining recipients. *)
  | `Bad_recipients of (string * Reply.t) list
  | `Other of Reply.t
  ] with sexp

  val to_string : t -> string

  val to_error : t -> Error.t
end

module Smtp_result : sig
  type ('a, 'err) t = ('a, 'err) Result.t Or_error.t Deferred.t

  val ok_exn : (unit, Smtp_error.t) t -> unit Deferred.t
end

val write
  :  Writer.t
  -> ?helo:string
  -> Envelope.t Pipe.Reader.t
  -> unit Or_error.t Deferred.t

val send
  :  Host_and_port.t
  -> Envelope.t
  -> (unit, Smtp_error.t) Smtp_result.t

(* Read and forward raw SMTP commands. Send one email only. Send a helo if the
   input does not already contain one. *)
val send_raw
  :  Host_and_port.t
  -> Reader.t
  -> ([ `Ok_summary of string | `Eof ], Smtp_error.t) Smtp_result.t
