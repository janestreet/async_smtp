open! Core
open! Async
open With_info
module Reject_or_error = Reject_or_error

type 'a t = ('a, Reject_or_error.t) Deferred.Result.t

include Monad.S with type 'a t := 'a t

val exn : (Exn.t -> _ t) with_reject
val error : (Error.t -> _ t) with_reject
val error_string : (string -> _ t) with_reject
val errorf : (('a, unit, string, _ t) format4 -> 'a) with_reject
val reject : (Smtp_reply.t -> _ t) with_here
val tag : tag:string -> ('a t -> 'a t) with_maybe_here
val tag' : ?tag:string -> ('a t -> 'a t) with_maybe_here

(** Helpers to convert to an ['a t]. Unlike [try_with*] below, these will not capture
    raised exceptions. *)
val ok : 'a Deferred.t -> 'a t

val of_or_error : ('a Deferred.Or_error.t -> 'a t) with_tag
val to_or_error : 'a t -> 'a Deferred.Or_error.t
val return_or_error : ('a Or_error.t -> 'a t) with_tag

(** Capture any raised exceptions as [Error]s. *)
val try_with : ((unit -> 'a Deferred.t) -> 'a t) with_tag

val try_with_join : ((unit -> 'a t) -> 'a t) with_tag
val try_with_or_error : ((unit -> 'a Deferred.Or_error.t) -> 'a t) with_tag
