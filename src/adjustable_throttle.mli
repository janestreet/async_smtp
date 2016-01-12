open Async.Std

(** A throttle with adjustable [max_concurrent_jobs]. *)
type t

val create : max_concurrent_jobs:int -> t

(* No new jobs will start running if the throttle is dead. *)
val is_dead : t -> bool

(** Raises exception if the throttle is dead. *)
val enqueue : t -> (unit -> unit Deferred.t) -> unit

val set_max_concurrent_jobs : t -> int -> unit

(** Make throttle dead and wait for all currently running jobs to finish. *)
val kill_and_flush : t -> unit Deferred.t
