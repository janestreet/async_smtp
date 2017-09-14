open Core
open Async

(** [Cache.Make] creates a cache module that exposes a simple [with_] interface over its
    resources. The cache has the following properties:

    Resource reuse: When a resource [r] is opened, it will remain open until one of the
    following:
    - [f r] raised an exception where [f] was a function passed to [with_]
    - [r] has been idle for [idle_cleanup_after]
    - [r] has been used [max_resource_reuse] times
    - [close_and_flush] has been called on the cache

    When a resource is closed, either because of one of the above conditions, or because
    it was closed by other means, it no longer counts towards the limits.

    Limits: The cache respects the following limits:
    - No more than [max_resources] are open simultaneously
    - No more than [max_resources_per_id] are open simultaneously for a given id (args)
*)

module type Resource_intf = sig
  module Key : Identifiable.S

  module Args : sig

    type t

    val key : t -> Key.t

    (** Used in error messages *)
    val to_string_hum : t -> string
  end

  type t

  val open_ : Args.t -> t Deferred.Or_error.t

  val close : t -> unit Deferred.t
  val close_finished : t -> unit Deferred.t

  (** [is_closed t] should return [true] iff [close t] has been called, even if
      [close_finished] has not been determined. *)
  val is_closed : t -> bool
end

module Config : sig
  type t =
    { max_resources        : int
    ; idle_cleanup_after   : Time.Span.t
    ; max_resources_per_id : int
    ; max_resource_reuse   : int
    } [@@deriving fields, sexp, bin_io, compare]

  val create
    :  max_resources : int
    -> idle_cleanup_after : Time.Span.t
    -> max_resources_per_id : int
    -> max_resource_reuse : int
    -> t
end

module type Status_intf = sig
  module Key : Identifiable.S

  module Resource : sig
    type state = [ `Busy | `Idle | `Closing ] [@@deriving sexp, bin_io, compare]

    type t =
      { state : state
      ; since : Time.Span.t
      } [@@deriving fields, sexp, bin_io, compare]
  end

  module Resource_list : sig
    type t =
      { key               : Key.t
      ; resources         : Resource.t list
      ; queue_length      : int
      ; max_time_on_queue : Time.Span.t option
      } [@@deriving fields, sexp, bin_io, compare]
  end

  type t =
    { resource_lists    : Resource_list.t list
    ; num_jobs_in_cache : int
    } [@@deriving fields, sexp, bin_io, compare]
end

module Make(R : Resource_intf) : sig

  module Status : Status_intf with module Key := R.Key

  type t

  val init : config : Config.t -> t

  val status : t -> Status.t
  val config : t -> Config.t

  (** [with_ t args ~f] calls [f resource] where [resource] is either:

      1) An existing cached resource that was opened with args' such that
      [R.Args.Key.equal (R.Args.key args) (R.Args.key args') = true]
      2) A newly opened resource created by [R.open_ args], respecting the
      limits of [t.config]

      Returns an error if:
      - the cache is closed
      - [R.open_] returned an error
      - no resource is obtained before [give_up] is determined

      If [f] raises, the exception is not caught, but the [resource] will be
      closed and the [Cache] will remain in a working state (no resources are lost).
  *)
  val with_
    :  ?open_timeout:Time.Span.t (** default [None] *)
    -> ?give_up:unit Deferred.t (** default [Deferred.never] *)
    -> t
    -> R.Args.t
    -> f : (R.t -> 'a Deferred.t)
    -> 'a Deferred.Or_error.t

  (** Like [with_] but classify the different errors *)
  val with_'
    :  ?open_timeout:Time.Span.t
    -> ?give_up:unit Deferred.t
    -> t
    -> R.Args.t
    -> f : (R.t -> 'a Deferred.t)
    -> [ `Ok of 'a
       | `Gave_up_waiting_for_resource
       | `Error_opening_resource of Error.t
       | `Cache_is_closed
       ] Deferred.t

  (** Like [with_] and [with_'] except [f] is run on the first matching available resource
      (or the first resource that has availability to be opened). Preference is given
      towards those earlier in [args_list] when possible *)
  val with_any
    :  ?open_timeout:Time.Span.t
    -> ?give_up:unit Deferred.t
    -> t
    -> R.Args.t list
    -> f : (R.t -> 'a Deferred.t)
    -> (R.Args.t * 'a) Deferred.Or_error.t

  val with_any'
    :  ?open_timeout:Time.Span.t
    -> ?give_up:unit Deferred.t
    -> t
    -> R.Args.t list
    -> f : (R.t -> 'a Deferred.t)
    -> [ `Ok of R.Args.t * 'a
       | `Error_opening_resource of R.Args.t * Error.t
       | `Gave_up_waiting_for_resource
       | `Cache_is_closed
       ] Deferred.t

  (** Tries [with_any'] in a loop (removing args that have open errors) until receiving an
      [`Ok], or until it has failed to open all resources in [args_list]. *)
  val with_any_loop
    :  ?open_timeout:Time.Span.t
    -> ?give_up:unit Deferred.t
    -> t
    -> R.Args.t list
    -> f : (R.t -> 'a Deferred.t)
    -> [ `Ok of R.Args.t * 'a
       | `Error_opening_all_resources of (R.Args.t * Error.t) list
       | `Gave_up_waiting_for_resource
       | `Cache_is_closed
       ] Deferred.t

  val close_started  : t -> bool
  val close_finished : t -> unit Deferred.t

  (** Close all currently open resources and prevent the creation of new ones. All
      subsequent calls to [with_] and [immediate] fail with [`Cache_is_closed]. Any jobs
      that are waiting for a connection will return with [`Cache_is_closed]. The
      returned [Deferred.t] is determined when all jobs have finished running and all
      resources have been closed. *)
  val close_and_flush : t -> unit Deferred.t

end
