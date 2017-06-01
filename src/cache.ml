open Core
open Async


module type Resource_intf = sig
  module Args : sig
    type t

    val to_string_hum : t -> string

    include Comparable.S_plain with type t := t
    include Hashable.  S_plain with type t := t
  end

  type t

  val open_ : Args.t -> t Deferred.Or_error.t

  val close : t -> unit Deferred.t
  val close_finished : t -> unit Deferred.t
  val is_closed : t -> bool
end

module Config = struct
  type t =
    { max_resources        : int
    ; idle_cleanup_after   : Time.Span.t
    ; max_resources_per_id : int
    ; max_resource_reuse   : int
    } [@@deriving fields, sexp]

  let create = Fields.create
end

module Uid = Unique_id.Int ()

module Make(R : Resource_intf) = struct

  module Delayed_failures = struct
    type t =
      [ `Error_opening_resource of R.Args.t * Error.t
      | `Cache_is_closed
      ]
  end

  module Job : sig
    type 'a t

    val create
      :  ?open_timeout:Time.Span.t
      -> give_up:unit Deferred.t
      -> f:(R.t -> 'a Deferred.t)
      -> 'a t

    (* Use [has_result t] instead of [Deferred.is_determined (result t)] to prevent a race
       condition. It is possible that the result ivar was filled but [result] is not yet
       determined. *)
    val has_result : _ t -> bool

    val result
      :  'a t
      -> [ `Ok of R.Args.t * 'a
         | `Gave_up_waiting_for_resource
         | Delayed_failures.t
         ] Deferred.t

    val f : 'a t -> (R.t -> 'a Deferred.t)
    val open_timeout : 'a t -> Time.Span.t option

    val mark_result_from_available_resource
      :  'a t
      -> R.Args.t
      -> result:'a Deferred.t
      -> unit

    val mark_result_from_resource_creation
      :  'a t
      -> result:
           [ `Ok of R.Args.t * 'a
           | Delayed_failures.t
           (* This case is not possible, but the compiler gets mad otherwise *)
           | `Gave_up_waiting_for_resource
           ] Deferred.t
      -> unit
    val mark_cache_closed : 'a t -> unit

  end = struct
    type 'a t =
      { f : R.t -> 'a Deferred.t
      ; result_ivar :
          [ `Ok of R.Args.t * 'a
          | `Gave_up_waiting_for_resource
          | Delayed_failures.t
          ] Deferred.t Ivar.t
      ; open_timeout : Time.Span.t option
      } [@@deriving fields]

    let create ?open_timeout ~give_up ~f =
      let result_ivar = Ivar.create () in
      upon give_up (fun () ->
        Ivar.fill_if_empty result_ivar (return `Gave_up_waiting_for_resource));
      { f
      ; result_ivar
      ; open_timeout
      }
    ;;

    let mark_result_from_available_resource t args ~result =
      Ivar.fill t.result_ivar (result >>| fun res -> `Ok (args, res))
    ;;

    let mark_result_from_resource_creation t ~result =
      Ivar.fill t.result_ivar result
    ;;

    let mark_cache_closed t =
      Ivar.fill_if_empty t.result_ivar (return `Cache_is_closed)
    ;;

    let has_result t = Ivar.is_full t.result_ivar

    let result t =
      let%bind result = Ivar.read t.result_ivar in
      result
    ;;
  end

  (* [Resource] wraps [R] taking care that uses of [with_] don't cross paths, and that
     [close] and [close_finished] are well behaved.

     It will trigger [close] once the [max_resource_reuse] or [idle_cleanup_after] are
     exceeded. *)
  module Resource : sig
    type t

    (* [create] will immediately produce a [Resource.t] that is initially
       busy with:
       - calling [R.open_]
       - calling [immediate ~f:with_] with the resource created (if successful)

       If [R.open_] fails, this resource is immediately closed
       otherwise the resource will become idle after the initial use.

       @see [immediate]. *)
    val create
      :  ?open_timeout:Time.Span.t
      -> Config.t
      -> R.Args.t
      -> with_:(R.t -> 'a Deferred.t)
      -> t * ([> `Ok of (R.Args.t * 'a) | Delayed_failures.t] Deferred.t)

    (* [close_when_idle] forces the resource to shutdown either now or when the currently
       running [f] completes *)
    val close_when_idle : t -> unit Deferred.t

    (* [close_finished] becomes determined when this [Resource] has been closed.
       We guarantee that this will become determined, even if the underlying
       resource implementation is not well behaved. *)
    val close_finished : t -> unit Deferred.t

    (* Aquire an exclusive lock on this resource and call [f]. If [f] fails, or if the
       number of calls exceeds [max_resource_reuse] this resource will be closed.
       Otherwise this resource will be marked as idle and will close if not used again
       within a predefined timeout. *)
    val immediate
      :  t
      -> f:(R.t -> 'a Deferred.t)
      -> [ `Ok of 'a Deferred.t
         | `Resource_unavailable_until of unit Deferred.t
         | `Resource_closed
         ]

    val equal : t -> t -> bool
  end = struct
    type t =
      { uid                    : Uid.t
      ; args                   : R.Args.t
      ; resource               : R.t Set_once.t
      ; mutable status         : [ `Idle_since of Time.t
                                 | `In_use_until of unit Ivar.t
                                 | `Closing ]
      ; config                 : Config.t
      ; mutable remaining_uses : int
      ; close_finished         : unit Ivar.t
      }

    let equal a b = Uid.equal a.uid b.uid

    let close_finished t = Ivar.read t.close_finished

    let close t =
      let really_close () =
        t.status <- `Closing;
        let closed =
          match Set_once.get t.resource with
          | None -> Deferred.unit
          | Some r ->
            Monitor.try_with (fun () ->
              if R.is_closed r then
                Deferred.unit
              else
                R.close r)
            >>| function
            | Ok () -> ()
            | Error exn -> Log.Global.error !"Exception closing resource: %{Exn}" exn
        in
        Clock.with_timeout (Time.Span.of_sec 10.) closed
        >>| function `Result () | `Timeout ->
          Ivar.fill t.close_finished ()
      in
      match t.status with
      | `Closing -> close_finished t
      | `Idle_since _ -> really_close ()
      | `In_use_until done_ ->
        assert (not (Ivar.is_full done_));
        close_finished t >>> Ivar.fill done_;
        really_close ()
    ;;

    let close_when_idle t =
      match t.status with
      | `Closing -> close_finished t
      | `Idle_since _ -> close t
      | `In_use_until _ ->
        (* This will trigger a [close] when the current task completes. *)
        t.remaining_uses <- 0;
        close_finished t
    ;;

    let set_idle t =
      match t.status with
      | `Closing -> failwith "Impossible, can't set a closed resource to idle"
      | `Idle_since _ -> failwith "Impossible, already marked as idle"
      | `In_use_until done_ ->
        assert (Ivar.is_empty done_);
        if t.remaining_uses <= 0 then
          don't_wait_for (close t)
        else (
          t.status <- `Idle_since (Time.now ());
          Ivar.fill done_ ();
          Clock.after t.config.idle_cleanup_after
          >>> fun () ->
          match t.status with
          | `Closing | `In_use_until _ -> ()
          | `Idle_since now ->
            if Time.diff (Time.now ()) now >= t.config.idle_cleanup_after then
              don't_wait_for (close t))
    ;;

    let unsafe_immediate t ~f =
      match t.status with
      | `Closing ->
        failwith "Can't [unsafe_immediate] a closed resource"
      | `Idle_since _ ->
        failwith "Can't [unsafe_immediate] an idle resource"
      | `In_use_until done_ ->
        assert (Ivar.is_empty done_);
        assert (t.remaining_uses > 0);
        t.remaining_uses <- t.remaining_uses - 1;
        (* deliberately not filling [done_] here.
           It is filled in [set_idle] or [close]. *)
        Monitor.try_with (fun () -> f (Set_once.get_exn t.resource [%here]))
        >>| function
        | Ok res ->
          set_idle t;
          res
        | Error exn ->
          don't_wait_for (close t);
          raise exn
    ;;

    let immediate t ~f =
      match t.status with
      | `Closing ->
        `Resource_closed
      | `In_use_until done_ ->
        `Resource_unavailable_until (Ivar.read done_)
      | `Idle_since _ ->
        (* It is possible that [R.close] was called but [R.close_finished] is not
           determined yet. Use [R.is_closed] to prevent this race. *)
        if R.is_closed (Set_once.get_exn t.resource [%here]) then
          `Resource_closed
        else (
          t.status <- `In_use_until (Ivar.create ());
          `Ok (unsafe_immediate t ~f))
    ;;

    let create ?open_timeout config args ~with_ =
      let t =
        { uid = Uid.create ()
        ; args
        ; resource = Set_once.create ()
        ; status = `In_use_until (Ivar.create ())
        ; config
        ; remaining_uses = config.Config.max_resource_reuse
        ; close_finished = Ivar.create ()
        }
      in
      let res =
        Deferred.Or_error.try_with_join (fun () ->
          match open_timeout with
          | None -> R.open_ args
          | Some timeout ->
            let resource_ivar = Ivar.create () in
            Clock.with_timeout timeout
              (R.open_ args
               >>| fun r ->
               Ivar.fill resource_ivar r;
               r)
            >>| function
            | `Result r -> r
            | `Timeout ->
              (* In case we timeout, make sure we cleanup after ourselves *)
              Ivar.read resource_ivar >>> (function
                | Error _ -> ()
                | Ok r -> don't_wait_for (R.close r));
              Or_error.error_string ("Exceeded open timeout while creating resource"))
        >>= function
        | Ok res ->
          (* A call to [close_and_flush] might have occurred *)
          if t.remaining_uses > 0 then (
            don't_wait_for (R.close_finished res >>= fun () -> close_when_idle t);
            Set_once.set_exn t.resource [%here] res;
            unsafe_immediate t ~f:with_
            >>| fun r ->
            `Ok (args, r))
          else
            return `Cache_is_closed
        | Error err ->
          (* Ensure [close_finished] gets filled *)
          don't_wait_for (close t);
          return (`Error_opening_resource (args, err))
      in
      t, res
    ;;
  end

  (* Limit the number of concurrent [Resource.t]s globally *)
  module Global_resource_limiter : sig
    type t
    val create : Config.t -> t

    (* create a single resource, and block a slot until the resource has been cleaned
       up *)
    val create_resource
      :  ?open_timeout:Time.Span.t
      -> t
      -> R.Args.t
      -> with_:(R.t -> 'a Deferred.t)
      -> [ `Ok of Resource.t * ([> `Ok of R.Args.t * 'a | Delayed_failures.t] Deferred.t)
         | `Cache_is_closed
         | `No_resource_available_until of unit Deferred.t
         ]

    val close_and_flush : t -> unit Deferred.t
  end = struct
    type t =
      { config : Config.t
      ; throttle : unit Throttle.t
      }

    let create config =
      { config
      ; throttle =
          Throttle.create
            ~continue_on_error:true
            ~max_concurrent_jobs:config.max_resources
      }
    ;;

    let create_resource ?open_timeout { config; throttle } args ~with_ =
      if Throttle.is_dead throttle then
        `Cache_is_closed
      else if Throttle.num_jobs_running throttle < Throttle.max_concurrent_jobs throttle then (
        assert(Throttle.num_jobs_waiting_to_start throttle = 0);
        let r, v = Resource.create ?open_timeout config args ~with_ in
        don't_wait_for (Throttle.enqueue throttle (fun () -> Resource.close_finished r));
        `Ok (r, v))
      else
        `No_resource_available_until
          (Deferred.any [Throttle.capacity_available throttle; Throttle.cleaned throttle])
    ;;

    let close_and_flush t =
      Throttle.kill t.throttle;
      Throttle.cleaned t.throttle
    ;;
  end

  (* Limit the number of concurrent [Resource.t]s locally *)
  module Resource_list : sig
    type t

    val create
      :  Config.t
      -> Global_resource_limiter.t
      -> R.Args.t
      -> t

    (* [is_empty] is true iff there are no currently connected/connecting resources. *)
    val is_empty : t -> bool

    (* [close_and_flush'] will mark this resource list for removal and start tearing down
       all its resources. *)
    val close_and_flush' : t -> unit

    (* [close_finished] becomes determined after [close_and_flush'] was called and all
       resources have been closed. *)
    val close_finished : t -> unit Deferred.t

    (* [find_available_resource] and [create_resource] can be used to bypass [enqueue] in
       the case where there is an idle resource or an available slot. *)
    val find_available_resource
      :  t
      -> f:(R.t -> 'a Deferred.t)
      -> [ `Immediate of 'a Deferred.t | `None_until of unit Deferred.t ]

    val create_resource
      :  ?open_timeout:Time.Span.t
      -> t
      -> f:(R.t -> 'a Deferred.t)
      ->[> `Ok of R.Args.t * 'a
        | Delayed_failures.t
        ] Deferred.t option

    val enqueue :  t -> 'a Job.t -> unit
  end = struct
    type job =
        T : 'a Job.t -> job

    type t =
      { config                  : Config.t
      ; args                    : R.Args.t
      ; global_resource_limiter : Global_resource_limiter.t
      ; mutable resources       : Resource.t list
      ; waiting_jobs            : job Queue.t
      ; trigger_queue_manager   : unit Mvar.Read_write.t
      ; mutable close_started   : bool
      ; close_finished          : unit Ivar.t
      }

    let find_available_resource t ~f =
      let rec loop ~until = function
        | [] -> `None_until (Deferred.any until)
        | r::rs ->
          match Resource.immediate r ~f with
          | `Ok r -> `Immediate r
          | `Resource_unavailable_until u -> loop ~until:(u::until) rs
          | `Resource_closed -> loop ~until rs
      in
      loop t.resources ~until:[]
    ;;

    let create_resource ?open_timeout t ~f =
      if List.length t.resources >= t.config.max_resources_per_id then
        None
      else (
        match
          Global_resource_limiter.create_resource ?open_timeout
            t.global_resource_limiter t.args ~with_:f
        with
        | `Cache_is_closed -> None
        | `No_resource_available_until u ->
          (* Trigger when there is global capacity available *)
          upon u (Mvar.set t.trigger_queue_manager);
          None
        | `Ok (resource, response) ->
          t.resources <- resource :: t.resources;
          Resource.close_finished resource >>> (fun () ->
            t.resources <- List.filter t.resources ~f:(fun r ->
              not (Resource.equal resource r));
            (* Trigger that capacity is now available *)
            Mvar.set t.trigger_queue_manager ();
            if t.close_started && List.is_empty t.resources then
              Ivar.fill t.close_finished ());
          (* Trigger when this resource is now available. This is needed because
             [create_resource] is called from outside this module *)
          upon response (fun _ -> Mvar.set t.trigger_queue_manager ());
          Some response)
    ;;

    let allocate_resources t =
      let rec loop () =
        match Queue.peek t.waiting_jobs with
        | None -> ()
        | Some (T job) ->
          (* Skip if this job has a result already *)
          if Job.has_result job then (
            ignore (Queue.dequeue_exn t.waiting_jobs);
            loop ())
          else (
            match find_available_resource t ~f:(Job.f job) with
            | `Immediate result ->
              Job.mark_result_from_available_resource job t.args ~result;
              ignore (Queue.dequeue_exn t.waiting_jobs);
              loop ()
            | `None_until until ->
              (* Trigger when a resource is available *)
              upon until (Mvar.set t.trigger_queue_manager);
              match create_resource ?open_timeout:(Job.open_timeout job) t ~f:(Job.f job) with
              | Some result ->
                Job.mark_result_from_resource_creation job ~result;
                ignore (Queue.dequeue_exn t.waiting_jobs);
                loop ()
              | None -> ())
      in
      loop ()
    ;;

    let start_background_resource_allocator t =
      let rec loop () =
        Mvar.take t.trigger_queue_manager
        >>= fun () ->
        if t.close_started then (
          Queue.iter t.waiting_jobs ~f:(fun (T job) -> Job.mark_cache_closed job);
          Queue.clear t.waiting_jobs;
          Deferred.unit)
        else (
          allocate_resources t;
          loop ())
      in
      loop ()
    ;;

    let create config global_resource_limiter args =
      let t =
        { config
        ; args
        ; global_resource_limiter
        ; resources = []
        ; waiting_jobs = Queue.create ()
        ; trigger_queue_manager = Mvar.create ()
        ; close_started = false
        ; close_finished = Ivar.create ()
        }
      in
      don't_wait_for (start_background_resource_allocator t);
      t
    ;;

    let enqueue t job =
      Queue.enqueue t.waiting_jobs (T job);
      (* Trigger that a new job is on the queue *)
      Mvar.set t.trigger_queue_manager ();
      upon (Job.result job) (fun _ ->
        Queue.filter_inplace t.waiting_jobs ~f:(fun (T job') ->
          not (phys_same job job'));
        (* Trigger that a resource is now available *)
        Mvar.set t.trigger_queue_manager ())
    ;;

    let is_empty t = List.is_empty t.resources && Queue.is_empty t.waiting_jobs

    let close_finished t = Ivar.read t.close_finished
    let close_and_flush' t =
      if not t.close_started then (
        t.close_started <- true;
        if List.is_empty t.resources
        then (Ivar.fill t.close_finished ())
        else (
          Mvar.set t.trigger_queue_manager ();
          List.iter t.resources ~f:(fun r ->
            don't_wait_for (Resource.close_when_idle r))))
    ;;
  end

  type t =
    { config                  : Config.t
    ; global_resource_limiter : Global_resource_limiter.t
    ; cache                   : Resource_list.t R.Args.Table.t
    ; mutable close_started   : bool
    ; close_finished          : unit Ivar.t
    } [@@deriving fields]

  let get_resource_list t args =
    Hashtbl.find_or_add t.cache args ~default:(fun () ->
      Resource_list.create t.config t.global_resource_limiter args)
  ;;

  let find_any_available_resource t args_list ~f =
    List.find_map args_list ~f:(fun args ->
      let res_list = get_resource_list t args in
      match Resource_list.find_available_resource res_list ~f with
      | `Immediate res -> Some (args, res)
      | `None_until _ -> None)
  ;;

  let create_any_resource ?open_timeout t args_list ~f =
    List.find_map args_list ~f:(fun args ->
      let res_list = get_resource_list t args in
      Resource_list.create_resource ?open_timeout res_list ~f)
  ;;

  let enqueue_all ?open_timeout t ~give_up args_list ~f =
    let job = Job.create ?open_timeout ~give_up ~f in
    List.iter args_list ~f:(fun args ->
      let res_list = get_resource_list t args in
      Resource_list.enqueue res_list job);
    Job.result job
  ;;

  let with_any' ?open_timeout ?(give_up=Deferred.never()) t args_list ~f =
    if t.close_started then
      return `Cache_is_closed
    else (
      match find_any_available_resource t args_list ~f with
      | Some (args, res) ->
        let%map res = res in
        `Ok (args, res)
      | None ->
        match create_any_resource ?open_timeout t args_list ~f with
        | Some res -> res
        | None ->
          if Deferred.is_determined give_up then
            return `Gave_up_waiting_for_resource
          else (
            enqueue_all ?open_timeout ~give_up t args_list ~f))
  ;;

  let with_any ?open_timeout ?give_up t args ~f =
    match%map with_any' ?open_timeout t ?give_up args ~f with
    | `Ok args_and_res -> Ok args_and_res
    | `Error_opening_resource (args, err) ->
      let tag = sprintf !"Error creating required resource: %{R.Args#hum}" args in
      Error (Error.tag ~tag err)
    | `Cache_is_closed ->
      Or_error.error_string "Cache is closed"
    | `Gave_up_waiting_for_resource ->
      Or_error.error_string "Gave up waiting for resource"
  ;;

  let with_ ?open_timeout ?give_up t args ~f =
    match%map with_any ?open_timeout ?give_up t [args] ~f with
    | Ok (_args, res) -> Ok res
    | Error e -> Error e
  ;;

  let with_' ?open_timeout ?give_up t args ~f =
    match%map with_any' ?open_timeout ?give_up t [args] ~f with
    | `Ok (_args, res) -> `Ok res
    | `Error_opening_resource (_args, err) -> `Error_opening_resource err
    | `Cache_is_closed -> `Cache_is_closed
    | `Gave_up_waiting_for_resource -> `Gave_up_waiting_for_resource
  ;;

  let init ~config =
    let t =
      { config
      ; global_resource_limiter = Global_resource_limiter.create config
      ; cache = R.Args.Table.create ()
      ; close_started = false
      ; close_finished = Ivar.create ()
      }
    in
    Clock.every ~stop:(Ivar.read t.close_finished) config.idle_cleanup_after
      (fun () ->
         Hashtbl.filter_inplace t.cache ~f:(fun d ->
           if Resource_list.is_empty d then (
             Resource_list.close_and_flush' d;
             false)
           else true));
    t
  ;;

  let close_and_flush t =
    if not t.close_started then (
      t.close_started <- true;
      Deferred.all_ignore
        (Global_resource_limiter.close_and_flush t.global_resource_limiter
         :: List.map (Hashtbl.data t.cache) ~f:(fun r ->
           Resource_list.close_and_flush' r;
           Resource_list.close_finished r))
      >>| fun () ->
      Ivar.fill t.close_finished ())
    else
      Ivar.read t.close_finished
  ;;

  let close_started t = t.close_started
  let close_finished t = Ivar.read t.close_finished
end
