open Core
open Async.Std
open Async_extended.Std

module Mutex = Async_mutex

type t =
  { waiting : (unit -> unit Deferred.t) Bag.t
  ; mutable resources : Mutex.t list
  ; mutable throttle : Mutex.t Throttle.t
  }

let create ~max_concurrent_jobs =
  let resources =
    List.init max_concurrent_jobs ~f:(fun _ -> Mutex.create ())
  in
  let waiting = Bag.create () in
  let throttle = Throttle.create_with ~continue_on_error:true resources in
  { waiting; resources; throttle }

let is_dead t =
  Throttle.is_dead t.throttle

let enqueue_elt t elt =
  let f resource =
    let f = Bag.Elt.value elt in
    Bag.remove t.waiting elt;
    Mutex.lock resource
    >>= fun () ->
    f ()
    >>| fun () ->
    Mutex.unlock resource;
  in
  don't_wait_for begin
    Throttle.enqueue' t.throttle f
    >>= function
    | `Ok () | `Aborted -> return ()
    | `Raised exn -> raise exn
  end

let enqueue t f =
  if is_dead t
  then failwith "Adjustable_throttle.enqueue: throttle is dead"
  else enqueue_elt t (Bag.add t.waiting f)

let set_max_concurrent_jobs t max_concurrent_jobs =
  if Throttle.is_dead t.throttle then ()
  else begin
    let need_more = max_concurrent_jobs - List.length t.resources in
    if need_more > 0
    then begin
      let more = List.init need_more ~f:(fun _ -> Mutex.create ()) in
      t.resources <- more @ t.resources;
    end;
    let resources = List.take t.resources max_concurrent_jobs in
    Throttle.kill t.throttle;
    t.throttle <- Throttle.create_with ~continue_on_error:true resources;
    Bag.iter_elt t.waiting ~f:(enqueue_elt t)
  end

let kill_and_flush t =
  Throttle.kill t.throttle;
  Deferred.List.iter t.resources ~f:Mutex.lock

let%test_module _ =
  (module struct
    let output = ref []

    let check_output expect =
      [%test_result: Int.Set.t] (Int.Set.of_list !output) ~expect:(Int.Set.of_list expect);
      output := []

    module Job = struct
      type t =
        { output : int
        ; enabled : unit Ivar.t
        }

      let create output =
        { output; enabled = Ivar.create () }

      let enable t =
        Ivar.fill t.enabled ()

      let run t () =
        Ivar.read t.enabled
        >>| fun () ->
        output := !output @ [t.output]
    end

    let%test_unit _ =
      Thread_safe.block_on_async_exn (fun () ->
        (* No jobs run if max_concurrent_jobs is 0. *)
        let t = create ~max_concurrent_jobs:0 in
        let j1 = Job.create 1 in
        let j2 = Job.create 2 in
        enqueue t (Job.run j1);
        enqueue t (Job.run j2);
        Job.enable j1;
        Job.enable j2;
        Scheduler.yield ()
        >>= fun () ->
        check_output [];

        (* Jobs do run if max_concurrent_jobs is 1. *)
        set_max_concurrent_jobs t 1;
        Scheduler.yield ()
        >>= fun () ->
        check_output [1; 2];

        (* Jobs run sequentially if max_concurrent_jobs is 1. *)
        let j3 = Job.create 3 in
        let j4 = Job.create 4 in
        enqueue t (Job.run j3);
        enqueue t (Job.run j4);
        (* j4 will not run since j3 will be blocking it. *)
        Job.enable j4;
        Scheduler.yield ()
        >>= fun () ->
        check_output [];

        (* Adding capacity allows j4 to run. *)
        set_max_concurrent_jobs t 2;
        Scheduler.yield ()
        >>= fun () ->
        check_output [4];
        Job.enable j3;
        Scheduler.yield ()
        >>= fun () ->
        check_output [3];

        (* Set it to 0 again. *)
        set_max_concurrent_jobs t 0;
        let j5 = Job.create 5 in
        let j6 = Job.create 6 in
        enqueue t (Job.run j5);
        enqueue t (Job.run j6);
        Job.enable j5;
        Job.enable j6;
        Scheduler.yield ()
        >>| fun () ->
        check_output [];
      )
  end)
