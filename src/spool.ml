module Stable = struct
  open Core.Core_stable
  open Async_smtp_types.Async_smtp_types_stable
  module Time = Time_float_unix.Stable
  module Message = Message.Stable
  module Quarantine_reason = Quarantine_reason.Stable
  module Message_id = Message.Id

  module Send_info = struct
    module V1 = struct
      type t =
        [ `All_messages
        | `Frozen_only
        | `Some_messages of Message_id.V1.t list
        ]
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 20aa2a292e80101031f8644e74731254 |}]
      ;;
    end
  end

  module Recover_info = struct
    module V2 = struct
      type t =
        { msgs : Message_id.V1.t list
        ; from : [ `Removed | `Quarantined ]
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 621f27a788c7dfa64d1c6455e1d84713 |}]
      ;;
    end
  end

  module Spooled_message_info = struct
    module V3 = struct
      type t =
        { message : Message.V4.t
        ; file_size : Byte_units.V1.t option
        ; envelope : Smtp_envelope.V2.t option
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4f0e4c8fae172cc9a1c2af02d24047ff |}]
      ;;
    end

    module V2 = struct
      type t =
        { message : Message.V3.t
        ; file_size : Byte_units.V1.t option
        ; envelope : Smtp_envelope.V2.t option
        }
      [@@deriving bin_io, stable_record ~version:V3.t ~modify:[ message ]]

      let of_v3 = of_V3_t ~modify_message:Message.V4.to_v3
      let to_v3 = to_V3_t ~modify_message:Message.V4.of_v3

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 6e3baf8e5ffb4510a8f942fe92cd9bba |}]
      ;;
    end
  end

  module Status = struct
    module V3 = struct
      type t = Spooled_message_info.V3.t list [@@deriving bin_io]

      include (
        Streamable.Of_list_rpc (struct
          include Spooled_message_info.V3
          include Streamable.Of_atomic_rpc (Spooled_message_info.V3)
        end) :
          Streamable.S_rpc with type t := t)

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8b4a0fae72e23152fa35fc5d804b0e32 |}]
      ;;
    end

    module V2 = struct
      type t = Spooled_message_info.V2.t list [@@deriving bin_io]

      let to_v3 = Core.List.map ~f:Spooled_message_info.V2.to_v3
      let of_v3 = Core.List.map ~f:Spooled_message_info.V2.of_v3

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a50476da0ab450c17d23516dd893b378 |}]
      ;;
    end
  end

  module Event = struct
    module V1 = struct
      type spool_event =
        [ `Presend of [ `Send_now | `Send_at of Time.V1.t | `Freeze | `Remove ]
        | `Spooled
        | `Delivered
        | `Frozen
        | `Removed
        | `Unfrozen
        | `Recovered of [ `From_quarantined | `From_removed ]
        | `Quarantined of [ `Reason of Quarantine_reason.V1.t ]
        | `Delayed of Time.V1.t
        | `Delayed_rate_limited of Time.V1.t
        ]
        * Message_id.V1.t
        * Smtp_envelope.Info.V2.t
      [@@deriving bin_io, sexp]

      type t = Time.V1.t * [ `Spool_event of spool_event | `Ping ]
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 39b4b08898af01d27f2aea892c9ff159 |}]
      ;;
    end
  end
end

open Core
open Poly
open Async
open Async_smtp_types
module Time = Time_float_unix
module Config = Spool_config
module Message_id = Message.Id
module Message_queue = Message.Queue
module Log = Mail_log

module Event = struct
  module T = struct
    type spool_event =
      [ `Presend of [ `Send_now | `Send_at of Time.t | `Freeze | `Remove ]
      | `Spooled
      | `Delivered
      | `Frozen
      | `Removed
      | `Unfrozen
      | `Recovered of [ `From_quarantined | `From_removed ]
      | `Quarantined of [ `Reason of Quarantine_reason.t ]
      | `Delayed of Time.t
      | `Delayed_rate_limited of Time.t
      ]
      * Message_id.t
      * Smtp_envelope.Info.t
    [@@deriving sexp_of, compare]

    type t = Time.t * [ `Spool_event of spool_event | `Ping ]
    [@@deriving sexp_of, compare]
  end

  include T
  include Comparable.Make_plain (T)

  let to_string (_rcvd_time, happening) =
    let without_time =
      match happening with
      | `Ping -> "ping"
      | `Spool_event (spool_event, id, _info) ->
        let event_string =
          match spool_event with
          | `Presend _ -> "presend"
          | `Spooled -> "spooled"
          | `Frozen -> "frozen"
          | `Removed -> "removed"
          | `Unfrozen -> "unfrozen"
          | `Delivered -> "delivered"
          | `Recovered _from -> "recovered"
          | `Quarantined _reason -> "quarantined"
          | `Delayed _ -> "delayed"
          | `Delayed_rate_limited _ -> "delayed-rate-limited"
        in
        Message_id.to_string id ^ " " ^ event_string
    in
    without_time
  ;;

  let event_gen
    event
    ~(time : [ `Now | `Of_msg of Message.t -> Time.t ])
    ~here
    ~log
    event_stream
    spooled_msg
    =
    let id = Message.id spooled_msg in
    let t =
      let time =
        match time with
        | `Now -> Time.now ()
        | `Of_msg f -> f spooled_msg
      in
      let info = Message.envelope_info spooled_msg in
      time, `Spool_event (event, id, info)
    in
    Log.debug
      log
      (lazy
        (Log.Message.create
           ~here
           ~flows:(Message.flows spooled_msg)
           ~component:[ "spool"; "event-stream" ]
           ~spool_id:(Message_id.to_string id)
           ~tags:[ "event", sprintf !"%{sexp:t}" t ]
           "writing event"));
    Bus.write event_stream t
  ;;

  let presend send_at = event_gen (`Presend send_at) ~time:(`Of_msg Message.spool_date)
  let spooled = event_gen `Spooled ~time:(`Of_msg Message.spool_date)
  let delivered = event_gen `Delivered ~time:`Now
  let frozen = event_gen `Frozen ~time:`Now
  let removed = event_gen `Removed ~time:`Now
  let unfrozen = event_gen `Unfrozen ~time:`Now
  let quarantined reason = event_gen (`Quarantined reason) ~time:`Now
  let recovered from_where = event_gen (`Recovered from_where) ~time:`Now
  let delayed ~at = event_gen (`Delayed at) ~time:`Now
  let delayed_rate_limited ~at = event_gen (`Delayed_rate_limited at) ~time:`Now
end

type t =
  { spool : Message_spool.t
  ; active_send_jobs : int Message_id.Table.t
  ; messages : Message.t Message_id.Table.t
  ; event_stream : (Event.t -> unit) Bus.Read_write.t
  ; client_cache : Client_cache.t
  ; log : Log.t
  ; mutable is_killed : bool
  ; killed_and_flushed : unit Ivar.t
  ; presend :
      log:Log.t
      -> Message.t
      -> [ `Send_now | `Send_at of Time_float.t | `Freeze | `Remove ] Deferred.t
  ; on_error :
      log:Log.t
      -> load_envelope:(unit -> Smtp_envelope.t Deferred.Or_error.t)
      -> Message.t
      -> Smtp_reply.t
      -> [ `Fail_permanently | `Try_later | `Try_later_rate_limited | `Done ] Deferred.t
  }
[@@deriving fields ~getters ~iterators:create]

let add_message t msg =
  let id = Message.id msg in
  match Hashtbl.add t.messages ~key:id ~data:msg with
  | `Ok -> ()
  | `Duplicate ->
    Log.error
      t.log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows:(Message.flows msg)
           ~component:[ "spool"; "admin" ]
           ~spool_id:(Message_id.to_string id)
           "Message already in spool"))
;;

let remove_message t msg =
  let id = Message.id msg in
  if Hashtbl.mem t.messages id
  then Hashtbl.remove t.messages id
  else
    Log.error
      t.log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows:(Message.flows msg)
           ~component:[ "spool"; "admin" ]
           ~spool_id:(Message_id.to_string id)
           "Trying to remove message that is not in spool"))
;;

let with_event_writer ~here spool spooled_msg ~f =
  f ~here ~log:spool.log spool.event_stream spooled_msg
;;

let spooled_event = with_event_writer ~f:Event.spooled
let delivered_event = with_event_writer ~f:Event.delivered
let frozen_event = with_event_writer ~f:Event.frozen
let removed_event = with_event_writer ~f:Event.removed
let unfrozen_event = with_event_writer ~f:Event.unfrozen
let recovered_event t from_where = with_event_writer t ~f:(Event.recovered from_where)
let quarantined_event t reason = with_event_writer t ~f:(Event.quarantined reason)
let delayed_event ~at = with_event_writer ~f:(Event.delayed ~at)
let delayed_rate_limited_event ~at = with_event_writer ~f:(Event.delayed_rate_limited ~at)

let internal_create ~presend ~on_error ~config ~log () : t Deferred.Or_error.t =
  let spool_dir = Config.spool_dir config in
  Log.info
    log
    (lazy
      (Log.Message.create
         ~here:[%here]
         ~flows:Log.Flows.none
         ~component:[ "spool" ]
         ~tags:[ "spool-dir", spool_dir ]
         "initializing"));
  Message_spool.create spool_dir
  >>|? fun spool ->
  let event_stream =
    Bus.create_exn
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:Error.raise
      ()
  in
  Clock.every (sec 30.) (fun () ->
    Log.debug
      log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows:Log.Flows.none
           ~component:[ "spool"; "event-stream" ]
           "ping"));
    Bus.write event_stream (Time.now (), `Ping));
  let active_send_jobs = Message_id.Table.create () in
  let messages = Message_id.Table.create () in
  let client_cache =
    Client_cache.init
      ~log
      ~component:[ "spool"; "client_cache" ]
      ~cache_config:
        (config.connection_cache
         |> Resource_cache.Address_config.Stable.V1.to_v2
         |> Resource_cache.Address_config.Stable.V2.to_v3)
      ~client_config:config.client
      ~connection_cache_warming:config.connection_cache_warming
      ()
  in
  let killed_and_flushed = Ivar.create () in
  (* Wrap the [presend] and [on_error] callbacks to perform some logging and error
     handling by default. *)
  let presend ~log message =
    let component = [ "spool"; "plugin-callback"; "presend" ] in
    let flows = Message.flows message in
    let%map decision =
      presend ~log:(Log.with_flow_and_component ~flows ~component log) message
    in
    Log.info
      log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~component
           ~recipients:
             (Message.remaining_recipients message
              |> List.map ~f:(fun email -> `Email email))
           ~spool_id:(Message.id message |> Message.Id.to_string)
           ~flows
           (Sexp.to_string
              [%sexp
                (decision
                 : [ `Send_now | `Send_at of Time_float_unix.t | `Freeze | `Remove ])])));
    Event.presend decision ~here:[%here] ~log event_stream message;
    decision
  in
  let on_error ~log ~load_envelope message reply =
    let component = [ "spool"; "plugin-callback"; "on-error" ] in
    let flows = Message.flows message in
    let recipients =
      Message.remaining_recipients message |> List.map ~f:(fun email -> `Email email)
    in
    let spool_id = Message.id message |> Message.Id.to_string in
    match%map
      Deferred.Or_error.try_with_join (fun () ->
        on_error
          ~log:(Log.with_flow_and_component ~flows ~component log)
          ~load_envelope
          message
          reply)
    with
    | Ok decision ->
      Log.info
        log
        (lazy
          (Log.Message.create
             ~here:[%here]
             ~component
             ~recipients
             ~spool_id
             ~reply
             ~flows
             (Sexp.to_string
                [%sexp
                  (decision
                   : [ `Fail_permanently | `Try_later | `Try_later_rate_limited | `Done ])])));
      decision
    | Error error ->
      Log.error
        log
        (lazy
          (Log.Message.of_error
             ~here:[%here]
             ~component
             ~recipients
             ~spool_id
             ~reply
             ~flows
             error));
      (* Default to try later upon errors to give us more chances to handle the reject. *)
      `Try_later
  in
  Fields.create
    ~spool
    ~active_send_jobs
    ~messages
    ~event_stream
    ~client_cache
    ~log
    ~is_killed:false
    ~killed_and_flushed
    ~presend
    ~on_error
;;

let add_active_send_job t msgid =
  Hashtbl.update t.active_send_jobs msgid ~f:(function
    | None -> 1
    | Some x -> x + 1)
;;

let remove_active_send_job ~flows t msgid =
  Hashtbl.change t.active_send_jobs msgid ~f:(function
    | None ->
      Log.error
        t.log
        (lazy
          (Log.Message.create
             ~here:[%here]
             ~flows
             ~component:[ "spool"; "remove_active_send_job" ]
             ~spool_id:(Message_id.to_string msgid)
             "No active job to remove"));
      None
    | Some x ->
      let res = x - 1 in
      if res = 0 then None else Some res);
  if t.is_killed && Hashtbl.length t.active_send_jobs = 0
  then Ivar.fill_if_empty t.killed_and_flushed ()
;;

let spool_is_killed_error = Error.createf "Spool is killed."

let with_spooled_message' t message ~f =
  if t.is_killed
  then return `Spool_is_killed
  else (
    let msgid = Message.id message in
    let flows = Message.flows message in
    add_active_send_job t msgid;
    let%bind result = f () in
    remove_active_send_job ~flows t msgid;
    return (`Ok result))
;;

let with_spooled_message t message ~f =
  match%bind with_spooled_message' t message ~f with
  | `Spool_is_killed -> return (Error spool_is_killed_error)
  | `Ok result -> return result
;;

let rec enqueue ?don't_re_enqueue ?at t spooled_msg =
  let msgid = Message.id spooled_msg in
  let error_opt = function
    | None -> return (Ok ())
    | Some error -> return (Error error)
  in
  let%bind () =
    match at with
    | None -> Deferred.unit
    | Some time -> Clock.at time
  in
  Log.debug
    t.log
    (lazy
      (Log.Message.create
         ~here:[%here]
         ~flows:(Message.flows spooled_msg)
         ~component:[ "spool"; "throttle" ]
         ~spool_id:(Message_id.to_string msgid)
         "enqueuing message"));
  let status = Message.status spooled_msg in
  match status with
  | `Delivered | `Sending | `Frozen | `Removed | `Quarantined _ ->
    (* Only actually call [send] if the status is [`Send_now] or [`Send_at _]. Between the
       time an async job was scheduled and the time it gets run, the status of the message
       can change. We make sure not to try to send if we have manually intervened, causing
       a permanent status change. *)
    Deferred.Or_error.errorf
      !"Not attempting delivery for status %{sexp: Message.Status.t}"
      status
  | `Send_now | `Send_at _ ->
    (match%bind
       with_spooled_message' t spooled_msg ~f:(fun () ->
         Message_spool.send
           spooled_msg
           ~log:t.log
           ~client_cache:t.client_cache
             (* Curry [spooled_message] to [on_error] and [presend].

                note: a [Message.t] is mutable, so it shouldn't actually matter if we
                curry here or pass it as an argument in [Message_spool.send]. *)
           ~on_error:(t.on_error spooled_msg)
           ~presend:(t.presend spooled_msg))
     with
     | `Spool_is_killed -> return (Error spool_is_killed_error)
     | `Ok (Error error) ->
       Log.error
         t.log
         (lazy
           (Log.Message.of_error
              ~here:[%here]
              ~flows:(Message.flows spooled_msg)
              ~component:[ "spool"; "send" ]
              ~spool_id:(Message_id.to_string msgid)
              error));
       return (Error error)
     | `Ok (Ok (((`Delayed_to at | `Delayed_to_rate_limited at) as delayed), maybe_error))
       ->
       (match delayed with
        | `Delayed_to at -> delayed_event ~at ~here:[%here] t spooled_msg
        | `Delayed_to_rate_limited at ->
          delayed_rate_limited_event ~at ~here:[%here] t spooled_msg);
       (match don't_re_enqueue with
        | Some () -> error_opt maybe_error
        | None ->
          (* Use the Async queue to store messages waiting for retry. *)
          enqueue ~at t spooled_msg)
     | `Ok (Ok (((`Delivered | `Frozen | `Removed) as spool_send_result), maybe_error)) ->
       (match spool_send_result with
        | `Delivered ->
          remove_message t spooled_msg;
          delivered_event t ~here:[%here] spooled_msg
        | `Frozen -> frozen_event t ~here:[%here] spooled_msg
        | `Removed ->
          remove_message t spooled_msg;
          removed_event ~here:[%here] t spooled_msg);
       error_opt maybe_error)
;;

let schedule t msg =
  match Message.status msg with
  | `Frozen | `Removed | `Quarantined _ -> ()
  | (`Sending | `Delivered) as status ->
    failwithf
      !"Unexpected status %{sexp:Message.Status.t} when trying to schedule message \
        %{Message_id}"
      status
      (Message.id msg)
      ()
  | `Send_at at ->
    Log.info
      t.log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows:(Message.flows msg)
           ~component:[ "spool"; "queue" ]
           ~spool_id:(Message_id.to_string (Message.id msg))
           ~tags:[ "time", sprintf !"%{sexp:Time.t}" at ]
           "send later"));
    don't_wait_for (enqueue ~at t msg >>| (ignore : unit Or_error.t -> unit))
  | `Send_now ->
    Log.info
      t.log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows:(Message.flows msg)
           ~component:[ "spool"; "queue" ]
           ~spool_id:(Message_id.to_string (Message.id msg))
           "send now"));
    don't_wait_for (enqueue t msg >>| (ignore : unit Or_error.t -> unit))
;;

let uncheckout_all_entries t =
  let%map `Recovered recovered, `Errors error =
    Message_spool.uncheckout_all_entries t.spool
  in
  List.iter recovered ~f:(fun spool_id ->
    Log.info
      t.log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows:Log.Flows.none
           ~component:[ "spool"; "init" ]
           ~spool_id
           "recovered from checkout")));
  match error with
  | None -> ()
  | Some e ->
    Log.error
      t.log
      (lazy
        (Log.Message.of_error
           ~here:[%here]
           ~flows:Log.Flows.none
           ~component:[ "spool"; "init" ]
           e))
;;

let load t =
  Message_spool.ls t.spool [ Message_queue.Active; Message_queue.Frozen ]
  >>=? fun entries ->
  Log.debug
    t.log
    (lazy
      (Log.Message.create
         ~here:[%here]
         ~flows:Log.Flows.none
         ~component:[ "spool"; "init" ]
         ~tags:[ "entries", sprintf !"%{sexp:Message_spool.Entry.t list}" entries ]
         "found files"));
  let%bind () =
    Deferred.List.iter entries ~how:`Parallel ~f:(fun entry ->
      match%map Message_spool.Entry.to_message entry with
      | Error e ->
        Log.error
          t.log
          (lazy
            (Log.Message.of_error
               ~here:[%here]
               ~flows:Log.Flows.none
               ~component:[ "spool"; "init" ]
               ~tags:
                 [ "entries", sprintf !"%{sexp:Message_spool.Entry.t list}" [ entry ] ]
               e))
      | Ok msg ->
        Log.info
          t.log
          (lazy
            (Log.Message.create
               ~here:[%here]
               ~flows:Log.Flows.none
               ~component:[ "spool"; "init" ]
               ~tags:
                 [ "entries", sprintf !"%{sexp:Message_spool.Entry.t list}" [ entry ] ]
               ~spool_id:(Message_id.to_string (Message.id msg))
               "loaded"));
        add_message t msg;
        schedule t msg)
  in
  return (Ok ())
;;

let create ?presend ?on_error ~config ~log () =
  let presend =
    match presend with
    | Some f ->
      fun ~log msg ->
        let%map d = f ~log msg in
        (d :> [ `Send_now | `Send_at of Time.t | `Freeze | `Remove ])
    | None -> fun ~log:_ _message -> return `Send_now
  in
  let on_error =
    match on_error with
    | Some f ->
      fun ~log ~load_envelope msg reply ->
        let%map d = f ~log ~load_envelope msg reply in
        (d
          :> [ `Fail_permanently | `Try_later | `Try_later_rate_limited | `Done ]
               Or_error.t)
    | None ->
      fun ~log:_ ~load_envelope:_ _message reply ->
        if Smtp_reply.is_permanent_error reply
        then return (Ok `Fail_permanently)
        else return (Ok `Try_later)
  in
  internal_create ~presend ~on_error ~config ~log ()
  >>=? fun t ->
  let%bind () = uncheckout_all_entries t in
  load t >>=? fun () -> return (Ok t)
;;

let add
  t
  ?(initial_status = `Send_now)
  ?(set_related_ids = false)
  ~flows
  ~original_msg
  envelope_batches
  =
  Deferred.Or_error.List.concat_map
    envelope_batches
    ~how:`Parallel
    ~f:(fun envelope_batch ->
      Message_spool.enqueue
        t.spool
        ~log:t.log
        ~flows:(Log.Flows.extend flows `Outbound_envelope)
        ~initial_status:(initial_status :> Message.Status.t)
        ~set_related_ids
        envelope_batch
        ~original_msg
      >>|? fun spooled_msgs ->
      List.map spooled_msgs ~f:(fun (msg, envelope) ->
        add_message t msg;
        spooled_event t ~here:[%here] msg;
        don't_wait_for (enqueue t msg >>| (ignore : unit Or_error.t -> unit));
        Message.id msg, envelope))
;;

let quarantine t ~reason ~flows ~original_msg envelope_batches =
  Deferred.Or_error.List.concat_map
    envelope_batches
    ~how:`Parallel
    ~f:(fun envelope_batch ->
      Message_spool.enqueue
        t.spool
        ~log:t.log
        ~flows:(Log.Flows.extend flows `Outbound_envelope)
        ~initial_status:(`Quarantined reason)
        ~set_related_ids:false
        envelope_batch
        ~original_msg
      >>|? fun quarantined_msgs ->
      List.map quarantined_msgs ~f:(fun (msg, envelope) ->
        quarantined_event ~here:[%here] t (`Reason reason) msg;
        Message.id msg, envelope))
;;

let kill_and_flush ?(timeout = Deferred.never ()) t =
  t.is_killed <- true;
  if Hashtbl.length t.active_send_jobs = 0 then Ivar.fill_if_empty t.killed_and_flushed ();
  let finished =
    Deferred.all_unit
      [ Client_cache.close_and_flush t.client_cache; Ivar.read t.killed_and_flushed ]
  in
  match%bind
    Deferred.choose
      [ Deferred.choice timeout (fun () -> `Timeout)
      ; Deferred.choice finished (fun () -> `Finished)
      ]
  with
  | `Finished -> Deferred.Or_error.ok_unit
  | `Timeout -> Deferred.Or_error.error_string "Timed out killing and flushing spool"
;;

let get_msg t id ~f =
  match Hashtbl.find t.messages id with
  | None -> return (Or_error.error_string "Not found")
  | Some spooled_msg -> f spooled_msg
;;

let freeze t ids =
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    get_msg t id ~f:(fun spooled_msg ->
      with_spooled_message t spooled_msg ~f:(fun () ->
        Message_spool.freeze spooled_msg ~log:t.log)
      >>=? fun () ->
      frozen_event t ~here:[%here] spooled_msg;
      Deferred.Or_error.ok_unit))
;;

module Send_info = struct
  type t =
    [ `All_messages
    | `Frozen_only
    | `Some_messages of Message_id.t list
    ]
end

let send_msgs ?(retry_intervals = []) t ids =
  let do_send ?event msg =
    with_spooled_message t msg ~f:(fun () ->
      Message_spool.mark_for_send_now ~retry_intervals msg ~log:t.log)
    >>=? fun () ->
    Option.iter event ~f:(fun event -> event t msg);
    enqueue ~don't_re_enqueue:() t msg
  in
  Deferred.List.map ids ~how:`Parallel ~f:(fun id ->
    get_msg t id ~f:(fun msg ->
      match Message.status msg with
      | `Frozen -> do_send ~event:(unfrozen_event ~here:[%here]) msg
      | `Send_at _ ->
        (* This will enqueue the message without changing what was previously queued, so
           we will attempt to deliver twice. It's ok, [Message.send] can deal with this. *)
        do_send msg
      | `Sending | `Delivered | `Send_now | `Removed | `Quarantined _ -> return (Ok ()))
    >>| Or_error.tag ~tag:(Message_id.to_string id))
  >>| Or_error.combine_errors_unit
;;

let remove t ids =
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    get_msg t id ~f:(fun spooled_msg ->
      (match Message.status spooled_msg with
       | `Frozen ->
         with_spooled_message t spooled_msg ~f:(fun () ->
           Message_spool.remove spooled_msg ~log:t.log)
       | `Send_at _ | `Send_now | `Sending | `Delivered | `Removed | `Quarantined _ ->
         Error (Error.of_string "Cannot remove a message that is not currently frozen")
         |> return)
      >>=? fun () ->
      remove_message t spooled_msg;
      removed_event t ~here:[%here] spooled_msg;
      Deferred.Or_error.ok_unit))
;;

module Recover_info = struct
  type t = Stable.Recover_info.V2.t =
    { msgs : Message_id.t list
    ; from : [ `Removed | `Quarantined ]
    }
end

let recover t (info : Recover_info.t) =
  let from_queue, from_queue' =
    match info.from with
    | `Quarantined -> Message_queue.Quarantine, `From_quarantined
    | `Removed -> Message_queue.Removed, `From_removed
  in
  Deferred.Or_error.List.iter info.msgs ~how:`Parallel ~f:(fun id ->
    let name = Message_id.to_string id in
    let entry = Message_spool.Entry.create t.spool from_queue ~name in
    match%bind Message_spool.Entry.to_message entry with
    | Error e ->
      let e = Error.tag e ~tag:"Failed to recover message" in
      Log.info
        t.log
        (lazy
          (Log.Message.of_error
             ~here:[%here]
             ~flows:Log.Flows.none
             ~component:[ "spool"; "admin" ]
             ~spool_id:(Message_id.to_string id)
             e));
      return (Error e)
    | Ok msg ->
      with_spooled_message t msg ~f:(fun () -> Message_spool.freeze msg ~log:t.log)
      >>=? fun () ->
      add_message t msg;
      recovered_event t ~here:[%here] from_queue' msg;
      Deferred.Or_error.ok_unit)
;;

let send_all ?retry_intervals ?(frozen_only = false) t =
  Hashtbl.data t.messages
  |> List.filter_map ~f:(fun m ->
    match Message.status m, frozen_only with
    | `Frozen, _ -> Some (Message.id m)
    | `Send_at _, false -> Some (Message.id m)
    | `Send_now, _
    | `Send_at _, true
    | `Sending, _
    | `Delivered, _
    | `Removed, _
    | `Quarantined _, _ -> None)
  |> send_msgs ?retry_intervals t
;;

let send ?retry_intervals t send_info =
  match send_info with
  | `All_messages -> send_all ?retry_intervals t
  | `Frozen_only -> send_all ?retry_intervals ~frozen_only:true t
  | `Some_messages msgids -> send_msgs ?retry_intervals t msgids
;;

module Spooled_message_info = struct
  type t = Stable.Spooled_message_info.V3.t =
    { message : Message.t
    ; file_size : Byte_units.t option
    ; envelope : Smtp_envelope.t option
    }
  [@@deriving fields ~getters, sexp_of]

  let sp f t = f t.message
  let id = sp Message.id
  let spool_date = sp Message.spool_date
  let last_relay_attempt = sp Message.last_relay_attempt
  let parent_id = sp Message.parent_id
  let envelope_info = sp Message.envelope_info
  let status = sp Message.status

  (* Internal *)
  let time_on_spool = sp Message.time_on_spool
  let next_hop_choices = sp Message.next_hop_choices
end

module Status = struct
  type t = Spooled_message_info.t list [@@deriving sexp_of]

  let sort t =
    let cmp a b =
      let f = Spooled_message_info.time_on_spool in
      Time.Span.compare (f a) (f b)
    in
    List.sort ~compare:cmp t
  ;;

  let to_string_exim t =
    (* From `man 8 exim`:

       Each message on the queue is display as in the following example:

       25m 2.9K 0t5C6f-0000c8-00 <alice@wonderland.fict.example>
       red.king@looking-glass.fict.example <other addresses>

       The first line contains the length of time the message has been on the queue (in
       this case 25 minutes), the size of the message (2.9K), the unique local identifier
       for the message, and the message sender, as contained in the envelope. For bounce
       messages, the sender address is empty, and appears as "<>". If the message was
       submitted locally by an untrusted user who overrode the default sender address, the
       user’s login name is shown in parentheses before the sender address.

       If the message is frozen (attempts to deliver it are suspended) then the text "***
       frozen ***" is displayed at the end of this line.

       The recipients of the message (taken from the envelope, not the headers) are
       displayed on subsequent lines. Those addresses to which the message has already
       been delivered are marked with the letter D. If an original address gets expanded
       into several addresses via an alias or forward file, the original is displayed with
       a D only when deliveries for all of its child addresses are complete. *)
    let module S = Spooled_message_info in
    let msg_to_string msg =
      let frozen_text =
        match S.status msg with
        | `Send_at _ | `Send_now | `Sending | `Delivered | `Removed | `Quarantined _ -> ""
        | `Frozen -> "*** frozen ***"
      in
      let time = S.time_on_spool msg |> Time.Span.to_short_string in
      let size =
        match S.file_size msg with
        | None -> ""
        | Some size ->
          Byte_units.kilobytes size
          |> Float.to_string_hum ~decimals:1 ~delimiter:'.' ~strip_zero:true
          |> fun s -> s ^ "K"
      in
      match S.envelope msg with
      | Some envelope ->
        let recipients =
          Smtp_envelope.string_recipients envelope |> String.concat ~sep:"\n"
        in
        sprintf
          !"%4s %5s %{Message_id} <%s> %s\n           %s\n"
          time
          size
          (S.id msg)
          (Smtp_envelope.string_sender envelope)
          frozen_text
          recipients
      | None ->
        sprintf
          !"%4s %5s %{Message_id} %s (sender and recipients hidden)\n"
          time
          size
          (S.id msg)
          frozen_text
    in
    List.map (sort t) ~f:msg_to_string |> String.concat ~sep:"\n"
  ;;

  let to_formatted_string t ~format =
    let format : [ `Sexp | `Exim | `Id | `Ascii_table_with_max_width of int ] =
      match format with
      | `Ascii_table -> `Ascii_table_with_max_width 180
      | (`Sexp | `Exim | `Id | `Ascii_table_with_max_width _) as format -> format
    in
    let f t =
      match format with
      | `Sexp -> Sexp.to_string_hum (sexp_of_t t)
      | `Exim -> to_string_exim t
      | `Id ->
        List.map t ~f:(Fn.compose Message_id.to_string Spooled_message_info.id)
        |> String.concat_lines
      | `Ascii_table_with_max_width limit_width_to ->
        let display_span span =
          let style =
            match Time.Span.to_min span with
            | x when 0. < x && x < 10. -> [ `Green ]
            | x when 10. <= x && x < 60. -> [ `Yellow ]
            | x when 60. <= x && x < 180. -> [ `Red ]
            | _x -> [ `Blue ]
          in
          style, Time.Span.to_short_string span
        in
        let columns =
          let module S = Spooled_message_info in
          let open Ascii_table in
          let id = Column.create "id" (fun a -> S.id a |> Message_id.to_string) in
          let sender =
            Column.create "sender" (fun a ->
              match S.envelope a with
              | None -> "(hidden)"
              | Some envelope -> Smtp_envelope.string_sender envelope)
          in
          let recipients =
            Column.create "recipients" (fun a ->
              match S.envelope a with
              | None -> "(hidden)"
              | Some envelope ->
                Smtp_envelope.string_recipients envelope |> String.concat ~sep:", ")
          in
          let next_attempt =
            Column.create_attr "next attempt" (fun a ->
              match S.status a with
              | `Send_at at -> display_span (Time.diff at (Time.now ()))
              | `Send_now | `Sending -> [ `Green ], "now"
              | `Frozen -> [ `Red ], "frozen"
              | `Removed -> [ `Red ], "removed"
              | `Quarantined _ -> [ `Yellow ], "quarantine"
              | `Delivered -> [ `Green ], "delivered")
          in
          let next_hop =
            Column.create "next hop" (fun a ->
              S.next_hop_choices a
              |> List.map ~f:Host_and_port.to_string
              |> String.concat ~sep:", ")
          in
          let time_on_spool =
            Column.create_attr "time on spool" (fun a -> display_span (S.time_on_spool a))
          in
          [ id; sender; recipients; time_on_spool; next_attempt; next_hop ]
        in
        Ascii_table.to_string ~limit_width_to columns (sort t)
    in
    f t
  ;;
end

let status t =
  Hashtbl.data t.messages
  |> List.map ~f:(fun message ->
    { Spooled_message_info.message; envelope = None; file_size = None })
;;

let get_message t msg_id = Hashtbl.find t.messages msg_id

let status_from_disk config =
  Message_spool.load (Config.spool_dir config)
  >>=? fun spool ->
  Message_spool.ls spool [ Message_queue.Active; Message_queue.Frozen ]
  >>=? fun entries ->
  let%map msgs =
    Deferred.List.map ~how:`Sequential entries ~f:(fun entry ->
      Message_spool.Entry.size entry
      >>=? fun file_size ->
      Message_spool.Entry.to_message_with_envelope entry
      >>=? fun (message, envelope) ->
      return
        (Ok
           { Spooled_message_info.message
           ; envelope = Some envelope
           ; file_size = Some file_size
           }))
    (* Drop errors because the list of files might have changed since we read the
       directory. *)
  in
  Ok (List.filter_map msgs ~f:Result.ok)
;;

let count_from_disk config =
  Message_spool.load (Config.spool_dir config)
  >>=? fun spool ->
  Message_spool.ls spool [ Message_queue.Active; Message_queue.Frozen ]
  >>=? fun entries -> return (Ok (List.length entries))
;;

let event_stream t =
  Log.debug
    t.log
    (lazy
      (Log.Message.create
         ~here:[%here]
         ~flows:Log.Flows.none
         ~component:[ "spool"; "event-stream" ]
         "subscription"));
  Async_bus.pipe1_exn t.event_stream
;;
