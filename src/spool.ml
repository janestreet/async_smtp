module Stable = struct
  open Core.Core_stable
  open Email_message.Email_message_stable
  open Async_smtp_types.Async_smtp_types_stable

  module Message = Message.Stable
  module Quarantine_reason = Quarantine_reason.Stable

  module Message_id = Message.Id

  module Send_info = struct
    module V1 = struct
      type t =
        [ `All_messages
        | `Frozen_only
        | `Some_messages of Message_id.V1.t list ]
      [@@deriving bin_io]
    end
  end

  module Recover_info = struct
    module V1 = struct
      type t =
        { msgs :
            [ `Removed of Message_id.V1.t list
            | `Quarantined of Message_id.V1.t list ]
        ; wrapper : Email_wrapper.V1.t option
        } [@@deriving bin_io]
    end
  end

  module Spooled_message_info = struct
    module V1 = struct
      type t =
        { message : Message.V2.t
        ; file_size : Byte_units.V1.t option
        ; envelope : Smtp_envelope.V2.t option
        } [@@deriving bin_io]
    end
  end

  module Status = struct
    module V1 = struct
      type t = Spooled_message_info.V1.t list [@@deriving bin_io]
    end
  end

  module Event = struct
    module V1 = struct
      type spool_event =
        [ `Spooled
        | `Delivered
        | `Frozen
        | `Removed
        | `Unfrozen
        | `Recovered of [`From_quarantined | `From_removed]
        | `Quarantined of [`Reason of Quarantine_reason.V1.t]
        ] * Message_id.V1.t * Smtp_envelope.Info.V2.t [@@deriving bin_io, sexp]

      type t = Time.V1.t * [ `Spool_event of spool_event | `Ping ]
      [@@deriving bin_io, sexp]
    end
  end
end

open Core
open Async
open Async_smtp_types

module Config = Server_config


module Message_id    = Message.Id
module Message_queue = Message.Queue

module Log = Mail_log

module Event = struct
  module T = struct
    type spool_event =
      [ `Spooled
      | `Delivered
      | `Frozen
      | `Removed
      | `Unfrozen
      | `Recovered of [`From_quarantined | `From_removed]
      | `Quarantined of [`Reason of Quarantine_reason.t]
      ] * Message_id.t * Smtp_envelope.Info.t [@@deriving sexp_of, compare]
    type t = Time.t * [ `Spool_event of spool_event | `Ping ] [@@deriving sexp_of, compare]
  end

  include T
  include Comparable.Make_plain(T)

  let to_string (_rcvd_time, happening) =
    let without_time =
      match happening with
      | `Ping -> "ping"
      | `Spool_event (spool_event, id, _info) ->
        let event_string = match spool_event with
          | `Spooled             -> "spooled"
          | `Frozen              -> "frozen"
          | `Removed             -> "removed"
          | `Unfrozen            -> "unfrozen"
          | `Delivered           -> "delivered"
          | `Recovered _from     -> "recovered"
          | `Quarantined _reason -> "quarantined"
        in
        (Message_id.to_string id) ^ " " ^ event_string
    in
    without_time
  ;;

  let event_gen event ~(time: [`Now | `Of_msg of Message.t -> Time.t])
        ~here ~log event_stream spooled_msg =
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
    Log.debug log (lazy (Log.Message.create
                           ~here
                           ~flows:(Message.flows spooled_msg)
                           ~component:["spool";"event-stream"]
                           ~spool_id:(Message_id.to_string id)
                           ~tags:["event", sprintf !"%{sexp:t}" t]
                           "writing event"));
    Bus.write event_stream t
  ;;

  let spooled   = event_gen `Spooled   ~time:(`Of_msg Message.spool_date)
  let delivered = event_gen `Delivered ~time:`Now
  let frozen    = event_gen `Frozen    ~time:`Now
  let removed   = event_gen `Removed   ~time:`Now
  let unfrozen  = event_gen `Unfrozen  ~time:`Now

  let quarantined reason      = event_gen (`Quarantined reason)   ~time:`Now
  let recovered   from_where  = event_gen (`Recovered from_where) ~time:`Now
end

type t =
  { spool              : Message_spool.t
  ; active_send_jobs   : int Message_id.Table.t
  ; messages           : Message.t Message_id.Table.t
  ; event_stream       : (Event.t -> unit) Bus.Read_write.t
  ; client_cache       : Client_cache.t
  ; log                : Log.t
  ; mutable is_killed  : bool
  ; killed_and_flushed : unit Ivar.t
  } [@@deriving fields]
;;

let add_message t msg =
  let id = Message.id msg in
  match Hashtbl.add t.messages ~key:id ~data:msg with
  | `Ok -> ()
  | `Duplicate ->
    Log.error t.log (lazy (Log.Message.create
                             ~here:[%here]
                             ~flows:(Message.flows msg)
                             ~component:["spool";"admin"]
                             ~spool_id:(Message_id.to_string id)
                             "Message already in spool"))

let remove_message t msg =
  let id = Message.id msg in
  if Hashtbl.mem t.messages id
  then Hashtbl.remove t.messages id
  else (
    Log.error t.log (lazy (Log.Message.create
                             ~here:[%here]
                             ~flows:(Message.flows msg)
                             ~component:["spool";"admin"]
                             ~spool_id:(Message_id.to_string id)
                             "Trying to remove message that is not in spool")))

let with_event_writer ~here spool spooled_msg ~f = f ~here ~log:spool.log spool.event_stream spooled_msg
let spooled_event   = with_event_writer ~f:Event.spooled
let delivered_event = with_event_writer ~f:Event.delivered
let frozen_event    = with_event_writer ~f:Event.frozen
let removed_event   = with_event_writer ~f:Event.removed
let unfrozen_event  = with_event_writer ~f:Event.unfrozen
let recovered_event t from_where = with_event_writer t ~f:(Event.recovered from_where)
let quarantined_event t reason = with_event_writer t ~f:(Event.quarantined reason)

let create ~config ~log : t Deferred.Or_error.t =
  let spool_dir = Config.spool_dir config in
  Log.info log (lazy (Log.Message.create
                        ~here:[%here]
                        ~flows:Log.Flows.none
                        ~component:["spool"]
                        ~tags:[ "spool-dir", spool_dir ]
                        "initializing"));
  Message_spool.create spool_dir
  >>|? fun spool ->
  let event_stream =
    Bus.create [%here] Arity1 ~on_subscription_after_first_write:Allow
      ~on_callback_raise:Error.raise
  in
  Clock.every (sec 30.)
    (fun () ->
       Log.debug log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows:Log.Flows.none
                              ~component:["spool";"event-stream"]
                              "ping"));
       Bus.write event_stream (Time.now (), `Ping));
  let active_send_jobs = Message_id.Table.create () in
  let messages = Message_id.Table.create () in
  let cache_config =
    Client_cache.Config.create
      ~max_open_connections:500
      ~cleanup_idle_connection_after:(sec 5.)
      ~max_connections_per_address:10
      ~max_connection_reuse:10
  in
  let client_cache =
    Client_cache.init
      ~log
      ~component:["spool"; "client_cache"]
      ~cache_config
      ~client_config:config.Config.client
      ()
  in
  let killed_and_flushed = Ivar.create () in
  Fields.create
    ~spool
    ~active_send_jobs
    ~messages
    ~event_stream
    ~client_cache
    ~log
    ~is_killed:false
    ~killed_and_flushed
;;

let add_active_send_job t msgid =
  Hashtbl.update t.active_send_jobs msgid ~f:(function
    | None -> 1
    | Some x -> x + 1)
;;

let remove_active_send_job ~flows t msgid =
  Hashtbl.change t.active_send_jobs msgid ~f:(function
    | None ->
      Log.error t.log (lazy (Log.Message.create
                               ~here:[%here]
                               ~flows
                               ~component:["spool";"remove_active_send_job"]
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
  else begin
    let msgid = Message.id message in
    let flows = Message.flows message in
    add_active_send_job t msgid;
    let%bind result = f () in
    remove_active_send_job ~flows t msgid;
    return (`Ok result)
  end
;;

let with_spooled_message t message ~f =
  match%bind with_spooled_message' t message ~f with
  | `Spool_is_killed -> return (Error spool_is_killed_error)
  | `Ok result -> return result
;;

let rec enqueue ?don't_retry ?at t spooled_msg =
  let msgid = Message.id spooled_msg in
  begin match at with
  | None      -> Deferred.unit
  | Some time -> Clock.at time
  end
  >>= fun () ->
  Log.debug t.log (lazy (Log.Message.create
                           ~here:[%here]
                           ~flows:(Message.flows spooled_msg)
                           ~component:["spool";"throttle"]
                           ~spool_id:(Message_id.to_string msgid)
                           "enqueuing message"));
  let status = Message.status spooled_msg in
  match status with
  | `Delivered  | `Sending | `Frozen | `Removed | `Quarantined _ ->
    (* Only actually call [send] if the status is [`Send_now]. Between the time an
       async job was scheduled and the time it gets run, the status of the message
       can change. We make sure not to try to send if we have manually intervened,
       causing a permanent status change. *)
    Or_error.errorf !"Not attempting delivery for status %{sexp: Message.Status.t}"
      status
    |> return
  | `Send_now  | `Send_at _ ->
    with_spooled_message' t spooled_msg ~f:(fun () ->
      Message_spool.send spooled_msg ~log:t.log ~client_cache:t.client_cache)
    >>= function
    | `Spool_is_killed ->
      return (Error spool_is_killed_error)
    | `Ok spool_send_result ->
      match spool_send_result with
      | Error e ->
        Log.error t.log
          (lazy (Log.Message.of_error
                   ~here:[%here]
                   ~flows:(Message.flows spooled_msg)
                   ~component:["spool";"send"]
                   ~spool_id:(Message_id.to_string msgid)
                   e));
        return (Error e)
      | Ok `Delivered ->
        remove_message t spooled_msg;
        delivered_event t ~here:[%here] spooled_msg;
        return (Ok ())
      | Ok (`Failed relay_attempt) ->
        match don't_retry with
        | Some () ->
          return (Error relay_attempt)
        | None ->
          (* Use the Async queue to store the messages waiting for retry. *)
          match Message.status spooled_msg with
          | `Send_now ->
            enqueue t spooled_msg
          | `Send_at at ->
            enqueue ~at t spooled_msg
          | `Frozen ->
            frozen_event t ~here:[%here] spooled_msg;
            return (Error relay_attempt)
          | `Delivered | `Sending | `Removed | `Quarantined _  ->
            Log.error t.log
              (lazy (Log.Message.create
                       ~here:[%here]
                       ~flows:(Message.flows spooled_msg)
                       ~component:["spool";"send"]
                       ~spool_id:(Message_id.to_string msgid)
                       ~tags:["status", sprintf !"%{sexp:Message.Status.t}" status]
                       "Unexpected status after [Message_spool.send]"));
            return (Error relay_attempt)
;;

let schedule t msg =
  match Message.status msg with
  | `Frozen | `Removed | `Quarantined _ -> ()
  | (`Sending | `Delivered) as status ->
    failwithf !"Unexpected status %{sexp:Message.Status.t} when trying \
                to schedule message %{Message_id}"
      status (Message.id msg) ()
  | `Send_at at ->
    Log.info t.log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows:(Message.flows msg)
                            ~component:["spool";"queue"]
                            ~spool_id:(Message_id.to_string (Message.id msg))
                            ~tags:["time", sprintf !"%{sexp:Time.t}" at]
                            "send later"));
    don't_wait_for (enqueue ~at t msg >>| (ignore : unit Or_error.t -> unit))
  | `Send_now ->
    Log.info t.log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows:(Message.flows msg)
                            ~component:["spool";"queue"]
                            ~spool_id:(Message_id.to_string (Message.id msg))
                            "send now"));
    don't_wait_for (enqueue t msg >>| (ignore : unit Or_error.t -> unit))

let load t =
  Message_spool.ls t.spool [Message_queue.Active; Message_queue.Frozen]
  >>=? fun entries ->
  Log.debug t.log (lazy (Log.Message.create
                           ~here:[%here]
                           ~flows:Log.Flows.none
                           ~component:["spool";"init"]
                           ~tags:["entries", sprintf !"%{sexp:Message_spool.Entry.t list}" entries]
                           "found files"));
  Deferred.List.iter entries ~how:`Parallel ~f:(fun entry ->
    Message_spool.Entry.to_message entry
    >>| function
    | Error e ->
      Log.error t.log
        (lazy (Log.Message.of_error
                 ~here:[%here]
                 ~flows:Log.Flows.none
                 ~component:["spool";"init"]
                 ~tags:["entries", sprintf !"%{sexp:Message_spool.Entry.t list}" [entry]]
                 e))
    | Ok msg ->
      Log.info t.log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows:Log.Flows.none
                              ~component:["spool";"init"]
                              ~tags:["entries", sprintf !"%{sexp:Message_spool.Entry.t list}" [entry]]
                              ~spool_id:(Message_id.to_string (Message.id msg))
                              "loaded"));
      add_message t msg;
      schedule t msg)
  >>= fun () ->
  return (Ok ())
;;

let create ~config ~log () =
  create ~config ~log
  >>=? fun t ->
  load t
  >>=? fun () ->
  return (Ok t)
;;

let add t ?(initial_status = `Send_now) ~flows ~original_msg envelope_batches =
  Deferred.Or_error.List.iter envelope_batches ~how:`Parallel ~f:(fun envelope_batch ->
    Message_spool.enqueue
      t.spool
      ~log:t.log
      ~flows:(Log.Flows.extend flows `Outbound_envelope)
      ~initial_status:(initial_status :> Message.Status.t)
      envelope_batch
      ~original_msg
    >>|? fun spooled_msgs ->
    List.iter spooled_msgs ~f:(fun msg ->
      add_message t msg;
      don't_wait_for (enqueue t msg >>| (ignore : unit Or_error.t -> unit));
      spooled_event t ~here:[%here] msg))
  >>|? fun () ->
  Smtp_envelope.id original_msg
;;

let quarantine t ~reason ~flows ~original_msg envelope_batches =
  Deferred.Or_error.List.iter envelope_batches ~how:`Parallel ~f:(fun envelope_batch ->
    Message_spool.enqueue
      t.spool
      ~log:t.log
      ~flows:(Log.Flows.extend flows `Outbound_envelope)
      ~initial_status:(`Quarantined reason)
      envelope_batch
      ~original_msg
    >>|? fun quarantined_msgs ->
    List.iter quarantined_msgs ~f:(fun msg ->
      quarantined_event ~here:[%here] t (`Reason reason) msg))
;;

let kill_and_flush t =
  t.is_killed <- true;
  if Hashtbl.length t.active_send_jobs = 0
  then Ivar.fill_if_empty t.killed_and_flushed ();
  Deferred.all_unit
    [ Client_cache.close_and_flush t.client_cache
    ; Ivar.read t.killed_and_flushed
    ]
;;

let get_msg t id ~f =
  match Hashtbl.find t.messages id with
  | None ->
    return (Or_error.error_string "Not found")
  | Some spooled_msg ->
    f spooled_msg

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
    | `Some_messages of Message_id.t list ]
end

let send_msgs ?(retry_intervals = []) t ids =
  let do_send ?event msg =
    with_spooled_message t msg ~f:(fun () ->
      Message_spool.mark_for_send_now ~retry_intervals msg ~log:t.log)
    >>=? fun () ->
    Option.iter event ~f:(fun event -> event t msg);
    enqueue ~don't_retry:() t msg
  in
  Deferred.List.map ids ~how:`Parallel ~f:(fun id ->
    get_msg t id ~f:(fun msg ->
      match Message.status msg with
      | `Frozen ->
        do_send ~event:(unfrozen_event ~here:[%here]) msg
      | `Send_at _ ->
        (* This will enqueue the message without changing what was previously
           queued, so we will attempt to deliver twice. It's ok,
           [Message.send] can deal with this. *)
        do_send msg
      | `Sending | `Delivered | `Send_now | `Removed | `Quarantined _ -> return (Ok ())
    )
    >>| Or_error.tag ~tag:(Message_id.to_string id))
  >>| Or_error.combine_errors_unit
;;

let remove t ids =
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    get_msg t id ~f:(fun spooled_msg ->
      begin
        match Message.status spooled_msg with
        | `Frozen ->
          with_spooled_message t spooled_msg ~f:(fun () ->
            Message_spool.remove spooled_msg ~log:t.log)
        | `Send_at _ | `Send_now | `Sending | `Delivered | `Removed | `Quarantined _ ->
          Error (Error.of_string "Cannot remove a message that is not currently frozen")
          |> return
      end
      >>=? fun () ->
      remove_message t spooled_msg;
      removed_event t ~here:[%here] spooled_msg;
      Deferred.Or_error.ok_unit))
;;

module Recover_info = struct
  type t = Stable.Recover_info.V1.t =
    { msgs :
        [ `Removed of Message_id.t list
        | `Quarantined of Message_id.t list ]
    ; wrapper : Email_wrapper.t option
    }
end

let recover t (info : Recover_info.t) =
  let msgids, from_queue, from_queue' =
    match info.msgs with
    | `Quarantined msgids -> msgids, Message_queue.Quarantine, `From_quarantined
    | `Removed msgids     -> msgids, Message_queue.Removed, `From_removed
  in
  Deferred.Or_error.List.iter msgids ~how:`Parallel ~f:(fun id ->
    let name = Message_id.to_string id in
    let entry = Message_spool.Entry.create t.spool from_queue ~name in
    Message_spool.Entry.to_message entry
    >>= function
    | Error e ->
      let e = Error.tag e ~tag:"Failed to recover message" in
      Log.info t.log
        (lazy (Log.Message.of_error
                 ~here:[%here]
                 ~flows:Log.Flows.none
                 ~component:["spool";"admin"]
                 ~spool_id:(Message_id.to_string id)
                 e));
      return (Error e)
    | Ok msg ->
      Option.value_map info.wrapper ~default:Deferred.Or_error.ok_unit ~f:(fun wrapper ->
        with_spooled_message t msg ~f:(fun () ->
          Message_spool.map_email msg ~f:(fun email ->
            Email_wrapper.add wrapper email)))
      >>=? fun () ->
      with_spooled_message t msg ~f:(fun () ->
        Message_spool.freeze msg ~log:t.log)
      >>=? fun () ->
      add_message t msg;
      recovered_event t ~here:[%here] from_queue' msg;
      Deferred.Or_error.ok_unit)
;;

let send_all ?retry_intervals ?(frozen_only = false) t =
  Hashtbl.data t.messages
  |> List.filter_map ~f:(fun m ->
    match Message.status m, frozen_only with
    | `Frozen, _        -> Some (Message.id m)
    | `Send_at _, false -> Some (Message.id m)
    | `Send_now, _  | `Send_at _, true | `Sending, _
    | `Delivered, _ | `Removed, _      | `Quarantined _, _-> None)
  |> send_msgs ?retry_intervals t
;;

let send ?retry_intervals t send_info =
  match send_info with
  | `All_messages -> send_all ?retry_intervals t
  | `Frozen_only ->  send_all ?retry_intervals ~frozen_only:true t
  | `Some_messages msgids -> send_msgs ?retry_intervals t msgids

module Spooled_message_info = struct
  type t = Stable.Spooled_message_info.V1.t =
    { message   : Message.t
    ; file_size : Byte_units.t option
    ; envelope  : Smtp_envelope.t option
    } [@@deriving fields, sexp_of]

  let sp f t = f t.message

  let id                 = sp Message.id
  let spool_date         = sp Message.spool_date
  let last_relay_attempt = sp Message.last_relay_attempt
  let parent_id          = sp Message.parent_id
  let envelope_info      = sp Message.envelope_info
  let status             = sp Message.status

  (* Internal *)
  let time_on_spool      = sp Message.time_on_spool
  let next_hop_choices   = sp Message.next_hop_choices
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

       25m  2.9K 0t5C6f-0000c8-00 <alice@wonderland.fict.example>
       red.king@looking-glass.fict.example
       <other addresses>

       The first line contains the length of time the message has been on the queue
       (in this case 25 minutes), the size of the message (2.9K), the unique local
       identifier for the message, and the message sender, as contained in the
       envelope. For bounce messages, the sender address is empty, and appears as
       "<>". If the message was submitted locally by an untrusted user who overrode
       the default sender address, the user’s login name is shown in parentheses
       before the sender address.

       If the message is frozen (attempts to deliver it are suspended) then the text
       "*** frozen ***" is displayed at the end of this line.

       The recipients of the message (taken from the envelope, not the headers) are
       displayed on subsequent lines. Those addresses to which the message has already
       been delivered are marked with the letter D. If an original address gets
       expanded into several addresses via an alias or forward file, the original is
       displayed with a D only when deliveries for all of its child addresses are
       complete.  *)
    let module S = Spooled_message_info in
    let msg_to_string msg =
      let frozen_text =
        match S.status msg with
        | `Send_at _ | `Send_now | `Sending | `Delivered | `Removed | `Quarantined _ -> ""
        | `Frozen                                        -> "*** frozen ***"
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
        let recipients = Smtp_envelope.string_recipients envelope |> String.concat ~sep:"\n" in
        sprintf !"%4s %5s %{Message_id} <%s> %s\n           %s\n"
          time size (S.id msg) (Smtp_envelope.string_sender envelope)
          frozen_text recipients
      | None ->
        sprintf !"%4s %5s %{Message_id} %s (sender and recipients hidden)\n"
          time size (S.id msg) frozen_text
    in
    List.map (sort t) ~f:msg_to_string
    |> String.concat ~sep:"\n"
  ;;

  let to_formatted_string t ~format =
    let open Textutils in
    let format : [ `Sexp | `Exim | `Ascii_table_with_max_width of int ] =
      match format with
      | `Ascii_table -> `Ascii_table_with_max_width 180
      | (`Sexp | `Exim | `Ascii_table_with_max_width _) as format ->
        format
    in
    let f t =
      match format with
      | `Sexp -> Sexp.to_string_hum (sexp_of_t t)
      | `Exim -> to_string_exim t
      | `Ascii_table_with_max_width limit_width_to ->
        let display_span span =
          let style =
            match Time.Span.to_min span with
            | x when 0. < x && x < 10.    -> [ `Green ]
            | x when 10. <= x && x < 60.  -> [ `Yellow ]
            | x when 60. <= x && x < 180. -> [ `Red ]
            | _x                          -> [ `Blue ]
          in
          style, Time.Span.to_short_string span
        in
        let columns =
          let module S = Spooled_message_info in
          let open Ascii_table in
          let id =
            Column.create "id"
              (fun a -> S.id a |> Message_id.to_string)
          in
          let sender = Column.create "sender" (fun a ->
            match S.envelope a with
            | None -> "(hidden)"
            | Some envelope ->
              Smtp_envelope.string_sender envelope)
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
              | `Send_at at            -> display_span (Time.diff at (Time.now ()))
              | (`Send_now | `Sending) -> [`Green]  , "now"
              | `Frozen                -> [`Red]    , "frozen"
              | `Removed               -> [`Red]    , "removed"
              | `Quarantined _         -> [`Yellow] , "quarantine"
              | `Delivered             -> [`Green]  , "delivered"
            )
          in
          let next_hop =
            Column.create "next hop" (fun a ->
              S.next_hop_choices a
              |> List.map ~f:Smtp_socket_address.to_string
              |> String.concat ~sep:", ")
          in
          let time_on_spool =
            Column.create_attr "time on spool" (fun a ->
              display_span (S.time_on_spool a))
          in
          [ id; sender; recipients; time_on_spool; next_attempt; next_hop ]
        in
        Ascii_table.to_string ~limit_width_to columns (sort t)
    in
    f t
end

let status t =
  Hashtbl.data t.messages
  |> List.map ~f:(fun message ->
    { Spooled_message_info. message
    ; envelope = None; file_size = None })

let status_from_disk config =
  Message_spool.load (Config.spool_dir config)
  >>=? fun spool ->
  Message_spool.ls spool [Message_queue.Active; Message_queue.Frozen]
  >>=? fun entries ->
  Deferred.List.map entries ~f:(fun entry ->
    Message_spool.Entry.to_message_with_envelope entry
    >>=? fun (message, envelope) ->
    Message_spool.size_of_file message
    >>=? fun file_size ->
    return (Ok { Spooled_message_info. message
               ; envelope = Some envelope
               ; file_size = Some file_size }))
  (* Drop errors because the list of files might have changed since we read
     the directory. *)
  >>| fun msgs ->
  Ok (List.filter_map msgs ~f:Result.ok)
;;

let count_from_disk config =
  Message_spool.load (Config.spool_dir config)
  >>=? fun spool ->
  Message_spool.ls spool [Message_queue.Active; Message_queue.Frozen]
  >>=? fun entries ->
  return (Ok (List.length entries))
;;

let event_stream t =
  Log.debug t.log (lazy (Log.Message.create
                           ~here:[%here]
                           ~flows:Log.Flows.none
                           ~component:["spool";"event-stream"]
                           "subscription"));
  Bus.pipe1_exn (Bus.read_only t.event_stream) [%here]
;;
