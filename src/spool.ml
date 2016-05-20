open Core.Std
open Async.Std

module Config = Server_config

open Types

module Spooled_message = Spooled_message_internal
module Spooled_message_id = Spooled_message.Id

module Log = Mail_log

module Event = struct
  module T = struct
    type t = Time.t *
             [ `Spooled     of Spooled_message.Id.t
             | `Delivered   of Spooled_message.Id.t
             | `Frozen      of Spooled_message.Id.t
             | `Removed     of Spooled_message.Id.t
             | `Unfrozen    of Spooled_message.Id.t
             | `Recovered   of Spooled_message_id.t * [`From_quarantined | `From_removed]
             | `Quarantined of Spooled_message.Id.t * [`Reason of string]
             | `Ping ]
    [@@deriving sexp, bin_io, compare]

    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let to_string (_rcvd_time, happening) =
    let without_time =
      let id_to_s = Spooled_message.Id.to_string in
      match happening with
      | `Spooled    msgid             -> (id_to_s msgid) ^ " spooled"
      | `Frozen     msgid             -> (id_to_s msgid) ^ " frozen"
      | `Removed    msgid             -> (id_to_s msgid) ^ " removed"
      | `Unfrozen   msgid             -> (id_to_s msgid) ^ " unfrozen"
      | `Delivered  msgid             -> (id_to_s msgid) ^ " delivered"
      | `Recovered (msgid, _from)     -> (id_to_s msgid) ^ " recovered"
      | `Quarantined (msgid, _reason) -> (id_to_s msgid) ^ " quarantined"
      | `Ping                         -> "ping"
    in
    without_time
  ;;

  let event_gen ~here ~log ~f event_stream spooled_msg =
    let id = Spooled_message.id spooled_msg in
    let t = f ~id spooled_msg in
    Log.debug log (lazy (Log.Message.create
                           ~here
                           ~flows:(Spooled_message.flows spooled_msg)
                           ~component:["spool";"event-stream"]
                           ~spool_id:(Spooled_message_id.to_string id)
                           ~tags:["event", sprintf !"%{sexp:t}" t]
                           "writing event"));
    Bus.write event_stream t
  ;;

  let spooled   =
    event_gen ~f:(fun ~id  msg -> (Spooled_message.spool_date msg, `Spooled id))
  ;;

  let quarantine reason =
    event_gen ~f:(fun ~id _msg -> Time.now (), `Quarantined (id, reason))
  ;;

  let recover from_where =
    event_gen ~f:(fun ~id _msg -> Time.now (), `Recovered (id, from_where))
  ;;

  let delivered = event_gen ~f:(fun ~id _msg -> Time.now (), `Delivered id)
  let frozen    = event_gen ~f:(fun ~id _msg -> Time.now (), `Frozen id)
  let remove    = event_gen ~f:(fun ~id _msg -> Time.now (), `Removed id)
  let unfrozen  = event_gen ~f:(fun ~id _msg -> Time.now (), `Unfrozen id)
end

type t =
  { spool_dir     : Spool_directory.t
  ; send_throttle : Adjustable_throttle.t
  (* Don't hit the max open files system limit. *)
  ; file_throttle : unit Throttle.t
  ; messages      : Spooled_message.t Spooled_message.Id.Table.t
  ; event_stream  : (Event.t -> unit) Bus.Read_write.t
  ; client        : Client_config.t
  ; log           : Log.t
  } [@@deriving fields]
;;

let add_message t msg =
  let id = Spooled_message.id msg in
  match Hashtbl.add t.messages ~key:id ~data:msg with
  | `Ok -> ()
  | `Duplicate ->
    Log.error t.log (lazy (Log.Message.create
                             ~here:[%here]
                             ~flows:(Spooled_message.flows msg)
                             ~component:["spool";"admin"]
                             ~spool_id:(Spooled_message_id.to_string id)
                             "Message already in spool"))

let remove_message t msg =
  let id = Spooled_message.id msg in
  if not (Hashtbl.mem t.messages id) then
    Log.error t.log (lazy (Log.Message.create
                             ~here:[%here]
                             ~flows:(Spooled_message.flows msg)
                             ~component:["spool";"admin"]
                             ~spool_id:(Spooled_message_id.to_string id)
                             "Trying to remove message that is not in spool"));
  Hashtbl.remove t.messages id

let with_event_writer ~here spool spooled_msg ~f = f ~here ~log:spool.log spool.event_stream spooled_msg
let spooled_event   = with_event_writer ~f:Event.spooled
let delivered_event = with_event_writer ~f:Event.delivered
let frozen_event    = with_event_writer ~f:Event.frozen
let remove_event    = with_event_writer ~f:Event.remove
let unfrozen_event  = with_event_writer ~f:Event.unfrozen
let recover_event t from_where = with_event_writer t ~f:(Event.recover from_where)
let quarantine_event t reason = with_event_writer t ~f:(Event.quarantine reason)

let create ~config ~log : t Deferred.Or_error.t =
  let spool_dir = Config.spool_dir config in
  let max_concurrent_jobs = Config.max_concurrent_send_jobs config in
  Log.info log (lazy (Log.Message.create
                        ~here:[%here]
                        ~flows:Log.Flows.none
                        ~component:["spool"]
                        ~tags:[ "spool-dir", spool_dir
                              ; "max-concurrent-jobs", Int.to_string max_concurrent_jobs]
                        "initializing"));
  Spool_directory.init ~path:spool_dir
  >>|? fun spool_dir ->
  let event_stream =
    Bus.create [%here] Arity1 ~allow_subscription_after_first_write:true
      ~on_callback_raise:Error.raise
  in
  let send_throttle = Adjustable_throttle.create ~max_concurrent_jobs in
  let file_throttle =
    Throttle.create ~continue_on_error:true ~max_concurrent_jobs:200
  in
  Clock.every (sec 30.)
    (fun () ->
       Log.debug log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows:Log.Flows.none
                              ~component:["spool";"event-stream"]
                              "ping"));
      Bus.write event_stream (Time.now (), `Ping));
  let messages = Spooled_message.Id.Table.create () in
  Fields.create
    ~spool_dir
    ~send_throttle
    ~file_throttle
    ~messages
    ~event_stream
    ~client:config.Config.client
    ~log
;;

(* Using the Async queue to store the messages waiting for retry. *)
let rec enqueue ?at t spooled_msg =
  let msgid = Spooled_message.id spooled_msg in
  don't_wait_for (
    begin match at with
    | None      -> Deferred.unit
    | Some time -> Clock.at time
    end
    >>| fun () ->
    if Adjustable_throttle.is_dead t.send_throttle then ()
    else begin
      Log.debug t.log (lazy (Log.Message.create
                               ~here:[%here]
                               ~flows:(Spooled_message.flows spooled_msg)
                               ~component:["spool";"throttle"]
                               ~spool_id:(Spooled_message_id.to_string msgid)
                               "enqueuing message"));
      Adjustable_throttle.enqueue t.send_throttle (fun () ->
        match Spooled_message.status spooled_msg with
        | `Delivered ->
          (* Do nothing here because calling [Spool.send] enqueues a message without
             changing what was previously queued, so this code can be run multiple times
             for the same message *)
          return ()
        | _ ->
          Spooled_message.send spooled_msg ~log:t.log ~config:t.client
          >>| function
          | Error e ->
            Log.error t.log (lazy (Log.Message.of_error
                                     ~here:[%here]
                                     ~flows:(Spooled_message.flows spooled_msg)
                                     ~component:["spool";"send"]
                                     ~spool_id:(Spooled_message_id.to_string msgid)
                                     e))
          | Ok () ->
            match Spooled_message.status spooled_msg with
            | `Delivered ->
              remove_message t spooled_msg;
              delivered_event t ~here:[%here] spooled_msg
            | `Send_now ->
              enqueue t spooled_msg
            | `Send_at at ->
              enqueue ~at t spooled_msg
            | `Sending ->
              failwithf !"Message has status Sending after returning from \
                          Spooled_message.send: %{Spooled_message.Id}"
                (Spooled_message.id spooled_msg) ()
            | `Frozen | `Removed | `Quarantined _ -> ())
    end)
;;

let schedule t msg =
  match Spooled_message.status msg with
  | `Frozen | `Removed | `Quarantined _ -> ()
  | (`Sending | `Delivered) as status ->
    failwithf !"Unexpected status %{sexp:Spooled_message.Status.t} when trying \
                to schedule message %{Spooled_message.Id}"
      status (Spooled_message.id msg) ()
  | `Send_at at ->
    Log.info t.log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows:(Spooled_message.flows msg)
                            ~component:["spool";"queue"]
                            ~spool_id:(Spooled_message_id.to_string (Spooled_message.id msg))
                            ~tags:["time", sprintf !"%{sexp:Time.t}" at]
                            "send later"));
    enqueue ~at t msg
  | `Send_now ->
    Log.info t.log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows:(Spooled_message.flows msg)
                            ~component:["spool";"queue"]
                            ~spool_id:(Spooled_message_id.to_string (Spooled_message.id msg))
                            "send now"));
    enqueue t msg

let load t =
  Spool_directory.ls t.spool_dir
  >>=? fun entries ->
  Log.debug t.log (lazy (Log.Message.create
                           ~here:[%here]
                           ~flows:Log.Flows.none
                           ~component:["spool";"init"]
                           ~tags:["files", sprintf !"%{sexp:string list}" entries]
                           "found files"));
  Deferred.List.iter entries ~how:`Parallel ~f:(fun entry ->
      Throttle.enqueue t.file_throttle (fun () -> Spooled_message.load entry)
      >>| function
      | Error e ->
        Log.error t.log (lazy (Log.Message.of_error
                                 ~here:[%here]
                                 ~flows:Log.Flows.none
                                 ~component:["spool";"init"]
                                 ~tags:["files", sprintf !"%{sexp: string list}" [entry]]
                                 e))
      | Ok msg ->
        Log.info t.log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows:Log.Flows.none
                                ~component:["spool";"init"]
                                ~tags:["files", sprintf !"%{sexp: string list}" [entry]]
                                ~spool_id:(Spooled_message_id.to_string (Spooled_message.id msg))
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

let add t ~flows ~original_msg messages =
  Deferred.Or_error.List.iter messages ~how:`Parallel ~f:(fun envelope_with_next_hop ->
      Throttle.enqueue t.file_throttle (fun () ->
          Spooled_message.create
            t.spool_dir
            ~log:t.log
            ~flows:(Log.Flows.extend flows `Outbound_envelope)
            ~initial_status:`Send_now
            envelope_with_next_hop
            ~original_msg)
      >>|? fun spooled_msg ->
      add_message t spooled_msg;
      enqueue t spooled_msg;
      spooled_event t ~here:[%here] spooled_msg)
  >>|? fun () ->
  Envelope.id original_msg
;;

let quarantine t ~reason ~flows ~original_msg messages =
  Deferred.Or_error.List.iter messages
    ~how:`Parallel
    ~f:(fun envelope_with_next_hop ->
      Throttle.enqueue t.file_throttle
        (fun () ->
           Spooled_message.create
             t.spool_dir
             ~log:t.log
             ~flows:(Log.Flows.extend flows `Outbound_envelope)
             ~initial_status:(`Quarantined reason)
             envelope_with_next_hop
             ~original_msg)
      >>|? fun quarantined_msg ->
      quarantine_event ~here:[%here] t (`Reason reason) quarantined_msg)
;;

let kill_and_flush ?(timeout = Deferred.never ()) t =
  Deferred.choose
    [ Deferred.choice timeout
        (fun () -> `Timeout)
    ; Deferred.choice (Adjustable_throttle.kill_and_flush t.send_throttle)
        (fun () -> `Finished) ]
;;

let set_max_concurrent_jobs t =
  Adjustable_throttle.set_max_concurrent_jobs t.send_throttle

let with_outstanding_msg t id ~f =
  match Hashtbl.find t.messages id with
  | None ->
    return (Or_error.error_string "Not found")
  | Some spooled_msg ->
    f spooled_msg

let freeze t ids =
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    with_outstanding_msg t id ~f:(fun spooled_msg ->
      Throttle.enqueue t.file_throttle (fun () ->
        Spooled_message.freeze spooled_msg ~log:t.log)
      >>=? fun () ->
      frozen_event t ~here:[%here] spooled_msg;
      Deferred.Or_error.ok_unit))
;;

module Send_info = struct
  type t =
    [ `All_messages
    | `Frozen_only
    | `Some_messages of Spooled_message_id.t list ] [@@deriving bin_io]
end

let send_msgs ?(retry_intervals = []) t ids =
  let do_send ?event msg =
    Throttle.enqueue t.file_throttle (fun () ->
      Spooled_message.mark_for_send_now ~retry_intervals msg ~log:t.log
    )
    >>=? fun () ->
    Option.iter event ~f:(fun event -> event t msg);
    enqueue t msg;
    Deferred.Or_error.ok_unit
  in
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    with_outstanding_msg t id ~f:(fun msg ->
      match Spooled_message.status msg with
      | `Frozen ->
        do_send ~event:(unfrozen_event ~here:[%here]) msg
      | `Send_at _ ->
        (* This will enqueue the message without changing what was previously
           queued, so we will attempt to deliver twice. It's ok,
           [Spooled_message.send] can deal with this. *)
        do_send msg
      | `Sending | `Delivered | `Send_now | `Removed | `Quarantined _ -> return (Ok ())
    )
  )
;;

let remove t ids =
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    with_outstanding_msg t id ~f:(fun spooled_msg ->
      begin
        match Spooled_message.status spooled_msg with
        | `Frozen ->
          Throttle.enqueue t.file_throttle
            (fun () -> Spooled_message.remove spooled_msg ~log:t.log)
        | `Send_at _ | `Send_now | `Sending | `Delivered | `Removed | `Quarantined _ ->
          Error (Error.of_string "Cannot remove a message that is not currently frozen")
          |> return
      end
      >>=? fun () ->
      remove_message t spooled_msg;
      remove_event t ~here:[%here] spooled_msg;
      Deferred.Or_error.ok_unit))
;;

module Recover_info = struct
  type t =
    [ `Removed of Spooled_message_id.t list
    | `Quarantined of Spooled_message_id.t list ] [@@deriving bin_io]
end

let recover t info =
  let msgids, from_where, dir =
    match info with
    | `Quarantined msgids ->
      msgids, `From_quarantined, Spool_directory.quarantine_dir t.spool_dir
    | `Removed msgids ->
      msgids, `From_removed, Spool_directory.removed_dir t.spool_dir
  in
  Deferred.Or_error.List.iter msgids ~how:`Parallel ~f:(fun id ->
    let path = dir ^/ Spooled_message.Id.to_string id in
    Throttle.enqueue t.file_throttle (fun () -> Spooled_message.load path)
    >>= function
    | Error e ->
      let e = Error.tag e ~tag:"Failed to recover message" in
      Log.error t.log (lazy (Log.Message.of_error
                               ~here:[%here]
                               ~flows:Log.Flows.none
                               ~component:["spool";"admin"]
                               ~spool_id:(Spooled_message_id.to_string id)
                               e));
      return (Error e)
    | Ok msg ->
      Throttle.enqueue t.file_throttle (fun () -> Spooled_message.freeze msg ~log:t.log)
      >>=? fun () ->
      add_message t msg;
      recover_event t ~here:[%here] from_where msg;
      Deferred.Or_error.ok_unit)
;;

let send_all ?retry_intervals ?(frozen_only = false) t =
  Hashtbl.data t.messages
  |> List.filter_map ~f:(fun m ->
    match Spooled_message.status m, frozen_only with
    | `Frozen, _        -> Some (Spooled_message.id m)
    | `Send_at _, false -> Some (Spooled_message.id m)
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
  type t =
    { spooled_message : Spooled_message.t
    ; file_size : Byte_units.t option
    ; envelope : Envelope.t option
    } [@@deriving fields, sexp, bin_io]

  let sp f t = f t.spooled_message

  let id                 = sp Spooled_message.id
  let spool_date         = sp Spooled_message.spool_date
  let last_relay_attempt = sp Spooled_message.last_relay_attempt
  let parent_id          = sp Spooled_message.parent_id
  let status             = sp Spooled_message.status

  (* Internal *)
  let time_on_spool      = sp Spooled_message.time_on_spool
  let next_hop_choices   = sp Spooled_message.next_hop_choices
end

module Status = struct
  type t = Spooled_message_info.t list [@@deriving sexp, bin_io]

  let sort t =
    let cmp a b =
      let f = Spooled_message_info.time_on_spool in
      Time.Span.compare (f a) (f b)
    in
    List.sort ~cmp t
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
        let recipients = Envelope.string_recipients envelope |> String.concat ~sep:"\n" in
        sprintf !"%4s %5s %{Spooled_message.Id} <%s> %s\n           %s\n"
          time size (S.id msg) (Envelope.string_sender envelope)
          frozen_text recipients
      | None ->
        sprintf !"%4s %5s %{Spooled_message.Id} %s (sender and recipients hidden)\n"
          time size (S.id msg) frozen_text
    in
    List.map (sort t) ~f:msg_to_string
    |> String.concat ~sep:"\n"
  ;;

  let to_formatted_string t ~format =
    let open Textutils.Std in
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
              (fun a -> S.id a |> Spooled_message.Id.to_string)
          in
          let sender = Column.create "sender" (fun a ->
            match S.envelope a with
            | None -> "(hidden)"
            | Some envelope ->
              Envelope.string_sender envelope)
          in
          let recipients =
            Column.create "recipients" (fun a ->
              match S.envelope a with
              | None -> "(hidden)"
              | Some envelope ->
                Envelope.string_recipients envelope |> String.concat ~sep:", ")
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
              |> List.map ~f:Address.to_string
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
  |> List.map ~f:(fun spooled_message ->
    { Spooled_message_info. spooled_message
    ; envelope = None; file_size = None })

let status_from_disk config =
  Spool_directory.ls (Config.spool_dir config)
  >>=? fun entries ->
  Deferred.List.map entries ~f:(fun entry ->
    Spooled_message.load_with_envelope entry
    >>=? fun (spooled_message, envelope) ->
    Spooled_message.size_of_file spooled_message
    >>=? fun file_size ->
    return (Ok { Spooled_message_info. spooled_message
               ; envelope = Some envelope
               ; file_size = Some file_size }))
  (* Drop errors because the list of files might have changed since we read
     the directory. *)
  >>| fun msgs ->
  Ok (List.filter_map msgs ~f:Result.ok)

;;

let count_from_disk config =
  Spool_directory.ls (Config.spool_dir config)
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
