open Core
open Async

module Config = Server_config


module Message_id = Message.Id
module Message_spool = Message.On_disk_spool
module Message_queue = Message_spool.Queue

module Log = Mail_log

module Event = struct
  module T = struct
    type t = Time.t *
             [ `Spooled     of Message_id.t
             | `Delivered   of Message_id.t
             | `Frozen      of Message_id.t
             | `Removed     of Message_id.t
             | `Unfrozen    of Message_id.t
             | `Recovered   of Message_id.t * [`From_quarantined | `From_removed]
             | `Quarantined of Message_id.t * [`Reason of string]
             | `Ping ]
    [@@deriving sexp, bin_io, compare]

    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let to_string (_rcvd_time, happening) =
    let without_time =
      let id_to_s = Message_id.to_string in
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
    let id = Message.id spooled_msg in
    let t = f ~id spooled_msg in
    Log.debug log (lazy (Log.Message.create
                           ~here
                           ~flows:(Message.flows spooled_msg)
                           ~component:["spool";"event-stream"]
                           ~spool_id:(Message_id.to_string id)
                           ~tags:["event", sprintf !"%{sexp:t}" t]
                           "writing event"));
    Bus.write event_stream t
  ;;

  let spooled   =
    event_gen ~f:(fun ~id  msg -> (Message.spool_date msg, `Spooled id))
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
  { spool             : Message_spool.t
  ; mutable is_killed : bool
  ; messages          : Message.t Message_id.Table.t
  ; event_stream      : (Event.t -> unit) Bus.Read_write.t
  ; client_cache      : Client_cache.t
  ; log               : Log.t
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
  if not (Hashtbl.mem t.messages id) then
    Log.error t.log (lazy (Log.Message.create
                             ~here:[%here]
                             ~flows:(Message.flows msg)
                             ~component:["spool";"admin"]
                             ~spool_id:(Message_id.to_string id)
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
  Log.info log (lazy (Log.Message.create
                        ~here:[%here]
                        ~flows:Log.Flows.none
                        ~component:["spool"]
                        ~tags:[ "spool-dir", spool_dir ]
                        "initializing"));
  Message_spool.create spool_dir
  >>|? fun spool ->
  let event_stream =
    Bus.create [%here] Arity1 ~allow_subscription_after_first_write:true
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
  Fields.create
    ~spool
    ~is_killed:false
    ~messages
    ~event_stream
    ~client_cache
    ~log
;;

(* Using the Async queue to store the messages waiting for retry. *)
let rec enqueue ?at t spooled_msg =
  let msgid = Message.id spooled_msg in
  don't_wait_for (
    begin match at with
    | None      -> Deferred.unit
    | Some time -> Clock.at time
    end
    >>= fun () ->
    if t.is_killed then return ()
    else begin
      Log.debug t.log (lazy (Log.Message.create
                               ~here:[%here]
                               ~flows:(Message.flows spooled_msg)
                               ~component:["spool";"throttle"]
                               ~spool_id:(Message_id.to_string msgid)
                               "enqueuing message"));
      match Message.status spooled_msg with
      | `Delivered  | `Sending | `Frozen | `Removed | `Quarantined _ ->
        (* Only actually call [send] if the status is [`Send_now]. Between the time an
           async job was scheduled and the time it gets run, the status of the message
           can change. We make sure not to try to send if we have manually intervened,
           causing a permanent status change. *)
        return ()
      | `Send_now  | `Send_at _ ->
        Message.send spooled_msg ~log:t.log ~client_cache:t.client_cache
        >>| function
        | Error e ->
          Log.error t.log (lazy (Log.Message.of_error
                                   ~here:[%here]
                                   ~flows:(Message.flows spooled_msg)
                                   ~component:["spool";"send"]
                                   ~spool_id:(Message_id.to_string msgid)
                                   e))
        | Ok () ->
          match Message.status spooled_msg with
          | `Delivered ->
            remove_message t spooled_msg;
            delivered_event t ~here:[%here] spooled_msg
          | `Send_now ->
            enqueue t spooled_msg
          | `Send_at at ->
            enqueue ~at t spooled_msg
          | `Sending ->
            failwithf !"Message has status Sending after returning from \
                        Message.send: %{Message_id}"
              (Message.id spooled_msg) ()
          | `Frozen | `Removed | `Quarantined _ -> ()
    end)
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
    enqueue ~at t msg
  | `Send_now ->
    Log.info t.log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows:(Message.flows msg)
                            ~component:["spool";"queue"]
                            ~spool_id:(Message_id.to_string (Message.id msg))
                            "send now"));
    enqueue t msg

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
    Message.load entry
    >>| function
    | Error e ->
      Log.error t.log (lazy (Log.Message.of_error
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

let add t ~flows ~original_msg messages =
  Deferred.Or_error.List.iter messages ~how:`Parallel ~f:(fun envelope_with_next_hop ->
    Message.create
      t.spool
      ~log:t.log
      ~flows:(Log.Flows.extend flows `Outbound_envelope)
      ~initial_status:`Send_now
      envelope_with_next_hop
      ~original_msg
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
      Message.create
        t.spool
        ~log:t.log
        ~flows:(Log.Flows.extend flows `Outbound_envelope)
        ~initial_status:(`Quarantined reason)
        envelope_with_next_hop
        ~original_msg
      >>|? fun quarantined_msg ->
      quarantine_event ~here:[%here] t (`Reason reason) quarantined_msg)
;;

let kill_and_flush ?(timeout = Deferred.never ()) t =
  t.is_killed <- true;
  Deferred.choose
    [ Deferred.choice timeout
        (fun () -> `Timeout)
    ; Deferred.choice (Client_cache.close_and_flush t.client_cache)
        (fun () -> `Finished) ]
;;

let with_outstanding_msg t id ~f =
  match Hashtbl.find t.messages id with
  | None ->
    return (Or_error.error_string "Not found")
  | Some spooled_msg ->
    f spooled_msg

let freeze t ids =
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    with_outstanding_msg t id ~f:(fun spooled_msg ->
      Message.freeze spooled_msg ~log:t.log
      >>=? fun () ->
      frozen_event t ~here:[%here] spooled_msg;
      Deferred.Or_error.ok_unit))
;;

module Send_info = struct
  type t =
    [ `All_messages
    | `Frozen_only
    | `Some_messages of Message_id.t list ] [@@deriving bin_io]
end

let send_msgs ?(retry_intervals = []) t ids =
  let do_send ?event msg =
    Message.mark_for_send_now ~retry_intervals msg ~log:t.log
    >>=? fun () ->
    Option.iter event ~f:(fun event -> event t msg);
    enqueue t msg;
    Deferred.Or_error.ok_unit
  in
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    with_outstanding_msg t id ~f:(fun msg ->
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
  )
;;

let remove t ids =
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    with_outstanding_msg t id ~f:(fun spooled_msg ->
      begin
        match Message.status spooled_msg with
        | `Frozen ->
          Message.remove spooled_msg ~log:t.log
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
    { msgs :
        [ `Removed of Message_id.t list
        | `Quarantined of Message_id.t list ]
    ; wrapper : Email_message.Wrapper.t option
    } [@@deriving bin_io]
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
    Message.load entry
    >>= function
    | Error e ->
      let e = Error.tag e ~tag:"Failed to recover message" in
      Log.error t.log (lazy (Log.Message.of_error
                               ~here:[%here]
                               ~flows:Log.Flows.none
                               ~component:["spool";"admin"]
                               ~spool_id:(Message_id.to_string id)
                               e));
      return (Error e)
    | Ok msg ->
      Option.value_map info.wrapper ~default:Deferred.Or_error.ok_unit ~f:(fun wrapper ->
        Message.map_email msg ~f:(fun email ->
          Email_message.Wrapper.add wrapper email))
      >>=? fun () ->
      Message.freeze msg ~log:t.log
      >>=? fun () ->
      add_message t msg;
      recover_event t ~here:[%here] from_queue' msg;
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
  type t =
    { message : Message.t
    ; file_size : Byte_units.t option
    ; envelope : Envelope.t option
    } [@@deriving fields, sexp, bin_io]

  let sp f t = f t.message

  let id                 = sp Message.id
  let spool_date         = sp Message.spool_date
  let last_relay_attempt = sp Message.last_relay_attempt
  let parent_id          = sp Message.parent_id
  let status             = sp Message.status

  (* Internal *)
  let time_on_spool      = sp Message.time_on_spool
  let next_hop_choices   = sp Message.next_hop_choices
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
        sprintf !"%4s %5s %{Message_id} <%s> %s\n           %s\n"
          time size (S.id msg) (Envelope.string_sender envelope)
          frozen_text recipients
      | None ->
        sprintf !"%4s %5s %{Message_id} %s (sender and recipients hidden)\n"
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
              (fun a -> S.id a |> Message_id.to_string)
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
  |> List.map ~f:(fun message ->
    { Spooled_message_info. message
    ; envelope = None; file_size = None })

let status_from_disk config =
  Message_spool.load (Config.spool_dir config)
  >>=? fun spool ->
  Message_spool.ls spool [Message_queue.Active; Message_queue.Frozen]
  >>=? fun entries ->
  Deferred.List.map entries ~f:(fun entry ->
    Message.load_with_envelope entry
    >>=? fun (message, envelope) ->
    Message.size_of_file message
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
