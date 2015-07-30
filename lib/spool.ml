open Core.Std
open Async.Std
open Async_extended.Std

module Config = Server_config

open Types

module Spooled_message = Spooled_message_internal
module Spooled_message_id = Spooled_message.Id

module Event = struct
  module T = struct
    type t = Time.t * [ `Spooled   of Spooled_message.Id.t
                      | `Delivered of Spooled_message.Id.t
                      | `Frozen    of Spooled_message.Id.t
                      | `Unfrozen  of Spooled_message.Id.t
                      | `Ping ]
    with sexp, bin_io, compare

    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let to_string (_rcvd_time, happening) =
    let without_time =
      let id_to_s = Spooled_message.Id.to_string in
      match happening with
      | `Spooled   msgid -> (id_to_s msgid) ^ " spooled"
      | `Frozen    msgid -> (id_to_s msgid) ^ " frozen"
      | `Unfrozen  msgid -> (id_to_s msgid) ^ " unfrozen"
      | `Delivered msgid -> (id_to_s msgid) ^ " delivered"
      | `Ping            -> "ping"
    in
    without_time
  ;;

  let event_gen ~f event_stream spooled_msg =
    let id = Spooled_message.id spooled_msg in
    let t = f ~id spooled_msg in
    Log.Global.debug !"writing event %{sexp:t} to stream" t;
    Bus.write event_stream t
  ;;

  let spooled   = event_gen ~f:(fun ~id  msg -> (Spooled_message.spool_date msg, `Spooled id))
  let delivered = event_gen ~f:(fun ~id _msg -> Time.now (), `Delivered id)
  let frozen    = event_gen ~f:(fun ~id _msg -> Time.now (), `Frozen id)
  let unfrozen  = event_gen ~f:(fun ~id _msg -> Time.now (), `Unfrozen id)
end

type t =
  { spool_dir     : Spool_directory.t
  ; send_throttle : Adjustable_throttle.t
  (* Don't hit the max open files system limit. *)
  ; file_throttle : unit Throttle.t
  ; messages      : Spooled_message.t Spooled_message.Id.Table.t
  ; event_stream  : Event.t Bus.t
  ; client        : Client_config.t
  } with fields
;;

let add_message t msg =
  let id = Spooled_message.id msg in
  match Hashtbl.add t.messages ~key:id ~data:msg with
  | `Ok -> ()
  | `Duplicate ->
    Log.Global.error "Message already in spool: %s"
      (Spooled_message.Id.to_string id)

let remove_message t msg =
  let id = Spooled_message.id msg in
  if not (Hashtbl.mem t.messages id)
  then Log.Global.error "Trying to remove message that is not in spool: %s"
    (Spooled_message.Id.to_string id);
  Hashtbl.remove t.messages id

let with_event_writer spool spooled_msg ~f = f spool.event_stream spooled_msg
let spooled_event   = with_event_writer ~f:Event.spooled
let delivered_event = with_event_writer ~f:Event.delivered
let frozen_event    = with_event_writer ~f:Event.frozen
let unfrozen_event  = with_event_writer ~f:Event.unfrozen

let create ~config : t Deferred.Or_error.t =
  let spool_dir = Config.spool_dir config in
  Spool_directory.init ~path:spool_dir
  >>|? fun spool_dir ->
  let event_stream = Bus.create ~can_subscribe_after_start:true in
  let max_concurrent_jobs = Config.max_concurrent_send_jobs config in
  let send_throttle = Adjustable_throttle.create ~max_concurrent_jobs in
  let file_throttle =
    Throttle.create ~continue_on_error:true ~max_concurrent_jobs:200
  in
  Bus.start event_stream;
  Clock.every (sec 30.)
    (fun () ->
      Log.Global.debug "pinging event stream";
      Bus.write event_stream (Time.now (), `Ping));
  let messages = Spooled_message.Id.Table.create () in
  Fields.create
    ~spool_dir
    ~send_throttle
    ~file_throttle
    ~messages
    ~event_stream
    ~client:config.Config.client
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
      Log.Global.debug !"enqueuing message %{Spooled_message.Id}" msgid;
      Adjustable_throttle.enqueue t.send_throttle (fun () ->
        Spooled_message.send spooled_msg ~config:t.client
        >>| function
        | Error e ->
          Log.Global.error !"error sending %{Spooled_message.Id}: %s"
            (Spooled_message.id spooled_msg) (Error.to_string_hum e)
        | Ok () ->
          match Spooled_message.status spooled_msg with
          | `Delivered ->
            remove_message t spooled_msg;
            delivered_event t spooled_msg
          | `Send_now ->
            enqueue t spooled_msg
          | `Send_at at ->
            enqueue ~at t spooled_msg
          | `Sending ->
            failwithf !"Message has status Sending after returning from \
                        Spooled_message.send: %{Spooled_message.Id}"
              (Spooled_message.id spooled_msg) ()
          | `Frozen -> ())
    end)
;;

let schedule t msg =
  match Spooled_message.status msg with
  | `Frozen  -> ()
  | (`Sending | `Delivered) as status ->
    failwithf !"Unexpected status %{sexp:Spooled_message.Status.t} when trying \
                to schedule message %{Spooled_message.Id}"
      status (Spooled_message.id msg) ()
  | `Send_at at ->
    Log.Global.info !"spooling %{Spooled_message.Id} at %{Time}"
      (Spooled_message.id msg) at;
    enqueue ~at t msg
  | `Send_now ->
    Log.Global.info !"spooling %{Spooled_message.Id}"
      (Spooled_message.id msg);
    enqueue t msg

let load t =
  Spool_directory.ls t.spool_dir
  >>=? fun entries ->
  Log.Global.debug !"found spooled message files: %{sexp: string list}" entries;
  Deferred.List.iter entries ~how:`Parallel ~f:(fun entry ->
    Throttle.enqueue t.file_throttle (fun () -> Spooled_message.load entry)
    >>| function
    | Error e ->
      let e = Error.tag e "Failed to load file in spool" in
      Log.Global.error "%s" (Error.to_string_hum e)
    | Ok msg ->
      Log.Global.info !"loading %{Spooled_message.Id}" (Spooled_message.id msg);
      add_message t msg;
      schedule t msg)
  >>= fun () ->
  return (Ok ())
;;

let create ~config () =
  create ~config
  >>=? fun t ->
  load t
  >>=? fun () ->
  return (Ok t)
;;

let add t ~original_msg messages =
  Deferred.Or_error.List.iter messages ~how:`Parallel ~f:(fun envelope_with_next_hop ->
    Throttle.enqueue t.file_throttle (fun () ->
      Spooled_message.create
        t.spool_dir
        envelope_with_next_hop
        ~original_msg)
      >>|? fun spooled_msg ->
      add_message t spooled_msg;
      enqueue t spooled_msg;
      spooled_event t spooled_msg)
  >>|? fun () ->
  Envelope.id original_msg
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
        Spooled_message.freeze spooled_msg)
      >>=? fun () ->
      frozen_event t spooled_msg;
      Deferred.Or_error.ok_unit))
;;

let send_now ?(new_retry_intervals = []) t ids =
  Deferred.Or_error.List.iter ids ~how:`Parallel ~f:(fun id ->
    with_outstanding_msg t id ~f:(fun msg ->
      Throttle.enqueue t.file_throttle (fun () ->
        Spooled_message.unfreeze ~new_retry_intervals msg)
      >>=? fun () ->
      unfrozen_event t msg;
      enqueue t msg;
      Deferred.Or_error.ok_unit))
;;

module Spooled_message_info = struct
  type t =
    { spooled_message : Spooled_message.t
    ; file_size : Byte_units.t option
    ; envelope : Envelope.t option
    } with fields, sexp, bin_io

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
  type t = Spooled_message_info.t list with sexp, bin_io

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
        | `Send_at _ | `Send_now | `Sending | `Delivered -> ""
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
              | `Send_at at ->
                display_span (Time.diff at (Time.now ()))
              | (`Send_now | `Sending) ->
                [`Green], "now"
              | `Frozen ->
                [`Red], "frozen"
              | `Delivered ->
                [`Green], "delivered"
            )
          in
          let next_hop =
            Column.create "next hop" (fun a ->
              S.next_hop_choices a
              |> List.map ~f:Host_and_port.to_string
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
  Log.Global.debug "received event stream subscription";
  Bus.reader_exn t.event_stream
;;
