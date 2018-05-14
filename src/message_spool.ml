open Core
open Async
open Async_smtp_types

module Log = Mail_log

let compare _ _ = `You_are_using_poly_compare
let _silence_unused_warning = compare

module On_disk_spool = Message.On_disk_spool

let create   = On_disk_spool.create
let load str = On_disk_spool.load str

let ls t queues =
  Deferred.Or_error.List.concat_map queues ~f:(fun queue -> On_disk_spool.list t queue)
;;

module Entry = struct
  include On_disk_spool.Entry

  let to_message entry =
    On_disk_spool.Entry.Direct.contents entry
  ;;

  let to_message_with_envelope entry =
    let open Deferred.Or_error.Let_syntax in
    let%bind meta = to_message entry in
    let data_file = On_disk_spool.Entry.Direct.data_file entry in
    let%bind data = On_disk_spool.Data_file.load data_file in
    let email = Message.Data.to_email data in
    return (meta, Smtp_envelope.create' ~info:(Message.envelope_info meta) ~email)
  ;;
end

let entry t =
  let open Deferred.Or_error.Let_syntax in
  let spool = On_disk_spool.load_unsafe (Message.spool_dir t) in
  let%map queue = Message.Queue.of_status' (Message.status t) |> Deferred.return in
  Entry.create spool queue ~name:(Message.Id.to_string (Message.id t))
;;

type t = On_disk_spool.t
type spool = t

let enqueue meta_spool queue ~meta ~id ~data =
  let open Deferred.Or_error.Let_syntax in
  let%bind (_ : On_disk_spool.Entry.t) =
    On_disk_spool.enqueue meta_spool queue meta data (`Use id)
  in
  Deferred.Or_error.ok_unit
;;

let enqueue spool ~log:_ ~initial_status envelope_batch ~flows ~original_msg =
  let parent_id = Smtp_envelope.id original_msg in
  Message.Queue.of_status' initial_status |> Deferred.return
  >>=? fun queue ->
  Message.of_envelope_batch
    envelope_batch
    ~gen_id:(fun () -> On_disk_spool.Unique_name.reserve spool original_msg)
    ~spool_dir:(On_disk_spool.dir spool)
    ~spool_date:(Time.now ())
    ~failed_recipients:[]
    ~relay_attempts:[]
    ~parent_id
    ~status:initial_status
    ~flows
  >>=? fun messages_with_data ->
  Deferred.Or_error.List.map messages_with_data ~f:(fun (meta, data) ->
    let id = Message.id meta in
    enqueue spool queue ~meta ~id ~data
    >>|? fun () ->
    meta)
;;

let with_file t
      (f : On_disk_spool.Data_file.t ->
       ([`Sync_meta | `Sync_email of Email.t | `Unlink] * 'a) Or_error.t Deferred.t)
  : 'a Or_error.t Deferred.t =
  entry t
  >>=? fun entry ->
  return (Message.Queue.of_status' (Message.status t))
  >>=? fun original_queue ->
  On_disk_spool.with_entry entry
    ~f:(fun meta data_file ->
      match Message.compare t meta = 0 with
      | false ->
        let e =
          Error.create
            "spooled message in memory differs from spooled message on disk"
            (`In_memory t, `On_disk meta, `Entry entry)
            [%sexp_of: [`In_memory of Message.t] * [`On_disk of Message.t] * [`Entry of On_disk_spool.Entry.t]]
        in
        return (`Save (meta, original_queue), Error e)
      | true ->
        f data_file
        >>= function
        | Error _ as e -> return (`Save (meta, original_queue), e)
        | Ok (`Unlink, res) -> return (`Remove, Ok res)
        | Ok (`Sync_email email, res) ->
          let data = Message.Data.of_email email in
          On_disk_spool.Data_file.save data_file ~contents:data
          >>| fun save_data_result ->
          let res = Or_error.map save_data_result ~f:(fun () -> res) in
          (`Save (meta, original_queue), res)
        | Ok (`Sync_meta, res) ->
          (* Derive queue from mutable [Message.status t] as it may have changed in [~f] *)
          Message.Queue.of_status' (Message.status t)
          |> Or_error.tag ~tag:(Sexp.to_string (Message.sexp_of_t t))
          |> Deferred.return
          >>= function
          | Error _ as e -> return (`Save (meta, original_queue), e)
          | Ok new_queue ->
            return (`Save (t, new_queue), Ok res)
    )
  >>| Or_error.join
;;

let map_email t ~f =
  with_file t (fun data_file ->
    On_disk_spool.Data_file.load data_file
    >>=? fun data ->
    let email = f (Message.Data.to_email data) in
    return (Ok (`Sync_email email, ())))
;;

let freeze t ~log =
  with_file t (fun _data_file ->
    Log.info log (lazy (Log.Message.create
                          ~here:[%here]
                          ~flows:(Message.flows t)
                          ~component:["spool"]
                          ~spool_id:(Message.Id.to_string (Message.id t))
                          "frozen"));
    Message.set_status t `Frozen;
    return (Ok (`Sync_meta, ())))
;;

let mark_for_send_now ~retry_intervals t ~log =
  with_file t (fun _data_file ->
    Log.info log (lazy (Log.Message.create
                          ~here:[%here]
                          ~flows:(Message.flows t)
                          ~component:["spool"]
                          ~spool_id:(Message.Id.to_string (Message.id t))
                          "send_now"));
    Message.set_status t `Send_now;
    Message.add_retry_intervals t retry_intervals;
    Message.move_failed_recipients_to_remaining_recipients t;
    return (Ok (`Sync_meta, ())))
;;

let remove t ~log =
  with_file t (fun _data_file ->
    Log.info log (lazy (Log.Message.create
                          ~here:[%here]
                          ~flows:(Message.flows t)
                          ~component:["spool"]
                          ~spool_id:(Message.Id.to_string (Message.id t))
                          "removing"));
    Message.set_status t `Removed;
    return (Ok (`Sync_meta, ())))
;;

let send_envelope_via_sendfile client ~log ~flows ~component envelope_info data_file =
  let send_data client =
    let socket_fd = Writer.fd (Client_raw.writer client) in
    Async_sendfile.sendfile
      ~socket_fd
      ~file:(On_disk_spool.Data_file.path data_file)
      ()
  in
  Client.Expert.send_envelope client ~log ~flows ~component ~send_data envelope_info
;;

let send_to_hops t ~log ~client_cache data_file =
  let hops_tag =
    Sexp.to_string ([%sexp_of: Smtp_socket_address.t list] (Message.next_hop_choices t))
  in
  Log.debug log (lazy (Log.Message.create
                         ~here:[%here]
                         ~flows:(Message.flows t)
                         ~component:["spool";"send"]
                         ~spool_id:(Message.Id.to_string (Message.id t))
                         ~tags:["hops", hops_tag]
                         "attempting delivery"));
  Client_cache.Tcp.with_'
    ~give_up:(Clock.after (Time.Span.of_min 2.))
    ~cache:client_cache (Message.next_hop_choices t)
    ?route:(Smtp_envelope.Info.route (Message.envelope_info t))
    ~f:(fun ~flows client ->
      let flows = Log.Flows.union (Message.flows t) flows in
      let envelope_info =
        Smtp_envelope.Info.set (Message.envelope_info t)
          ~recipients:(Message.remaining_recipients t) ()
      in
      send_envelope_via_sendfile client ~log ~flows ~component:["spool";"send"]
        envelope_info data_file)
  >>= function
  | `Ok (hop, (Error e)) ->
    (* The client logs many common failures, so this might be repetitive. But
       duplication in the error case is better than missing potential errors. *)
    let e = Error.tag ~tag:"Unable to send envelope" e in
    Log.info log (lazy (Log.Message.of_error
                          ~here:[%here]
                          ~flows:(Message.flows t)
                          ~component:["spool"; "send"]
                          ~spool_id:(Message.Id.to_string (Message.id t))
                          ~remote_address:hop
                          e));
    Message.add_relay_attempt t (Time.now (), e);
    return `Try_later
  | `Error_opening_all_addresses hops_and_errors ->
    List.iter hops_and_errors ~f:(fun (hop, e) ->
      let e = Error.tag ~tag:"Unable to open connection for hop" e in
      Log.info log (lazy (Log.Message.of_error
                            ~here:[%here]
                            ~flows:(Message.flows t)
                            ~component:["spool"; "send"]
                            ~spool_id:(Message.Id.to_string (Message.id t))
                            ~remote_address:hop
                            e)));
    let e = Error.createf "No hops available" in
    Message.add_relay_attempt t (Time.now (), e);
    return `Try_later
  | `Gave_up_waiting_for_address ->
    let e = Error.createf "Gave up waiting for client" in
    Log.info log (lazy (Log.Message.of_error
                          ~here:[%here]
                          ~flows:(Message.flows t)
                          ~component:["spool"; "send"]
                          ~spool_id:(Message.Id.to_string (Message.id t))
                          ~tags:["hops", hops_tag]
                          e));
    Message.add_relay_attempt t (Time.now (), e);
    return `Try_later
  | `Cache_is_closed ->
    (* Shutdown is initiated, so stop trying to resend. *)
    Log.info log (lazy (Log.Message.create
                          ~here:[%here]
                          ~flows:(Message.flows t)
                          ~component:["spool"; "send"]
                          ~spool_id:(Message.Id.to_string (Message.id t))
                          ~tags:["hops", hops_tag]
                          "Cache is closed"));
    return `Try_later
  | `Ok (_hop, (Ok envelope_status)) ->
    match
      Client.Envelope_status.ok_or_error
        ~allow_rejected_recipients:false
        envelope_status
    with
    | Ok _msg_id ->
      (* Already logged by the client *)
      return `Done
    | Error e ->
      (* We are being conservative here for simplicity - if we get a permanent error
         from one hop, we assume that we would get the same error from the remaining
         hops. *)
      (* Already logged by the client *)
      Message.add_relay_attempt t (Time.now (), e);
      match envelope_status with
      | Ok (_ (* envelope_id *), rejected_recipients)
      | Error (`No_recipients rejected_recipients) ->
        let permanently_failed_recipients, temporarily_failed_recipients =
          List.partition_map rejected_recipients ~f:(fun (recipient, reject) ->
            if Smtp_reply.is_permanent_error reject then `Fst recipient
            else `Snd recipient)
        in
        Message.set_remaining_recipients t temporarily_failed_recipients;
        Message.set_failed_recipients t
          (Message.failed_recipients t @ permanently_failed_recipients);
        if List.is_empty (Message.remaining_recipients t)
        then (return `Fail_permanently)
        else (return `Try_later)
      | Error (`Rejected_sender r
              | `Rejected_sender_and_recipients (r,_)
              | `Rejected_body (r,_)) ->
        if Smtp_reply.is_permanent_error r
        then (return `Fail_permanently)
        else (return `Try_later)
;;

let do_send t ~log ~client_cache =
  let last_relay_error t =
    match Message.last_relay_attempt t with
    | None -> Error.of_string "No relay attempt"
    | Some (_, e) -> e
  in
  with_file t (fun get_envelope ->
    Message.set_status t `Sending;
    send_to_hops t ~log ~client_cache get_envelope
    >>| function
    | `Done ->
      Message.set_status t `Delivered;
      Ok (`Unlink, `Delivered)
    | (`Fail_permanently | `Try_later) as fail ->
      match fail, Message.retry_intervals t with
      | `Fail_permanently, _ | `Try_later, [] ->
        let delivery_failure = last_relay_error t in
        Message.set_status t `Frozen;
        Ok (`Sync_meta, `Failed delivery_failure)
      | `Try_later, r :: rs ->
        let delivery_failure = last_relay_error t in
        Message.set_status t
          (`Send_at (Time.add (Time.now ()) (Smtp_envelope.Retry_interval.to_span r)));
        Message.set_retry_intervals t rs;
        Ok (`Sync_meta, `Failed delivery_failure))
;;

let send t ~log ~client_cache =
  match Message.status t with
  | `Send_now | `Send_at _ -> do_send t ~log ~client_cache
  | `Frozen ->
    return (Or_error.error_string
              "Message.send: message is frozen")
  | `Removed ->
    return (Or_error.error_string
              "Message.send: message is removed")
  | `Quarantined _ ->
    return (Or_error.error_string
              "Message.send: message is quarantined")
  | `Sending ->
    return (Or_error.error_string
              "Message.send: message is already being sent")
  | `Delivered ->
    return (Or_error.error_string
              "Message.send: message is delivered")
;;

let size_of_file t =
  let open Deferred.Or_error.Let_syntax in
  let%bind entry = entry t in
  let data_file = On_disk_spool.Entry.Direct.data_file entry in
  let%bind stats = On_disk_spool.Data_file.stat data_file in
  let size = Unix.Stats.size stats |> Float.of_int64 in
  return (Byte_units.create `Bytes size)
;;

module On_disk_monitor = struct
  include Multispool.Monitor.Make(Message.On_disk)
end
