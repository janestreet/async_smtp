open Core
open Async
open Async_smtp_types
module Log = Mail_log

let compare _ _ = `You_are_using_poly_compare
let _silence_unused_warning = compare

module On_disk_spool = Message.On_disk_spool

let create = On_disk_spool.create
let load str = On_disk_spool.load str

let ls t queues =
  Deferred.Or_error.List.concat_map ~how:`Sequential queues ~f:(fun queue ->
    On_disk_spool.list t queue)
;;

let uncheckout_from_queue t queue =
  let module Checked_out_entry = On_disk_spool.Expert.Checked_out_entry in
  match%bind On_disk_spool.Expert.list_checkouts_unsafe t queue with
  | Error e -> return ([], [ e ])
  | Ok checked_out_entries ->
    List.map checked_out_entries ~f:(fun checked_out_entry ->
      Checked_out_entry.save checked_out_entry (Checked_out_entry.queue checked_out_entry)
      >>| Result.map ~f:(fun () -> Checked_out_entry.name checked_out_entry))
    |> Deferred.List.all
    >>| List.partition_result
;;

let uncheckout_all_entries t =
  let%bind recovered, errors =
    Deferred.List.map ~how:`Sequential Message.Queue.all ~f:(uncheckout_from_queue t)
    >>| List.unzip
  in
  let recovered, errors = List.concat recovered, List.concat errors in
  let errors =
    match errors with
    | [] -> None
    | _ -> Some (Error.of_list errors)
  in
  return (`Recovered recovered, `Errors errors)
;;

module Entry = struct
  include On_disk_spool.Entry

  let to_message entry = On_disk_spool.Entry.Direct.contents entry

  let to_message_with_envelope entry =
    let open Deferred.Or_error.Let_syntax in
    let%bind meta = to_message entry in
    let data_file = On_disk_spool.Entry.Direct.data_file entry in
    let%bind data = On_disk_spool.Data_file.load data_file in
    let email = Message.Data.to_email data in
    return (meta, Smtp_envelope.create' ~info:(Message.envelope_info meta) ~email)
  ;;

  let size entry =
    let open Deferred.Or_error.Let_syntax in
    let data_file = On_disk_spool.Entry.Direct.data_file entry in
    let%bind stats = On_disk_spool.Data_file.stat data_file in
    let size = Unix.Stats.size stats in
    return (Byte_units.of_bytes_int64_exn size)
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

let enqueue
  spool
  ~log:_
  ~initial_status
  ~set_related_ids
  envelope_batch
  ~flows
  ~original_msg
  =
  let parent_id = Smtp_envelope.id original_msg in
  Message.Queue.of_status' initial_status
  |> Deferred.return
  >>=? fun queue ->
  Message.of_envelope_batch
    envelope_batch
    ~gen_id:(fun () -> On_disk_spool.Unique_name.reserve spool original_msg)
    ~spool_dir:(On_disk_spool.dir spool)
    ~spool_date:(Time_float.now ())
    ~failed_recipients:[]
    ~relay_attempts:[]
    ~parent_id
    ~set_related_ids
    ~status:initial_status
    ~flows
  >>=? fun messages_with_data ->
  Deferred.Or_error.List.map
    ~how:`Sequential
    messages_with_data
    ~f:(fun (meta, data, envelope) ->
      let id = Message.id meta in
      enqueue spool queue ~meta ~id ~data >>|? fun () -> meta, envelope)
;;

let with_file
  t
  (f : On_disk_spool.Data_file.t -> ([ `Sync_meta | `Unlink ] * 'a) Or_error.t Deferred.t)
  : 'a Or_error.t Deferred.t
  =
  entry t
  >>=? fun entry ->
  return (Message.Queue.of_status' (Message.status t))
  >>=? fun original_queue ->
  On_disk_spool.with_entry entry ~f:(fun meta data_file ->
    match Message.compare t meta = 0 with
    | false ->
      let e =
        Error.create
          "spooled message in memory differs from spooled message on disk"
          (`In_memory t, `On_disk meta, `Entry entry)
          [%sexp_of:
            [ `In_memory of Message.t ]
            * [ `On_disk of Message.t ]
            * [ `Entry of On_disk_spool.Entry.t ]]
      in
      return (`Save (meta, original_queue), Error e)
    | true ->
      (match%bind f data_file with
       | Error _ as e -> return (`Save (meta, original_queue), e)
       | Ok (`Unlink, res) -> return (`Remove, Ok res)
       | Ok (`Sync_meta, res) ->
         (* Derive queue from mutable [Message.status t] as it may have changed in [~f] *)
         (match%bind
            Message.Queue.of_status' (Message.status t)
            |> Or_error.tag ~tag:(Sexp.to_string (Message.sexp_of_t t))
            |> Deferred.return
          with
          | Error _ as e -> return (`Save (meta, original_queue), e)
          | Ok new_queue -> return (`Save (t, new_queue), Ok res))))
  >>| Or_error.join
;;

let freeze_without_file t ~log =
  Log.info
    log
    (lazy
      (Log.Message.create
         ~here:[%here]
         ~flows:(Message.flows t)
         ~component:[ "spool" ]
         ~spool_id:(Message.Id.to_string (Message.id t))
         "frozen"));
  Message.set_status t `Frozen;
  return (Ok (`Sync_meta, ()))
;;

let freeze t ~log = with_file t (fun _data_file -> freeze_without_file t ~log)

let mark_for_send_now ~retry_intervals t ~log =
  with_file t (fun _data_file ->
    Log.info
      log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows:(Message.flows t)
           ~component:[ "spool" ]
           ~spool_id:(Message.Id.to_string (Message.id t))
           "send_now"));
    Message.set_status t `Send_now;
    Message.add_retry_intervals t retry_intervals;
    Message.move_failed_recipients_to_remaining_recipients t;
    return (Ok (`Sync_meta, ())))
;;

let mark_for_send_at_without_file t ~at ~log:_ =
  Message.set_status t (`Send_at at);
  return (Ok (`Sync_meta, ()))
;;

let mark_for_send_at t ~at ~log =
  with_file t (fun _data_file -> mark_for_send_at_without_file t ~at ~log)
;;

let remove_without_file t ~log =
  Log.info
    log
    (lazy
      (Log.Message.create
         ~here:[%here]
         ~flows:(Message.flows t)
         ~component:[ "spool" ]
         ~spool_id:(Message.Id.to_string (Message.id t))
         "removing"));
  Message.set_status t `Removed;
  return (Ok (`Sync_meta, ()))
;;

let remove t ~log = with_file t (fun _data_file -> remove_without_file t ~log)

let send_envelope_via_sendfile
  client
  ~log
  ~flows
  ~component
  ~spool_date
  envelope_info
  data_file
  =
  let send_data client =
    let socket_fd = Writer.fd (Client_raw.writer client) in
    Async_sendfile.sendfile
      ~fd:socket_fd
      ~file:(On_disk_spool.Data_file.path data_file)
      ()
  in
  Client.Expert.send_envelope
    client
    ~log
    ~flows
    ~component
    ~spool_date
    ~send_data
    envelope_info
;;

module Partitioned_recipients = struct
  type 'a t =
    { fail_permanently : 'a list
    ; try_later : 'a list
    ; done_ : 'a list
    ; try_later_rate_limited : bool
    }
  [@@deriving sexp_of]
end

let partition_send_results results : 'a Partitioned_recipients.t =
  let try_later_rate_limited = ref false in
  let fail_permanently, try_later, done_ =
    List.partition3_map results ~f:(fun (recipient, result) ->
      match result with
      | `Fail_permanently -> `Fst recipient
      | `Try_later -> `Snd recipient
      | `Try_later_rate_limited ->
        try_later_rate_limited := true;
        `Snd recipient
      | `Done -> `Trd recipient)
  in
  { fail_permanently; try_later; done_; try_later_rate_limited = !try_later_rate_limited }
;;

let%expect_test "partition_send_results" =
  let test decisions =
    let result =
      List.mapi decisions ~f:(fun i decision -> sprintf "r%d" i, decision)
      |> partition_send_results
    in
    print_s [%sexp (result : string Partitioned_recipients.t)]
  in
  let () = test [] in
  [%expect
    {|
    ((fail_permanently ()) (try_later ()) (done_ ())
     (try_later_rate_limited false))
    |}];
  let () = test [ `Fail_permanently; `Fail_permanently ] in
  [%expect
    {|
    ((fail_permanently (r0 r1)) (try_later ()) (done_ ())
     (try_later_rate_limited false))
    |}];
  let () = test [ `Try_later; `Try_later ] in
  [%expect
    {|
    ((fail_permanently ()) (try_later (r0 r1)) (done_ ())
     (try_later_rate_limited false))
    |}];
  let () = test [ `Try_later_rate_limited ] in
  [%expect
    {|
    ((fail_permanently ()) (try_later (r0)) (done_ ())
     (try_later_rate_limited true))
    |}];
  let () = test [ `Done; `Done ] in
  [%expect
    {|
    ((fail_permanently ()) (try_later ()) (done_ (r0 r1))
     (try_later_rate_limited false))
    |}];
  let () =
    test
      [ `Fail_permanently; `Try_later; `Try_later_rate_limited; `Done; `Fail_permanently ]
  in
  [%expect
    {|
    ((fail_permanently (r0 r4)) (try_later (r1 r2)) (done_ (r3))
     (try_later_rate_limited true))
    |}];
  return ()
;;

let send_to_hops t ~log ~client_cache ~on_error data_file =
  let hops_tag =
    Sexp.to_string ([%sexp_of: Host_and_port.t list] (Message.next_hop_choices t))
  in
  Log.debug
    log
    (lazy
      (Log.Message.create
         ~here:[%here]
         ~flows:(Message.flows t)
         ~component:[ "spool"; "send" ]
         ~spool_id:(Message.Id.to_string (Message.id t))
         ~tags:[ "hops", hops_tag ]
         "attempting delivery"));
  match%bind
    Client_cache.Tcp.with_'
      ~give_up:(Clock.after (Time_float.Span.of_min 2.))
      ~cache:client_cache
      (Message.next_hop_choices t)
      ?route:(Smtp_envelope.Info.route (Message.envelope_info t))
      ~f:(fun ~flows client ->
        let flows = Log.Flows.union (Message.flows t) flows in
        let envelope_info =
          Smtp_envelope.Info.set
            (Message.envelope_info t)
            ~recipients:(Message.remaining_recipients t)
            ()
        in
        send_envelope_via_sendfile
          client
          ~log
          ~flows
          ~component:[ "spool"; "send" ]
          ~spool_date:(Message.spool_date t)
          envelope_info
          data_file)
  with
  | `Ok (hop, Error e) ->
    (* The client logs many common failures, so this might be repetitive. But duplication
       in the error case is better than missing potential errors. *)
    let e = Error.tag ~tag:"Unable to send envelope" e in
    Log.info
      log
      (lazy
        (Log.Message.of_error
           ~here:[%here]
           ~flows:(Message.flows t)
           ~component:[ "spool"; "send" ]
           ~spool_id:(Message.Id.to_string (Message.id t))
           ~remote_address:hop
           e));
    Message.add_relay_attempt t (Time_float.now (), e);
    return `Try_later
  | `Error_opening_all_addresses hops_and_errors ->
    List.iter hops_and_errors ~f:(fun (hop, e) ->
      let e = Error.tag ~tag:"Unable to open connection for hop" e in
      Log.info
        log
        (lazy
          (Log.Message.of_error
             ~here:[%here]
             ~flows:(Message.flows t)
             ~component:[ "spool"; "send" ]
             ~spool_id:(Message.Id.to_string (Message.id t))
             ~remote_address:hop
             e)));
    let e = Error.createf "No hops available" in
    Message.add_relay_attempt t (Time_float.now (), e);
    return `Try_later
  | `Gave_up_waiting_for_address ->
    let e = Error.createf "Gave up waiting for client" in
    Log.info
      log
      (lazy
        (Log.Message.of_error
           ~here:[%here]
           ~flows:(Message.flows t)
           ~component:[ "spool"; "send" ]
           ~spool_id:(Message.Id.to_string (Message.id t))
           ~tags:[ "hops", hops_tag ]
           e));
    Message.add_relay_attempt t (Time_float.now (), e);
    return `Try_later
  | `Cache_is_closed ->
    (* Shutdown is initiated, so stop trying to resend. *)
    Log.info
      log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows:(Message.flows t)
           ~component:[ "spool"; "send" ]
           ~spool_id:(Message.Id.to_string (Message.id t))
           ~tags:[ "hops", hops_tag ]
           "Cache is closed"));
    return `Try_later
  | `Ok (_hop, Ok envelope_status) ->
    (match
       Client.Envelope_status.ok_or_error ~allow_rejected_recipients:false envelope_status
     with
     | Ok _msg_id ->
       (* Already logged by the client *)
       return `Done
     | Error e ->
       (* We are being conservative here for simplicity - if we get a permanent error from
          one hop, we assume that we would get the same error from the remaining hops, and
          call [on_error] to decide what to do. In order to send bounce messages,
          [on_error] needs the full envelope. *)
       let on_error ~log reply =
         let load_envelope () =
           let%map.Deferred.Or_error email =
             On_disk_spool.Data_file.load data_file >>|? Message.Data.to_email
           in
           Smtp_envelope.create' ~info:(Message.envelope_info t) ~email
         in
         on_error ~log ~load_envelope reply
       in
       (* Already logged by the client *)
       Message.add_relay_attempt t (Time_float.now (), e);
       (match envelope_status with
        | Ok (_ (* envelope_id *), rejected_recipients)
        | Error (`Rejected_all_recipients rejected_recipients) ->
          let%bind { fail_permanently; try_later; done_; try_later_rate_limited } =
            Deferred.List.map
              ~how:`Sequential
              rejected_recipients
              ~f:(fun (recipient, reject) ->
                let%map.Deferred result = on_error ~log reject in
                recipient, result)
            >>| partition_send_results
          in
          (* We don't need to track recipients who have received the email. *)
          let _ = done_ in
          Message.set_remaining_recipients t try_later;
          Message.set_failed_recipients t (Message.failed_recipients t @ fail_permanently);
          (match
             ( List.is_empty (Message.remaining_recipients t)
             , List.is_empty (Message.failed_recipients t) )
           with
           | true, true -> return `Done
           | true, false -> return `Fail_permanently
           | false, false | false, true ->
             return
               (if try_later_rate_limited then `Try_later_rate_limited else `Try_later))
        | Error
            ( `Rejected_sender r
            | `Rejected_sender_and_recipients (r, _)
            | `Rejected_body (r, _) ) -> on_error ~log r))
;;

let do_send_without_file t ~log ~client_cache ~on_error ~get_envelope =
  let last_relay_error t =
    match Message.last_relay_attempt t with
    | None -> Error.of_string "No relay attempt"
    | Some (_, e) -> e
  in
  Message.set_status t `Sending;
  match%bind send_to_hops t ~log ~client_cache ~on_error get_envelope with
  | `Done ->
    Message.set_status t `Delivered;
    Deferred.Or_error.return (`Unlink, (`Delivered, None))
  | (`Fail_permanently | `Try_later | `Try_later_rate_limited) as fail ->
    (match fail, Message.retry_intervals t with
     | `Fail_permanently, _ | (`Try_later | `Try_later_rate_limited), [] ->
       let delivery_failure = last_relay_error t in
       let%map.Deferred.Or_error action, () = freeze_without_file t ~log in
       action, (`Frozen, Some delivery_failure)
     | ((`Try_later | `Try_later_rate_limited) as try_later), r :: rs ->
       let delivery_failure = last_relay_error t in
       let at = Time_float.(add (now ()) (Smtp_envelope.Retry_interval.to_span r)) in
       let%map.Deferred.Or_error action, () = mark_for_send_at_without_file t ~at ~log in
       Message.set_retry_intervals t rs;
       let delayed =
         match try_later with
         | `Try_later -> `Delayed_to at
         | `Try_later_rate_limited -> `Delayed_to_rate_limited at
       in
       action, (delayed, Some delivery_failure))
;;

let do_send t ~log ~client_cache ~presend ~on_error =
  with_file t (fun get_envelope ->
    match%bind presend ~log with
    | `Freeze ->
      let%map.Deferred.Or_error action, () = freeze_without_file t ~log in
      action, (`Frozen, None)
    | `Remove ->
      let%map.Deferred.Or_error action, () = remove_without_file t ~log in
      action, (`Removed, None)
    | `Send_at at ->
      let%map.Deferred.Or_error action, () = mark_for_send_at_without_file t ~at ~log in
      action, (`Delayed_to at, None)
    | `Send_now -> do_send_without_file t ~log ~client_cache ~on_error ~get_envelope)
;;

let send t ~log ~client_cache ~presend ~on_error =
  match Message.status t with
  | `Send_now | `Send_at _ -> do_send t ~log ~client_cache ~presend ~on_error
  | `Frozen -> return (Or_error.error_string "Message.send: message is frozen")
  | `Removed -> return (Or_error.error_string "Message.send: message is removed")
  | `Quarantined _ ->
    return (Or_error.error_string "Message.send: message is quarantined")
  | `Sending ->
    return (Or_error.error_string "Message.send: message is already being sent")
  | `Delivered -> return (Or_error.error_string "Message.send: message is delivered")
;;

module On_disk_monitor = struct
  include Multispool.Monitor.Make (Message.On_disk)
end
