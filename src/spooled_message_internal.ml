open Core.Std
open Core_extended.Std
open Async.Std
open Async_extended.Std
open Types

module Mutex = Async_mutex

module Log = Mail_log

let compare _ _ = `You_are_using_poly_compare
let _silence_unused_warning = compare

(* Includes parent id and an incrementing counter. *)
module Id  = struct
  include String

  let counter = ref 0

  let create ~envelope_with_next_hop:_ ~original_msg =
    let parent_id = Envelope.id original_msg in
    let t =
      sprintf !"%{Envelope.Id}-%s" parent_id
        (Types.urlbase64_encode_float ~length:6 (!counter |> Int.to_float))
    in
    incr counter;
    t
  ;;
end

module Locks = struct
  let t : Mutex.t Id.Table.t = Id.Table.create ()

  let lock id =
    let r = Hashtbl.find_or_add t id ~default:(fun () -> Mutex.create ()) in
    Mutex.lock r

  let release id =
    let r = Hashtbl.find_or_add t id ~default:(fun () -> Mutex.create ()) in
    Mutex.unlock r;
    match Mutex.try_lock r with
    | `Acquired ->
      Mutex.unlock r;
      Hashtbl.remove t id
    | `Not_acquired -> ()
end

module Status = struct
  type t =
    [ `Send_now
    | `Send_at of Time.t
    | `Sending
    | `Frozen
    | `Removed
    | `Quarantined of string
    | `Delivered
    ] [@@deriving sexp, bin_io, compare]
end

(* A value of type t should only be modified via with_file below. This
   guarantees that one has exclusive access to the spool file (guarded by a
   mutex) and all changes are properly flushed to disk. *)
type t =
  { spool_dir                    : Spool_directory.t
  ; id                           : Id.t
  (* with default so that sexps without a flowid still parse. *)
  ; flows                        : Log.Flows.t [@default Log.Flows.none]
  ; parent_id                    : Envelope.Id.t
  ; spool_date                   : Time.t
  ; next_hop_choices             : Address.t list
  (* The head of this list is the time at which we should attempt a delivery
     after the next time a delivery fails. *)
  ; mutable retry_intervals      : Time.Span.t list
  ; mutable remaining_recipients : Email_address.t list
  (* Currently not used, but saved to disk to aid in triaging frozen messages and failed
     deliveries. Addresses on this list will not be included in remaining_recipients, and
     would otherwise be lost to the ether. *)
  ; mutable failed_recipients    : Email_address.t list
  ; mutable relay_attempts       : (Time.t * Error.t) list
  ; mutable status               : Status.t
  } [@@deriving fields, sexp, bin_io]

let compare t1 t2 =
  Sexp.compare (sexp_of_t t1) (sexp_of_t t2)

let file t =
  match t.status with
  | `Frozen ->
    Some (Spool_directory.frozen_dir t.spool_dir ^/ Id.to_string t.id)
  | `Send_now | `Send_at _ | `Sending ->
    Some (Spool_directory.active_dir t.spool_dir ^/ Id.to_string t.id)
  | `Removed ->
    Some (Spool_directory.removed_dir t.spool_dir ^/ Id.to_string t.id)
  | `Quarantined _ ->
    Some (Spool_directory.quarantine_dir t.spool_dir ^/ Id.to_string t.id)
  | `Delivered -> None

let status t =
  match t.status with
  | `Send_at time when Time.(time < now ()) -> `Send_now
  | status -> status

module On_disk = struct
  type meta = t [@@deriving sexp, bin_io]

  type t =
    { mutable meta : meta
    ; mutable envelope : Envelope.t
    } [@@deriving fields, sexp, bin_io]

  let create = Fields.create

  let save t =
    begin
      match file t.meta with
      | Some file -> return (Ok (file, Filename.basename file))
      | None ->
        Deferred.Or_error.error
          "Trying to save state of a delivered message: %s"
          t sexp_of_t
    end
    >>=? fun (file, filename) ->
    Deferred.Or_error.try_with (fun () ->
      let tmp = Spool_directory.tmp_dir t.meta.spool_dir ^/ filename in
      Writer.save_sexp tmp (sexp_of_t t)
      >>= fun () ->
      Unix.rename ~src:tmp ~dst:file)
    >>= function
    | Ok () -> return (Ok ())
    | Error e ->
      return (Error e)

  let load path =
    Deferred.Or_error.try_with_join (fun () ->
      Reader.load_sexp path t_of_sexp)
    >>=? fun t ->
    let file = Option.value_exn (file t.meta) in
    if String.equal file path then return (Ok t)
    else let error = sprintf
                       "path mismatch: spooled message loaded from %s should be in %s"
                       path file
      in
      return (Or_error.error error t sexp_of_t)
end

let create spool_dir ~log:_ ~initial_status envelope_with_next_hop ~flows ~original_msg =
  let parent_id = Envelope.id original_msg in
  let id = Id.create ~envelope_with_next_hop ~original_msg in
  let next_hop_choices =
    Envelope_with_next_hop.next_hop_choices envelope_with_next_hop
  in
  let retry_intervals =
    Envelope_with_next_hop.retry_intervals envelope_with_next_hop
  in
  let envelope = Envelope_with_next_hop.envelope envelope_with_next_hop in
  let remaining_recipients = Envelope.recipients envelope in
  let t =
    Fields.create
      ~spool_dir
      ~spool_date:(Time.now ())
      ~remaining_recipients
      ~failed_recipients:[]
      ~next_hop_choices
      ~retry_intervals
      ~relay_attempts:[]
      ~parent_id
      ~status:initial_status
      ~id
      ~flows
  in
  let on_disk = On_disk.create ~meta:t ~envelope in
  On_disk.save on_disk
  >>|? fun () ->
  t

let load path =
  On_disk.load path
  >>|? fun on_disk ->
  on_disk.meta

let load_with_envelope path =
  On_disk.load path
  >>|? fun on_disk ->
  on_disk.meta, on_disk.envelope

let last_relay_attempt t =
  List.hd t.relay_attempts

let with_file t
      (f : Envelope.t -> [`Sync of Envelope.t | `Unlink] Or_error.t Deferred.t)
  : unit Or_error.t Deferred.t
  =
  Locks.lock t.id
  >>= fun () ->
  begin
    begin
      match file t with
      | Some file -> return (Ok file)
      | None ->
        Deferred.Or_error.error
          "Attempting file operation on a delivered message"
          t sexp_of_t
    end
    >>=? fun from_file ->
    On_disk.load from_file
    >>=? fun on_disk ->
    begin
      if compare t on_disk.meta = 0 then return (Ok ())
      else let e = Error.create
                     "spooled message in memory differs from spooled message on disk"
                     (`In_memory t, `On_disk on_disk.meta, `File from_file)
                     [%sexp_of: [`In_memory of t] * [`On_disk of t] * [`File of string]]
        in
        return (Error e)
    end
    >>=? fun () ->
    (* If the destination file differs from the source file (say, if the status
       of the message changed) then we need to unlink the old file after
       creating the new one. However, if the destination file is the same as the
       source file, we need to be careful not to unlink the new file. We dont't
       trust String.equal because different paths may refer to the same file. So
       instead we move the source file out of the way first and then delete it
       at the end. *)
    let from_file_bak = from_file ^ ".bak" in
    Common.rename ~src:from_file ~dst:from_file_bak
    >>=? fun () ->
    f on_disk.envelope
    >>=? fun action ->
    begin match action with
    | `Unlink ->
      Common.unlink from_file_bak
    | `Sync envelope'  ->
      on_disk.meta <- t;
      on_disk.envelope <- envelope';
      On_disk.save on_disk
      >>=? fun () ->
      Common.unlink from_file_bak
    end
    >>=? fun () ->
    return (Ok ())
  end
  >>= fun result ->
  Locks.release t.id;
  return result

let map_envelope t ~f =
  with_file t (fun envelope ->
    let envelope' = f envelope in
    return (Ok (`Sync envelope')))
;;

let freeze t ~log =
  with_file t (fun envelope ->
    Log.info log (lazy (Log.Message.create
                          ~here:[%here]
                          ~flows:t.flows
                          ~component:["spool"]
                          ~spool_id:t.id
                          "frozen"));
    t.status <- `Frozen;
    return (Ok (`Sync envelope)))
;;

let mark_for_send_now ~retry_intervals t ~log =
  with_file t (fun envelope ->
    Log.info log (lazy (Log.Message.create
                          ~here:[%here]
                          ~flows:t.flows
                          ~component:["spool"]
                          ~spool_id:t.id
                          "send_now"));
    t.status <- `Send_now;
    t.retry_intervals <- retry_intervals @ t.retry_intervals;
    return (Ok (`Sync envelope)))
;;

let remove t ~log =
  with_file t (fun envelope ->
    Log.info log (lazy (Log.Message.create
                          ~here:[%here]
                          ~flows:t.flows
                          ~component:["spool"]
                          ~spool_id:t.id
                          "removing"));
    t.status <- `Removed;
    return (Ok (`Sync envelope)))
;;

(* We are being conservative here for simplicity - if we get a permanent error
   from one hop, we assume that we would get the same error from the remaining
   hops. *)
let send_to_hops t ~log ~config envelope =
  let rec send_to_hops = function
    | [] ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows:t.flows
                            ~component:["spool"]
                            ~spool_id:t.id
                            "all-next-hops-failed"));
      return `Try_later
    | hop :: untried_next_hops ->
      let envelope = Envelope.set envelope ~recipients:t.remaining_recipients () in
      Log.debug log (lazy (Log.Message.create
                             ~here:[%here]
                             ~flows:t.flows
                             ~component:["spool";"send"]
                             ~spool_id:t.id
                             ~remote_address:hop
                             "attempting delivery"));
      Client.Tcp.with_ ~config hop
        ~log
        ~component:["spool";"send"]
        ~f:(fun client ->
          (* Not passing flows along as the Client session already includes the flows. *)
          Client.send_envelope client ~log ~flows:t.flows ~component:["spool";"send"] envelope)
      >>= function
      | Error e ->
        (* The client logs many common failures, so this might be repetitive. But
           duplication in the error case is better than missing potential errors. *)
        let e = Error.tag ~tag:"Unable to send envelope" e in
        Log.info log (lazy (Log.Message.of_error
                              ~here:[%here]
                              ~flows:t.flows
                              ~component:["spool"; "send"]
                              ~spool_id:t.id
                              ~remote_address:hop
                              e));
        t.relay_attempts <- (Time.now (), e) :: t.relay_attempts;
        send_to_hops untried_next_hops
      | Ok envelope_status ->
        match Client.Envelope_status.ok_or_error ~allow_rejected_recipients:false envelope_status with
        | Ok _msg_id ->
          (* Already logged by the client *)
          return `Done
        | Error e ->
          (* Already logged by the client *)
          t.relay_attempts <- (Time.now (), e) :: t.relay_attempts;
          match envelope_status with
          | Ok (_ (* envelope_id *), rejected_recipients)
          | Error (`No_recipients rejected_recipients) ->
            let permanently_failed_recipients, temporarily_failed_recipients =
              List.partition_map rejected_recipients ~f:(fun (recipient, reject) ->
                if Reply.is_permanent_error reject then `Fst recipient
                else `Snd recipient)
            in
            t.remaining_recipients <- temporarily_failed_recipients;
            t.failed_recipients <- t.failed_recipients @ permanently_failed_recipients;
            if List.is_empty t.remaining_recipients
            then return `Fail_permanently
            else send_to_hops untried_next_hops
          | Error (`Rejected_sender r
                  | `Rejected_sender_and_recipients (r,_)
                  | `Rejected_body (r,_)) ->
            if Reply.is_permanent_error r
            then return `Fail_permanently
            else send_to_hops untried_next_hops
  in
  send_to_hops t.next_hop_choices

let do_send t ~log ~config =
  with_file t (fun envelope ->
    t.status <- `Sending;
    send_to_hops t ~log ~config envelope
    >>| function
    | `Done ->
      t.status <- `Delivered;
      Ok `Unlink
    | (`Fail_permanently | `Try_later) as fail ->
      match fail, t.retry_intervals with
      | `Fail_permanently, _ | _, [] ->
        t.status <- `Frozen;
        Ok (`Sync envelope)
      | `Try_later, r :: rs ->
        t.status <- `Send_at (Time.add (Time.now ()) r);
        t.retry_intervals <- rs;
        Ok (`Sync envelope))
;;

let send t ~log ~config =
  match t.status with
  | `Send_now | `Send_at _ -> do_send t ~log ~config
  | `Frozen ->
    return (Or_error.error_string
              "Spooled_message.send: message is frozen")
  | `Removed ->
    return (Or_error.error_string
              "Spooled_message.send: message is removed")
  | `Quarantined _ ->
    return (Or_error.error_string
              "Spooled_message.send: message is quarantined")
  | `Sending ->
    return (Or_error.error_string
              "Spooled_message.send: message is already being sent")
  | `Delivered ->
    return (Or_error.error_string
              "Spooled_message.send: message is delivered")

let size_of_file t =
  match file t with
  | None ->
    Deferred.Or_error.error
      "Spooled_message.size_of_file: message already delivered."
      t sexp_of_t
  | Some file ->
    Deferred.Or_error.try_with_join (fun () ->
      Unix.stat file
      >>= fun stats ->
      let size = Unix.Stats.size stats |> Float.of_int64 in
      return (Ok (Byte_units.create `Bytes size)))
;;

let time_on_spool t =
  Time.diff (Time.now ()) (spool_date t)

module T = struct
  type nonrec t = t [@@deriving sexp]

  let compare t1 t2 =
    Id.compare t1.id t2.id

  let hash t =
    Id.hash t.id
end

include Comparable.Make(T)
include Hashable.Make(T)
