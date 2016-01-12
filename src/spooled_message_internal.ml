open Core.Std
open Async.Std
open Async_extended.Std
open Types

module Mutex = Async_mutex

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
        (Base64.encode_float ~length:6 (!counter |> Int.to_float))
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
  ; parent_id                    : Envelope.Id.t
  ; spool_date                   : Time.t
  ; next_hop_choices             : Host_and_port.t list
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
    ; envelope : Envelope.t
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
      Log.Global.debug "failed to sync %s: %s" t.meta.id (Error.to_string_hum e);
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

let create spool_dir ~initial_status envelope_with_next_hop ~original_msg =
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
    (f : Envelope.t -> [`Sync | `Unlink] Or_error.t Deferred.t)
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
    | `Sync   ->
      on_disk.meta <- t;
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

let freeze t =
  with_file t (fun _envelope ->
    Log.Global.info !"freezing %{Id}" (id t);
    t.status <- `Frozen;
    return (Ok `Sync))
;;

let mark_for_send_now ~retry_intervals t =
  with_file t (fun _envelope ->
    Log.Global.info !"changing status to send_now for %{Id}" (id t);
    t.status <- `Send_now;
    t.retry_intervals <- retry_intervals @ t.retry_intervals;
    return (Ok `Sync))
;;

let remove t =
  with_file t (fun _envelope ->
    Log.Global.info !"deleting %{Id}" (id t);
    t.status <- `Removed;
    return (Ok `Sync))
;;

(* We are being conservative here for simplicity - if we get a permanent error
   from one hop, we assume that we would get the same error from the remaining
   hops. *)
let send_to_hops t ~config envelope =
  let rec send_to_hops = function
    | [] ->
      Log.Global.info !"all next hops failed for %{Envelope.Id}"
        (Envelope.id envelope);
      return `Try_later
    | hop :: untried_next_hops ->
      let error e =
        Log.Global.info
          "failed to send %s to destination %s: %s"
          t.id (Host_and_port.to_string hop)
          (Error.to_string_hum e);
        t.relay_attempts <- (Time.now (), e) :: t.relay_attempts
      in
      let fail_permanently () =
        Log.Global.error "Got permanent error, freezing the message";
        return `Fail_permanently
      in
      Log.Global.debug "trying to send message %s to %s"
        t.id (Host_and_port.to_string hop);
      let envelope = Envelope.set envelope ~recipients:t.remaining_recipients () in
      Client.Tcp.with_ ~config hop ~f:(fun client ->
          Client.send_envelope client envelope)
      >>= function
      | Error e ->
        error e;
        send_to_hops untried_next_hops
      | Ok envelope_status ->
        match Client.Envelope_status.ok_or_error ~allow_rejected_recipients:false envelope_status with
        | Ok _msg_id ->
          return `Done
        | Error e ->
          error e;
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
            then fail_permanently ()
            else send_to_hops untried_next_hops
          | Error (`Rejected_sender r
                  | `Rejected_sender_and_recipients (r,_)
                  | `Rejected_body (r,_)) ->
            if Reply.is_permanent_error r then fail_permanently ()
            else send_to_hops untried_next_hops
  in
  send_to_hops t.next_hop_choices

let do_send t ~config =
  with_file t (fun envelope ->
    t.status <- `Sending;
    send_to_hops t ~config envelope
    >>| function
    | `Done ->
      t.status <- `Delivered;
      Ok `Unlink
    | (`Fail_permanently | `Try_later) as fail ->
      match fail, t.retry_intervals with
      | `Fail_permanently, _ | _, [] ->
        t.status <- `Frozen;
        Ok `Sync
      | `Try_later, r :: rs ->
        t.status <- `Send_at (Time.add (Time.now ()) r);
        t.retry_intervals <- rs;
        Ok `Sync)
;;

let send t ~config =
  match t.status with
  | `Send_now | `Send_at _ -> do_send t ~config
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
    (* Not returning an error here because calling [Spool.send] will enqueue the
       message without changing what was previously queued, so we will attempt
       to deliver twice. *)
    return (Ok ())

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
