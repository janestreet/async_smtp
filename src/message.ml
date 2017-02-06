open Core
open Core_extended.Std
open Async
open Types

module Log = Mail_log

let compare _ _ = `You_are_using_poly_compare
let _silence_unused_warning = compare

(* Includes parent id and an incrementing counter. *)
module Id  = struct
  include String

  let counter = ref 0

  let create ~original_msg =
    let parent_id = Envelope.id original_msg in
    let t =
      sprintf !"%{Envelope.Id}-%s" parent_id
        (Types.urlbase64_encode_float ~length:6 (!counter |> Int.to_float))
    in
    incr counter;
    t
  ;;
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

module Queue = struct
  type t =
    | Active
    | Frozen
    | Removed
    | Quarantine
  [@@deriving sexp, enumerate, compare, bin_io]

  let to_dirname = function
    | Active     -> "active"
    | Frozen     -> "frozen"
    | Removed    -> "removed"
    | Quarantine -> "quarantine"
  ;;

  let of_status status =
    match status with
    | `Frozen                           -> Some Frozen
    | `Send_now | `Send_at _ | `Sending -> Some Active
    | `Removed                          -> Some Removed
    | `Quarantined _                    -> Some Quarantine
    | `Delivered                        -> None
  ;;

  let of_status' status =
    match of_status status with
    | Some queue  -> Ok queue
    | None        ->
      Or_error.error_s
        [%message "Specified status not associated with a queue"
                    (status : Status.t)]
  ;;
end

(* A value of type t should only be modified via [On_disk_spool].  This guarantees
   that all changes are properly flushed to disk. *)
type t =
  { spool_dir                    : string
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
     deliveries. Addresses on this list will not be included in remaining_recipients,
     and would otherwise be lost to the ether. *)
  ; mutable failed_recipients    : Email_address.t list
  ; mutable relay_attempts       : (Time.t * Error.t) list
  ; mutable status               : Status.t
  } [@@deriving fields, sexp, bin_io]

type meta = t [@@deriving sexp]


let compare t1 t2 =
  Sexp.compare (sexp_of_t t1) (sexp_of_t t2)
;;

let status t =
  match t.status with
  | `Send_at time when Time.(time < now ()) -> `Send_now
  | status -> status
;;

module On_disk = struct
  module T = struct
    type t =
      { mutable meta : meta
      ; mutable envelope : Envelope.t
      } [@@deriving fields, sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  module Queue = Queue

  module Name_generator = struct
    type t = Envelope.t
    let next original_msg ~attempt:_ =
      Id.create ~original_msg
    ;;
  end

  let create = Fields.create
end

module On_disk_spool = struct
  include Multispool.Make(On_disk)

  (* Hide optional argument from the interface *)
  let load str = load str

  let ls t queues =
    Deferred.Or_error.List.concat_map queues ~f:(fun queue -> list t queue)
  ;;
end

let entry t =
  let open Deferred.Or_error.Let_syntax in
  let open On_disk_spool in
  let spool = load_unsafe t.spool_dir in
  let%map queue = Queue.of_status' t.status |> Deferred.return in
  Entry.create spool queue ~name:t.id
;;

let create spool ~log:_ ~initial_status envelope_with_next_hop ~flows ~original_msg =
  let parent_id = Envelope.id original_msg in
  let next_hop_choices =
    Envelope_with_next_hop.next_hop_choices envelope_with_next_hop
  in
  let retry_intervals =
    Envelope_with_next_hop.retry_intervals envelope_with_next_hop
  in
  let envelope = Envelope_with_next_hop.envelope envelope_with_next_hop in
  let remaining_recipients = Envelope.recipients envelope in
  Queue.of_status' initial_status |> Deferred.return
  >>=? fun queue ->
  On_disk_spool.Unique_name.reserve spool original_msg
  >>=? fun id ->
  let t =
    Fields.create
      ~spool_dir:(On_disk_spool.dir spool)
      ~spool_date:(Time.now ())
      ~remaining_recipients
      ~failed_recipients:[]
      ~next_hop_choices
      ~retry_intervals
      ~relay_attempts:[]
      ~parent_id
      ~status:initial_status
      ~id:(id :> string)
      ~flows
  in
  let on_disk = On_disk.create ~meta:t ~envelope in
  On_disk_spool.enqueue spool queue on_disk (`Use id)
  >>=? fun (_ : On_disk_spool.Entry.t) ->
  Deferred.Or_error.return t
;;

let load entry =
  let open Deferred.Or_error.Let_syntax in
  let%map on_disk = On_disk_spool.Entry.contents_unsafe entry in
  on_disk.meta
;;

let load_with_envelope entry =
  let open Deferred.Or_error.Let_syntax in
  let%map on_disk = On_disk_spool.Entry.contents_unsafe entry in
  on_disk.meta, on_disk.envelope
;;

let last_relay_attempt t =
  List.hd t.relay_attempts
;;

let with_file t
      (f : Envelope.t -> [`Sync of Envelope.t | `Unlink] Or_error.t Deferred.t)
  : unit Or_error.t Deferred.t =
  entry t
  >>=? fun entry ->
  return (Queue.of_status' t.status)
  >>=? fun original_queue ->
  On_disk_spool.with_entry entry
    ~f:(fun on_disk ->
      match compare t on_disk.meta = 0 with
      | false ->
        let e = Error.create
                  "spooled message in memory differs from spooled message on disk"
                  (`In_memory t, `On_disk on_disk.meta, `Entry entry)
                  [%sexp_of: [`In_memory of t] * [`On_disk of t] * [`Entry of On_disk_spool.Entry.t]]
        in
        return (`Save (on_disk, original_queue), Error e)
      | true ->
        f on_disk.envelope
        >>= function
        | Error _ as e -> return (`Save (on_disk, original_queue), e)
        | Ok (`Unlink) -> return (`Remove, Ok ())
        | Ok (`Sync envelope) ->
          (* Derive queue from mutable [t.status] as it may have changed in [~f] *)
          Queue.of_status' t.status
          |> Or_error.tag ~tag:(Sexp.to_string (sexp_of_t t))
          |> Deferred.return
          >>= function
          | Error _ as e -> return (`Save (on_disk, original_queue), e)
          | Ok new_queue ->
            on_disk.meta <- t;
            on_disk.envelope <- envelope;
            return (`Save (on_disk, new_queue), Ok ())
    )
  >>| Or_error.join
;;

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
;;

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
  let%map stats = On_disk_spool.Entry.stat entry in
  let size = Unix.Stats.size stats |> Float.of_int64 in
  Byte_units.create `Bytes size
;;

let time_on_spool t =
  Time.diff (Time.now ()) t.spool_date
;;

module T = struct
  type nonrec t = t [@@deriving sexp]

  let compare t1 t2 =
    Id.compare t1.id t2.id

  let hash t =
    Id.hash t.id
end

include Comparable.Make(T)
include Hashable.Make(T)
