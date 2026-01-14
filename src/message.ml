module Stable = struct
  open Core.Core_stable
  open Email_message.Email_message_stable
  open Async_smtp_types.Async_smtp_types_stable
  module Time = Time_float_unix.Stable
  module Unstable_mail_log = Mail_log
  module Mail_log = Mail_log.Stable
  module Retry_interval = Smtp_envelope.Retry_interval
  module Quarantine_reason = Quarantine_reason.Stable

  module Id = struct
    module V1 = struct
      include String.V1

      let to_string t = t
      let of_string t = t

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
      ;;
    end
  end

  module Status = struct
    module V1 = struct
      type t =
        [ `Send_now
        | `Send_at of Time.V1.t
        | `Sending
        | `Frozen
        | `Removed
        | `Quarantined of Quarantine_reason.V1.t
        | `Delivered
        ]
      [@@deriving sexp, bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 424465fabd3656a7dfa206491ab934af |}]
      ;;
    end
  end

  module V1 = struct
    type t =
      { spool_dir : string
      ; id : Id.V1.t
      ; flows : Mail_log.Flows.V1.t [@default Unstable_mail_log.Flows.none]
      ; parent_id : Smtp_envelope.Id.V1.t
      ; spool_date : Time.V1.t
      ; next_hop_choices : [ `Inet of Host_and_port.V1.t ] list
      ; mutable retry_intervals : Retry_interval.V2.t list
      ; mutable remaining_recipients : Email_address.V1.t list
      ; mutable failed_recipients : Email_address.V1.t list
      ; mutable relay_attempts : (Time.V1.t * Error.V1.t) list
      ; mutable status : Status.V1.t
      ; mutable envelope_info : Smtp_envelope.Info.V1.t
      }
    [@@deriving sexp, bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 49b13cef6568275307ed409f67857b25 |}]
    ;;
  end

  module V2 = struct
    type t =
      { spool_dir : string
      ; id : Id.V1.t
      ; flows : Mail_log.Flows.V1.t [@default Unstable_mail_log.Flows.none]
      ; parent_id : Smtp_envelope.Id.V1.t
      ; spool_date : Time.V1.t
      ; next_hop_choices : [ `Inet of Host_and_port.V1.t ] list
      ; mutable retry_intervals : Retry_interval.V2.t list
      ; mutable remaining_recipients : Email_address.V1.t list
      ; mutable failed_recipients : Email_address.V1.t list
      ; mutable relay_attempts : (Time.V1.t * Error.V1.t) list
      ; mutable status : Status.V1.t
      ; mutable envelope_info : Smtp_envelope.Info.V2.t
      }
    [@@deriving sexp, bin_io, stable_record ~version:V1.t ~modify:[ envelope_info ]]

    let of_v1 = of_V1_t ~modify_envelope_info:Smtp_envelope.Info.V2.of_v1

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 586728abcc44ce512a1d7ef9abb4f35b |}]
    ;;
  end

  module V3 = struct
    type t =
      { spool_dir : string
      ; id : Id.V1.t
      ; flows : Mail_log.Flows.V1.t [@default Unstable_mail_log.Flows.none]
      ; parent_id : Smtp_envelope.Id.V1.t
      ; spool_date : Time.V1.t
      ; next_hop_choices : Host_and_port.V1.t list
      ; mutable retry_intervals : Retry_interval.V2.t list
      ; mutable remaining_recipients : Email_address.V1.t list
      ; mutable failed_recipients : Email_address.V1.t list
      ; mutable relay_attempts : (Time.V1.t * Error.V1.t) list
      ; mutable status : Status.V1.t
      ; mutable envelope_info : Smtp_envelope.Info.V2.t
      }
    [@@deriving sexp, bin_io, stable_record ~version:V2.t ~modify:[ next_hop_choices ]]

    let of_v2 =
      of_V2_t ~modify_next_hop_choices:(fun next_hop_choices ->
        Core.List.map next_hop_choices ~f:(fun (`Inet i) -> i))
    ;;

    let of_v1 v1 = of_v2 (V2.of_v1 v1)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 7c91581c5678eddbae053a4a79d731a0 |}]
    ;;
  end

  module V4 = struct
    type t =
      { spool_dir : string
      ; id : Id.V1.t
      ; flows : Mail_log.Flows.V1.t [@default Unstable_mail_log.Flows.none]
      ; parent_id : Smtp_envelope.Id.V1.t
      ; related_ids : Id.V1.t list
      ; spool_date : Time.V1.t
      ; next_hop_choices : Host_and_port.V1.t list
      ; mutable retry_intervals : Retry_interval.V2.t list
      ; mutable remaining_recipients : Email_address.V1.t list
      ; mutable failed_recipients : Email_address.V1.t list
      ; mutable relay_attempts : (Time.V1.t * Error.V1.t) list
      ; mutable status : Status.V1.t
      ; mutable envelope_info : Smtp_envelope.Info.V2.t
      }
    [@@deriving sexp, bin_io, stable_record ~version:V3.t ~remove:[ related_ids ]]

    let to_v3 = to_V3_t
    let of_v3 = of_V3_t ~related_ids:[]
    let of_v2 v2 = of_v3 (V3.of_v2 v2)
    let of_v1 v1 = of_v3 (V3.of_v1 v1)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 6a29555ee0a86522df7d11a2a07191ad |}]
    ;;
  end
end

open Core
open Async
open Async_smtp_types
module Time = Time_float_unix

(* Includes parent id and an incrementing counter. *)
module Id = struct
  include String

  let counter = ref 0

  let create ~original_msg =
    let parent_id = Smtp_envelope.id original_msg in
    let t =
      sprintf
        !"%{Smtp_envelope.Id}-%s"
        parent_id
        (Smtp_envelope.Id.urlbase64_encode_float ~length:6 (!counter |> Int.to_float)
         |> Smtp_envelope.Id.to_string)
    in
    incr counter;
    t
  ;;
end

module Status = Stable.Status.V1

module Queue = struct
  type t =
    | Active
    | Frozen
    | Removed
    | Quarantine
  [@@deriving sexp, enumerate, compare]

  let to_string = function
    | Active -> "active"
    | Frozen -> "frozen"
    | Removed -> "removed"
    | Quarantine -> "quarantine"
  ;;

  let to_dirname = to_string

  let of_status status =
    match status with
    | `Frozen -> Some Frozen
    | `Send_now | `Send_at _ | `Sending -> Some Active
    | `Removed -> Some Removed
    | `Quarantined _ -> Some Quarantine
    | `Delivered -> None
  ;;

  let of_status' status =
    match of_status status with
    | Some queue -> Ok queue
    | None ->
      Or_error.error_s
        [%message "Specified status not associated with a queue" (status : Status.t)]
  ;;
end

module Data = struct
  type t = Email.t

  let map_headers headers ~encode_or_decode =
    let f =
      match encode_or_decode with
      | `Encode -> fun s -> Dot_escaping.encode_line_string s |> String_monoid.to_string
      | `Decode -> Dot_escaping.decode_line_string
    in
    Email_headers.map' ~normalize:`None headers ~f:(fun ~name ~value -> f name, value)
  ;;

  let map_raw_content_bstr body ~encode_or_decode =
    let eol, f =
      match encode_or_decode with
      | `Encode -> "\r\n", Dot_escaping.encode_line_bigstring
      | `Decode ->
        "\n", fun s -> Dot_escaping.decode_line_bigstring s |> String_monoid.of_bigstring
    in
    (* Most likely, the output buffer will be the same length as the input buffer. Give
       ourselves some leeway to avoid having to resize. *)
    let buffer = Bigbuffer.create (Bigstring_shared.length body + 100) in
    let add_transformed_line line =
      String_monoid.output_bigbuffer (f (Bigstring_shared.to_bigstring line)) buffer
    in
    let rec loop seq =
      match Sequence.hd seq with
      | None -> ()
      | Some line ->
        add_transformed_line line;
        (match Sequence.tl seq with
         | None -> ()
         | Some tail ->
           (* Peek the sequence so we don't add an eol marker for the last line. *)
           if Option.is_some (Sequence.hd tail) then Bigbuffer.add_string buffer eol;
           loop tail)
    in
    loop (Bigstring_shared.lines_seq ~include_empty_last_line:() body);
    Bigstring_shared.of_bigbuffer_volatile buffer
  ;;

  let map_raw_content raw_content ~encode_or_decode =
    Option.map
      (Email.Raw_content.Expert.to_bigstring_shared_option raw_content)
      ~f:(map_raw_content_bstr ~encode_or_decode)
    |> Email.Raw_content.Expert.of_bigstring_shared_option
  ;;

  let map_email t ~encode_or_decode =
    Email.create
      ~headers:(map_headers (Email.headers t) ~encode_or_decode)
      ~raw_content:(map_raw_content (Email.raw_content t) ~encode_or_decode)
  ;;

  let to_email = map_email ~encode_or_decode:`Decode
  let of_email = map_email ~encode_or_decode:`Encode

  let load path =
    Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
      let%bind contents = Reader.file_contents path in
      return (Email.of_string contents))
  ;;

  let save ?temp_file t path =
    Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
      Email.save ?temp_file ~fsync:true ~eol_except_raw_content:`CRLF t path)
  ;;
end

(* A value of type t should only be modified via [On_disk_spool]. This guarantees that all
   changes are properly flushed to disk. *)
type t = Stable.V4.t =
  { spool_dir : string
  ; id : Id.t
  ; flows : Mail_log.Flows.t
  ; parent_id : Smtp_envelope.Id.t
  ; related_ids : Id.t list
  ; spool_date : Time.t
  ; next_hop_choices : Host_and_port.t list
  ; mutable retry_intervals : Smtp_envelope.Retry_interval.t list
  ; mutable remaining_recipients : Email_address.Stable.V1.t list
  ; mutable failed_recipients : Email_address.Stable.V1.t list
  ; mutable relay_attempts : (Time.t * Error.t) list
  ; mutable status : Status.t
  ; mutable envelope_info : Smtp_envelope.Info.t
  }
[@@deriving fields ~getters, sexp_of]

(* type alias to make code more readable below *)
type meta = t [@@deriving sexp_of]

let compare t1 t2 = Sexp.compare (sexp_of_t t1) (sexp_of_t t2)

let status t =
  match t.status with
  | `Send_at time when Time.(time < now ()) -> `Send_now
  | status -> status
;;

let time_on_spool t = Time.diff (Time.now ()) t.spool_date
let last_relay_attempt t = List.hd t.relay_attempts
let set_status t x = t.status <- x
let set_remaining_recipients t x = t.remaining_recipients <- x
let set_failed_recipients t x = t.failed_recipients <- x
let set_retry_intervals t x = t.retry_intervals <- x
let add_retry_intervals t x = t.retry_intervals <- x @ t.retry_intervals
let add_relay_attempt t x = t.relay_attempts <- x :: t.relay_attempts

let move_failed_recipients_to_remaining_recipients t =
  t.remaining_recipients <- t.remaining_recipients @ t.failed_recipients;
  t.failed_recipients <- []
;;

let of_envelope_batch
  envelope_batch
  ~gen_id
  ~spool_dir
  ~spool_date
  ~failed_recipients
  ~relay_attempts
  ~parent_id
  ~set_related_ids
  ~status
  ~flows
  =
  let open Deferred.Or_error.Let_syntax in
  let email_body = Smtp_envelope.Routed.Batch.email_body envelope_batch in
  (* We make sure to only map the email body once. *)
  let data_raw_content = Data.map_raw_content email_body ~encode_or_decode:`Encode in
  let%map envelopes_with_message_ids =
    Deferred.Or_error.List.map
      ~how:`Sequential
      (Smtp_envelope.Routed.Batch.envelopes envelope_batch)
      ~f:(fun envelope ->
        let%map id = gen_id () in
        id, envelope)
  in
  let messages_in_batch = List.map envelopes_with_message_ids ~f:Tuple2.get1 in
  List.map envelopes_with_message_ids ~f:(fun (id, envelope) ->
    let headers =
      Smtp_envelope.Bodiless.Routed.headers envelope
      |> Data.map_headers ~encode_or_decode:`Encode
    in
    let envelope_info = Smtp_envelope.Bodiless.Routed.envelope_info envelope in
    let data = Email.create ~headers ~raw_content:data_raw_content in
    let next_hop_choices = Smtp_envelope.Bodiless.Routed.next_hop_choices envelope in
    let retry_intervals = Smtp_envelope.Bodiless.Routed.retry_intervals envelope in
    let remaining_recipients = Smtp_envelope.Bodiless.Routed.recipients envelope in
    let related_ids = if set_related_ids then messages_in_batch else [] in
    let message =
      { spool_dir
      ; id
      ; flows
      ; parent_id
      ; related_ids
      ; spool_date
      ; next_hop_choices
      ; retry_intervals
      ; remaining_recipients
      ; failed_recipients
      ; relay_attempts
      ; status
      ; envelope_info
      }
    in
    message, data, Smtp_envelope.Routed.of_bodiless envelope email_body)
;;

module On_disk = struct
  module Metadata = struct
    module T = struct
      include Stable.V4

      let t_of_sexp sexp =
        try t_of_sexp sexp with
        | error_from_v4 ->
          (try Stable.V3.t_of_sexp sexp |> Stable.V4.of_v3 with
           | error_from_v3 ->
             (try Stable.V2.t_of_sexp sexp |> Stable.V4.of_v2 with
              | error_from_v2 ->
                (try Stable.V1.t_of_sexp sexp |> Stable.V4.of_v1 with
                 | error_from_v1 ->
                   raise_s
                     [%message
                       "[On_disk.Metadata.t_of_sexp]"
                         (error_from_v4 : exn)
                         (error_from_v3 : exn)
                         (error_from_v2 : exn)
                         (error_from_v1 : exn)])))
      ;;
    end

    include T
    include Sexpable.To_stringable (T)
  end

  module Data = Data
  module Queue = Queue

  module Name_generator = struct
    module Unique_name = Id

    type t = Smtp_envelope.t

    let next original_msg ~attempt:_ = Id.create ~original_msg
  end

  module Throttle = struct
    (* Don't hit the max open files system limit *)
    let t = Throttle.create ~continue_on_error:true ~max_concurrent_jobs:400
    let enqueue f = Throttle.enqueue t f
  end
end

module On_disk_spool = Multispool.Make (On_disk)
