open Core
open Poly
open! Async
open Async_smtp
module Time = Time_float_unix
module Message = Smtp_mail_log.Message
module Flows = Smtp_mail_log.Flows

let get_flow_id ~type_ message =
  List.find (Message.flows message :> Flows.Id.t list) ~f:(fun id -> Flows.Id.is id type_)
;;

let value_exn x =
  match x with
  | None -> failwith "invalid log entry"
  | Some x -> x
;;

module Outbound_envelope = struct
  type t =
    { id : Flows.Id.t
    ; mutable recipients : string list
    ; mutable events : Smtp_mail_log.Message.t list
    }
  [@@deriving fields ~getters, sexp_of]

  let create id = { id; recipients = []; events = [] }
  let add_event t event = t.events <- event :: t.events
  let recipients t = t.recipients
end

module Inbound_envelope = struct
  type t =
    { id : Flows.Id.t
    ; mutable mail_from : string option [@sexp.option]
    ; mutable rcpt_to : string list
    ; mutable data : Smtp_mail_log.Mail_fingerprint.t option [@sexp.option]
    ; mutable events : Smtp_mail_log.Message.t list
    ; mutable outbound_envelopes : Outbound_envelope.t list
    }
  [@@deriving fields ~getters, sexp_of]

  let create id =
    { id
    ; mail_from = None
    ; rcpt_to = []
    ; data = None
    ; outbound_envelopes = []
    ; events = []
    }
  ;;

  let add_event t event = t.events <- event :: t.events

  let find_outbound_envelope t id =
    List.find t.outbound_envelopes ~f:(fun out -> out.Outbound_envelope.id = id)
  ;;

  let envelope_sender t = t.mail_from
  let envelope_recipients t = t.rcpt_to
  let data t = t.data
  let outbound_envelopes t = t.outbound_envelopes

  let get_header_value t header =
    match t.data with
    | None -> []
    | Some data ->
      let headers =
        Email_headers.of_list
          ~normalize:`Whitespace
          (Smtp_mail_log.Mail_fingerprint.headers data)
      in
      Email_headers.find_all headers header
  ;;
end

type t =
  { id : Flows.Id.t
  ; mutable session_connect : Time.t option [@sexp.option]
  ; mutable events : Smtp_mail_log.Message.t list
  ; mutable inbound_envelopes : Inbound_envelope.t list
  ; mutable raw_messages : Message.t list [@sexp.list]
  }
[@@deriving sexp_of]

let compare s1 s2 =
  match s1.session_connect, s2.session_connect with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some t1, Some t2 -> compare t1 t2
;;

let sexp_of_t t =
  let events = List.rev t.events in
  let inbound_envelopes =
    List.map t.inbound_envelopes ~f:(fun in_ ->
      let events = List.rev in_.Inbound_envelope.events in
      let outbound_envelopes =
        List.map in_.Inbound_envelope.outbound_envelopes ~f:(fun out ->
          let events = List.rev out.Outbound_envelope.events in
          { out with Outbound_envelope.events })
        |> List.rev
      in
      { in_ with Inbound_envelope.events; outbound_envelopes })
    |> List.rev
  in
  let t' = { t with events; inbound_envelopes; raw_messages = [] } in
  sexp_of_t t'
;;

let create id =
  { id; session_connect = None; events = []; inbound_envelopes = []; raw_messages = [] }
;;

let find_inbound_envelope t id =
  List.find t.inbound_envelopes ~f:(fun in_ -> in_.Inbound_envelope.id = id)
;;

let add_event t message =
  let inbound_id = get_flow_id ~type_:`Inbound_envelope message in
  let outbound_id = get_flow_id ~type_:`Outbound_envelope message in
  match inbound_id, outbound_id with
  | None, None -> t.events <- message :: t.events
  | Some inbound_id, None ->
    Option.iter (find_inbound_envelope t inbound_id) ~f:(fun in_ ->
      Inbound_envelope.add_event in_ message)
  | Some inbound_id, Some outbound_id ->
    Option.iter (find_inbound_envelope t inbound_id) ~f:(fun in_ ->
      Option.iter (Inbound_envelope.find_outbound_envelope in_ outbound_id) ~f:(fun out ->
        Outbound_envelope.add_event out message))
  | None, Some _ -> failwiths "invalid log entry" message Message.sexp_of_t
;;

let set_connected t message = t.session_connect <- Some (Message.time message)

let create_inbound_envelope t message =
  let id = get_flow_id ~type_:`Inbound_envelope message |> value_exn in
  let inbound_envelope = Inbound_envelope.create id in
  let sender =
    Message.sender message
    |> Option.map ~f:(function
      | `String str -> str
      | `Sender sender -> Smtp_envelope.Sender.to_string sender)
  in
  inbound_envelope.Inbound_envelope.mail_from <- sender;
  t.inbound_envelopes <- inbound_envelope :: t.inbound_envelopes
;;

let update_inbound_envelope t message ~which =
  let inbound_id = get_flow_id ~type_:`Inbound_envelope message |> value_exn in
  let inbound_envelope = find_inbound_envelope t inbound_id in
  Option.iter inbound_envelope ~f:(fun in_ ->
    match which with
    | `Rcpt_to ->
      let recipients =
        Message.recipients message
        |> Option.map
             ~f:
               (List.map ~f:(function
                 | `String str -> str
                 | `Email addr -> Email_address.to_string addr))
      in
      Option.iter recipients ~f:(fun recipients ->
        in_.Inbound_envelope.rcpt_to <- recipients @ in_.Inbound_envelope.rcpt_to)
    | `Data ->
      let email = Message.email message in
      Option.iter email ~f:(fun email -> in_.Inbound_envelope.data <- Some email))
;;

let create_outbound_envelope t message =
  let inbound_id = get_flow_id ~type_:`Inbound_envelope message |> value_exn in
  let inbound_envelope = find_inbound_envelope t inbound_id in
  let outbound_id = get_flow_id ~type_:`Outbound_envelope message |> value_exn in
  Option.iter inbound_envelope ~f:(fun in_ ->
    let out = Outbound_envelope.create outbound_id in
    in_.Inbound_envelope.outbound_envelopes
    <- out :: in_.Inbound_envelope.outbound_envelopes;
    let recipients =
      Smtp_mail_log.Message.recipients message
      |> Option.value_map
           ~default:[]
           ~f:
             (List.map ~f:(function
               | `String str -> str
               | `Email addr -> Email_address.to_string addr))
    in
    out.Outbound_envelope.recipients <- recipients)
;;

let inbound_envelopes t = t.inbound_envelopes
let session_connect t = t.session_connect

let update session message =
  session.raw_messages <- message :: session.raw_messages;
  match Message.session_marker message with
  | None -> add_event session message
  | Some marker ->
    (match marker with
     | `Connected ->
       add_event session message;
       set_connected session message
     | `Mail_from -> create_inbound_envelope session message
     | `Rcpt_to -> update_inbound_envelope session message ~which:`Rcpt_to
     | `Data -> update_inbound_envelope session message ~which:`Data
     | `Sending -> create_outbound_envelope session message)
;;

let raw_messages t = List.rev t.raw_messages

module Summary = struct
  type email =
    { subject : string list [@sexp.list]
    ; from : string list [@sexp.list]
    ; to_ : string list [@sexp.list]
    ; cc : string list [@sexp.list]
    ; rfc822_id : string list [@sexp.list]
    ; flow : Smtp_mail_log.Flows.Id.t
    ; recipients : (string list * Smtp_mail_log.Flows.Id.t) list [@sexp.list]
    }
  [@@deriving sexp_of]

  type t =
    { connect_time : Time.t option [@sexp.option]
    ; emails : email list
    ; session_flow : Smtp_mail_log.Flows.Id.t
    }
  [@@deriving sexp_of]
end

let summary t =
  let connect_time = t.session_connect in
  let emails =
    List.map t.inbound_envelopes ~f:(fun inb ->
      { Summary.subject = Inbound_envelope.get_header_value inb "Subject"
      ; from = Inbound_envelope.get_header_value inb "From"
      ; to_ = Inbound_envelope.get_header_value inb "To"
      ; cc = Inbound_envelope.get_header_value inb "Cc"
      ; rfc822_id = Inbound_envelope.get_header_value inb "Message-Id"
      ; flow = Inbound_envelope.id inb
      ; recipients =
          List.map (Inbound_envelope.outbound_envelopes inb) ~f:(fun outb ->
            Outbound_envelope.recipients outb, Outbound_envelope.id outb)
      })
  in
  { Summary.connect_time; emails; session_flow = t.id }
;;
