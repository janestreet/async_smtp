module Stable = struct
  open Core.Core_stable
  module Sender = Sender.Stable
  module Sender_argument = Sender_argument.Stable

  module V1 = struct
    type t =
      { sender : Sender.V1.t
      ; sender_args : Sender_argument.V1.t list [@sexp.list]
      ; recipients : Email_address.Stable.V1.t list
      ; rejected_recipients : Email_address.Stable.V1.t list
      ; id : Envelope_id.Stable.V1.t
      }
    [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 240d6191e3020998430593a649c8f084 |}]
    ;;
  end

  module V2 = struct
    type t =
      { sender : Sender.V1.t
      ; sender_args : Sender_argument.V1.t list [@sexp.list]
      ; recipients : Email_address.Stable.V1.t list
      ; rejected_recipients : Email_address.Stable.V1.t list
      ; route : string option
      ; id : Envelope_id.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare]

    let of_v1 (v1 : V1.t) =
      { sender = v1.sender
      ; sender_args = v1.sender_args
      ; recipients = v1.recipients
      ; rejected_recipients = v1.rejected_recipients
      ; id = v1.id
      ; route = None
      }
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 578f34e0b1182b24d7578c5434232935 |}]
    ;;
  end
end

open! Core
open Email_message

module T = struct
  type t = Stable.V2.t =
    { sender : Sender.t
    ; sender_args : Sender_argument.t list [@sexp.list]
    ; recipients : Email_address.t list
    ; rejected_recipients : Email_address.t list
    ; route : string option
    ; id : Envelope_id.t [@compare.ignore] [@hash.ignore]
    }
  [@@deriving sexp_of, fields ~getters ~iterators:create, compare, hash]

  let info t = t
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

type 'a create =
  ?id:Envelope_id.t
  -> sender:Sender.t
  -> ?sender_args:Sender_argument.t list
  -> recipients:Email_address.t list
  -> ?rejected_recipients:Email_address.t list
  -> ?route:string
  -> 'a

type 'a set =
  ?sender:Sender.t
  -> ?sender_args:Sender_argument.t list
  -> ?recipients:Email_address.t list
  -> ?rejected_recipients:Email_address.t list
  -> ?route:string option
  -> 'a

let string_sender t = sender t |> Sender.to_string
let string_recipients t = recipients t |> List.map ~f:Email_address.to_string

let set ?sender ?sender_args ?recipients ?rejected_recipients ?route t () =
  { sender = Option.value sender ~default:t.sender
  ; sender_args = Option.value sender_args ~default:t.sender_args
  ; id = t.id
  ; recipients = Option.value recipients ~default:t.recipients
  ; rejected_recipients = Option.value rejected_recipients ~default:t.rejected_recipients
  ; route = Option.value route ~default:t.route
  }
;;

let create
  ?id
  ~sender
  ?(sender_args = [])
  ~recipients
  ?(rejected_recipients = [])
  ?route
  ()
  =
  let id =
    match id with
    | Some id -> id
    | None -> Envelope_id.create ()
  in
  Fields.create ~sender ~sender_args ~recipients ~rejected_recipients ~route ~id
;;

let of_email ?(ignore_unparseable_recipient_header = false) email =
  let open Or_error.Let_syntax in
  let headers = Email.headers email in
  let%bind sender =
    match Email_headers.find_all headers "From" with
    | [ sender ] -> Sender.of_string sender
    | _ ->
      Or_error.error "Email contains no sender or multiple senders." email Email.sexp_of_t
  in
  let%bind recipients =
    Or_error.try_with (fun () ->
      Email_headers.find_all headers "To"
      @ Email_headers.find_all headers "CC"
      @ Email_headers.find_all headers "Bcc"
      |> List.map
           ~f:
             (String.filter ~f:(function
               | '\n' | '\r' -> false
               | _ -> true))
      |> List.concat_map ~f:(fun emails ->
        match Email_address.list_of_string emails with
        | Ok result -> result
        | Error error ->
          if ignore_unparseable_recipient_header then [] else Error.raise error))
  in
  Ok (create ~sender ~recipients ~rejected_recipients:[] ())
;;
