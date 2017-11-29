module Stable = struct
  open Core.Core_stable

  module Sender = Sender.Stable
  module Sender_argument = Sender_argument.Stable

  module V1 = struct
    type t =
      { sender              : Sender.V1.t
      ; sender_args         : Sender_argument.V1.t sexp_list
      ; recipients          : Email_address.Stable.V1.t list
      ; rejected_recipients : Email_address.Stable.V1.t list
      ; id                  : Envelope_id.Stable.V1.t
      } [@@deriving bin_io, sexp]
  end

  module V2 = struct
    type t =
      { sender              : Sender.V1.t
      ; sender_args         : Sender_argument.V1.t sexp_list
      ; recipients          : Email_address.Stable.V1.t list
      ; rejected_recipients : Email_address.Stable.V1.t list
      ; route               : string option
      ; id                  : Envelope_id.Stable.V1.t
      } [@@deriving bin_io, sexp]

    let of_v1 (v1 : V1.t) =
      { sender              = v1.sender
      ; sender_args         = v1.sender_args
      ; recipients          = v1.recipients
      ; rejected_recipients = v1.rejected_recipients
      ; id                  = v1.id
      ; route               = None
      }
    ;;
  end
end

open! Core
open Email_message

module T = struct
  type t = Stable.V2.t =
    { sender              : Sender.t
    ; sender_args         : Sender_argument.t sexp_list
    ; recipients          : Email_address.t list
    ; rejected_recipients : Email_address.t list
    ; route               : string option
    ; id                  : Envelope_id.t [@compare.ignore] [@hash.ignore]
    } [@@deriving sexp_of, fields, compare, hash]

  let info t = t
end
include T
include Comparable.Make_plain(T)
include Hashable.Make_plain(T)

let string_sender t = sender t |> Sender.to_string
let string_recipients t = recipients t |> List.map ~f:Email_address.to_string

let set
      { sender; sender_args; id; recipients; rejected_recipients; route }
      ?(sender = sender)
      ?(sender_args = sender_args)
      ?(recipients = recipients)
      ?(rejected_recipients=rejected_recipients)
      ?(route = route)
      () =
  { sender; sender_args; id; recipients; rejected_recipients; route }
;;

let create ?id ~sender ?(sender_args=[]) ~recipients ?(rejected_recipients=[]) ?route () =
  let id = match id with
    | Some id -> id
    | None -> Envelope_id.create ()
  in
  Fields.create ~sender ~sender_args ~recipients ~rejected_recipients ~route ~id
;;

let of_email email =
  let open Or_error.Monad_infix in
  let headers = Email.headers email in
  begin match Email_headers.find_all headers "From" with
  | [sender] -> Sender.of_string sender
  | _ ->
    Or_error.error "Email contains no sender or multiple senders."
      email Email.sexp_of_t
  end
  >>= fun sender ->
  Or_error.try_with (fun () ->
    (Email_headers.find_all headers "To"
     @ Email_headers.find_all headers "CC"
     @ Email_headers.find_all headers "Bcc")
    |> List.map ~f:(String.filter ~f:(function
      | '\n' | '\r' -> false
      | _ -> true))
    |> List.concat_map ~f:Email_address.list_of_string_exn)
  >>= fun recipients ->
  Ok (create ~sender ~recipients ~rejected_recipients:[] ())
;;