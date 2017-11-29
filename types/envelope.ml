module Stable = struct
  open! Core.Core_stable
  open Email_message.Email_message_stable

  module V1 = struct
    type t =
      { info  : Envelope_info.Stable.V1.t
      ; email : Email.V1.t
      } [@@deriving bin_io, sexp]
  end

  module V2 = struct
    type t =
      { info  : Envelope_info.Stable.V2.t
      ; email : Email.V1.t
      } [@@deriving bin_io, sexp]

    let of_v1 (v1 : V1.t) =
      { info = Envelope_info.Stable.V2.of_v1 v1.info
      ; email = v1.email
      }
  end
end

open! Core
open Email_message

module T = struct
  type t = Stable.V2.t =
    { info : Envelope_info.t
    ; email : Email.t
    } [@@deriving sexp_of, fields, compare, hash]
end

include T
include Envelope_container.Make_with_info(T)
include Comparable.Make_plain(T)
include Hashable.Make_plain(T)

type envelope = t [@@deriving sexp_of, compare, hash]

let create ?id ~sender ?sender_args ~recipients ?rejected_recipients ?route ~email () =
  let info =
    Envelope_info.create ?id ~sender ?sender_args
      ~recipients ?rejected_recipients ?route ()
  in
  { info; email }
;;

let create' ~info ~email = Fields.create ~info ~email

let set
      { info; email }
      ?sender
      ?sender_args
      ?recipients
      ?rejected_recipients
      ?route
      ?(email = email)
      () =
  { info =
      Envelope_info.set info ?sender ?sender_args
        ?recipients ?rejected_recipients ?route ()
  ; email
  }
;;

let set' { info; email } ?(info = info) ?(email = email) () = { info; email }

let of_email email =
  Or_error.map (Envelope_info.of_email email) ~f:(fun info -> { info; email })
;;

let modify_email t ~f =
  let email = email t in
  let email = f email in
  { t with email }
;;

include Envelope_container.Make_with_headers(struct
    type t = envelope

    let headers t = Email.headers t.email
    let set_headers t headers =
      let email = Email.set_headers t.email headers in
      set t ~email ()
    ;;
  end)
