open! Core
open Email_message

module T = struct
  type t =
    { info : Envelope_info.t
    ; headers : Email_headers.t
    }
  [@@deriving fields ~getters ~iterators:create, compare, hash, sexp_of]

  let headers t = t.headers
  let set_headers t headers = { t with headers }
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)
include Envelope_container.Make_with_info (T)
include Envelope_container.Make_with_headers (T)

let create ?id ~sender ?sender_args ~recipients ?rejected_recipients ?route ~headers () =
  let info =
    Envelope_info.create
      ?id
      ~sender
      ?sender_args
      ~recipients
      ?rejected_recipients
      ?route
      ()
  in
  { info; headers }
;;

let create' = Fields.create

let set ?sender ?sender_args ?recipients ?rejected_recipients ?route ?headers t () =
  { info =
      Envelope_info.set
        t.info
        ?sender
        ?sender_args
        ?recipients
        ?rejected_recipients
        ?route
        ()
  ; headers = Option.value headers ~default:t.headers
  }
;;
