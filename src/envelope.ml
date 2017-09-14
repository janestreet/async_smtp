module Stable = struct
  open Core.Core_stable
  open Email_message.Email_message_stable

  module Sender = Sender.Stable
  module Sender_argument = Sender_argument.Stable

  module Id = struct
    module V1 = struct
      type t = string [@@deriving bin_io, sexp]
    end
  end

  module Info = struct
    module V1 = struct
      type t =
        { sender              : Sender.V1.t
        ; sender_args         : Sender_argument.V1.t sexp_list
        ; recipients          : Email_address.V1.t list
        ; rejected_recipients : Email_address.V1.t list
        ; route               : string option
        ; id                  : Id.V1.t
        } [@@deriving bin_io, sexp]
    end
  end

  module V1 = struct
    type t =
      { info : Info.V1.t
      ; email : Email.V1.t
      } [@@deriving bin_io, sexp]
  end
end

open! Core
open Email_message

module Id = struct
  include String

  let create () =
    let (^-) a b               = a ^"-"^ b in
    let time                   = Time.now () in
    let time_since_epoch       = Time.to_span_since_epoch time |> Time.Span.to_sec in
    let (integral, fractional) =
      let parts      = Float.modf time_since_epoch in
      let integral   = Float.Parts.integral parts in
      let fractional = (Float.Parts.fractional parts /. 0.0005) in
      integral, fractional
    in
    let pid = Unix.getpid () |> Pid.hash in
    let encode = Common.urlbase64_encode_float in
    let t =
      (encode integral)
      ^- (Int.to_float pid |> encode)
      ^- (encode ~length:2 fractional)
    in
    (* optionally pause until the next time in which a new [t] would be generated *)
    let next_unique_id_time = Time.add time (Time.Span.of_sec 0.0005) in
    let diff = Time.diff next_unique_id_time (Time.now ()) in
    (if Time.Span.(>) diff (Time.Span.of_int_sec 0)
     then Time.pause diff
     else ());
    t
  ;;
end

module Info = struct
  module T = struct
    type t = Stable.Info.V1.t =
      { sender              : Sender.t
      ; sender_args         : Sender_argument.t sexp_list
      ; recipients          : Email_address.t list
      ; rejected_recipients : Email_address.t list
      ; route               : string option
      ; id                  : Id.t [@compare.ignore] [@hash.ignore]
      } [@@deriving sexp_of, fields, compare, hash]
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
      | None -> Id.create ()
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
end

module Infoable = struct
  module type S = sig
    type t

    val sender              : t -> Sender.t
    val sender_args         : t -> Sender_argument.t list
    val string_sender       : t -> string
    val recipients          : t -> Email_address.t list
    val rejected_recipients : t -> Email_address.t list
    val string_recipients   : t -> string list
    val route               : t -> string option
    val id                  : t -> Id.t
  end

  module Make(S : sig
      type t

      val info : t -> Info.t
    end) = struct

    let sender              t = Info.sender              (S.info t)
    let sender_args         t = Info.sender_args         (S.info t)
    let string_sender       t = Info.string_sender       (S.info t)
    let recipients          t = Info.recipients          (S.info t)
    let rejected_recipients t = Info.rejected_recipients (S.info t)
    let string_recipients   t = Info.string_recipients   (S.info t)
    let route               t = Info.route               (S.info t)
    let id                  t = Info.id                  (S.info t)
  end
end

module T = struct
  type t = Stable.V1.t =
    { info : Info.t
    ; email : Email.t
    } [@@deriving sexp_of, fields, compare, hash]
end

include T
include Infoable.Make(T)
include Comparable.Make_plain(T)
include Hashable.Make_plain(T)

type envelope = t [@@deriving sexp_of, compare, hash]

let create ?id ~sender ?sender_args ~recipients ?rejected_recipients ?route ~email () =
  let info = Info.create ?id ~sender ?sender_args ~recipients ?rejected_recipients ?route () in
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
  { info = Info.set info ?sender ?sender_args ?recipients ?rejected_recipients ?route (); email }
;;

let set' { info; email } ?(info = info) ?(email = email) () = { info; email }

let of_email email =
  Or_error.map (Info.of_email email) ~f:(fun info -> { info; email })
;;

let last_header ?whitespace t name =
  Email_headers.last ?whitespace (Email.headers (email t)) name
;;

let find_all_headers ?whitespace t name =
  Email_headers.find_all ?whitespace (Email.headers (email t)) name
;;

let modify_email t ~f =
  let email = email t in
  let email = f email in
  { t with email }
;;

let modify_headers t ~f =
  modify_email t ~f:(fun email ->
    Email.modify_headers email ~f)
;;

let add_header ?whitespace t ~name ~value =
  modify_headers t ~f:(fun headers ->
    Email_headers.add ?whitespace headers ~name ~value)
;;

let add_headers ?whitespace t ts =
  modify_headers t ~f:(fun headers ->
    Email_headers.add_all ?whitespace headers ts)
;;

let set_header ?whitespace t ~name ~value =
  modify_headers t ~f:(fun headers ->
    Email_headers.set ?whitespace headers ~name ~value)
;;

let add_header_at_bottom ?whitespace t ~name ~value =
  modify_headers t ~f:(fun headers ->
    Email_headers.add_at_bottom ?whitespace headers ~name ~value)
;;

let add_headers_at_bottom ?whitespace t ts =
  modify_headers t ~f:(fun headers ->
    Email_headers.add_all_at_bottom ?whitespace headers ts)
;;

let set_header_at_bottom ?whitespace t ~name ~value =
  modify_headers t ~f:(fun headers ->
    Email_headers.set_at_bottom ?whitespace headers ~name ~value)
;;

let filter_headers ?whitespace t ~f =
  modify_headers t ~f:(fun headers ->
    Email_headers.filter ?whitespace headers ~f)
;;

let map_headers ?whitespace t ~f =
  modify_headers t ~f:(fun headers ->
    Email_headers.map ?whitespace headers ~f)
;;

let smash_and_add_header ?whitespace t ~name ~value =
  modify_headers t ~f:(fun headers ->
    Email_headers.smash_and_add ?whitespace headers ~name ~value)
;;

module With_next_hop = struct
  module T = struct
    type t =
      { envelope : envelope
      ; next_hop_choices : Address.t list
      ; retry_intervals : Retry_interval.t list
      } [@@deriving fields, sexp_of, compare, hash]

    let info t = info t.envelope
  end

  include T
  include Infoable.Make(T)
  include Comparable.Make_plain(T)
  include Hashable.Make_plain(T)

  let create ~envelope ~next_hop_choices ~retry_intervals =
    Fields.create ~envelope ~next_hop_choices ~retry_intervals
  ;;

  let email t = email t.envelope

  let set t ?sender ?sender_args ?recipients () =
    { t with envelope = set t.envelope ?sender ?sender_args ?recipients () }
  ;;
end
