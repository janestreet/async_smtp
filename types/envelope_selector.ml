open Core
open Email_message

module Email_selector : sig
  module Base : sig
    type t = [ | Email_selector.Base.t ] [@@deriving sexp]
    include module type of Email_selector.Base with type t:=t
  end
end = struct
  module Base = struct
    include Email_selector.Base
    include (Email_selector.Stable.Base.V1 : sig
               type t = [ | Email_selector.Base.t] [@@deriving sexp]
             end with type t:=Email_selector.Base.t)
  end
end

module Regex = Re2

module Base = struct
  type t =
    (* When adding to this type, don't forget to add to examples below. *)
    [ Email_selector.Base.t
    | `envelope_sender of Regex.t
    | `exists_envelope_recipient of Regex.t
    | `all_envelope_recipients of Regex.t
    ] [@@deriving sexp]

  let matches' t envelope =
    match t with
    | (#Email_selector.Base.t as t) ->
      Email_selector.Base.matches' t (Envelope_bodiless.headers envelope)
    | `envelope_sender regex ->
      Regex.matches regex (Envelope_bodiless.string_sender envelope)
    | `exists_envelope_recipient regex ->
      List.exists (Envelope_bodiless.string_recipients envelope) ~f:(fun recipient ->
        Regex.matches regex recipient)
    | `all_envelope_recipients regex ->
      List.for_all (Envelope_bodiless.string_recipients envelope) ~f:(fun recipient ->
        Regex.matches regex recipient)

  let matches t envelope =
    let bodiless, _ = Envelope.split_bodiless envelope in
    matches' t bodiless

  let examples : t list =
    [ `envelope_sender (Regex.of_string ".*@janestreet.com")
    ; `exists_envelope_recipient (Regex.of_string ".*@janestreet.com")
    ; `all_envelope_recipients (Regex.of_string ".*@janestreet.com")
    ]
end

type t = Base.t Blang.t [@@deriving sexp]

let matches' t envelope =
  Blang.eval t (fun base -> Base.matches' base envelope)

let matches t envelope =
  let bodiless, _ = Envelope.split_bodiless envelope in
  matches' t bodiless

let example : t =
  ((Email_selector.Base.examples :> Base.t list) @ Base.examples)
  |> List.map ~f:Blang.base
  |> Blang.and_
