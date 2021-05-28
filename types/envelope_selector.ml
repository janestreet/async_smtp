open! Core.Core_stable

module Stable = struct
  module Base = struct
    module V1 = struct
      type t =
        (* When adding to this type, don't forget to add to examples below. *)
        [ Email_message.Email_selector.Stable.Base.V1.t
        | `envelope_sender of Re2.Stable.V1_no_options.t
        | `exists_envelope_recipient of Re2.Stable.V1_no_options.t
        | `all_envelope_recipients of Re2.Stable.V1_no_options.t
        ]
      [@@deriving bin_shape, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 60bd581ef4f767466e97e74e3c15f1b8 |}]
      ;;
    end
  end

  module V1 = struct
    type t = Base.V1.t Blang.V1.t [@@deriving bin_shape, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 343a90f661b8f1e5dbe9d2c82f93ce4c |}]
    ;;
  end
end

open Core
open Email_message
module Regex = Re2

module Base = struct
  type t = Stable.Base.V1.t [@@deriving sexp_of]

  let matches' t envelope =
    match t with
    | #Email_selector.Base.t as t ->
      Email_selector.Base.matches' t (Envelope_bodiless.headers envelope)
    | `envelope_sender regex ->
      Regex.matches regex (Envelope_bodiless.string_sender envelope)
    | `exists_envelope_recipient regex ->
      List.exists (Envelope_bodiless.string_recipients envelope) ~f:(fun recipient ->
        Regex.matches regex recipient)
    | `all_envelope_recipients regex ->
      List.for_all (Envelope_bodiless.string_recipients envelope) ~f:(fun recipient ->
        Regex.matches regex recipient)
  ;;

  let matches t envelope =
    let bodiless, _ = Envelope.split_bodiless envelope in
    matches' t bodiless
  ;;

  let examples : t list =
    [ `envelope_sender (Regex.of_string ".*@janestreet.com")
    ; `exists_envelope_recipient (Regex.of_string ".*@janestreet.com")
    ; `all_envelope_recipients (Regex.of_string ".*@janestreet.com")
    ]
  ;;
end

type t = Base.t Blang.t [@@deriving sexp_of]

let matches' t envelope = Blang.eval t (fun base -> Base.matches' base envelope)

let matches t envelope =
  let bodiless, _ = Envelope.split_bodiless envelope in
  matches' t bodiless
;;

let example : t =
  (Email_selector.Base.examples :> Base.t list) @ Base.examples
  |> List.map ~f:Blang.base
  |> Blang.and_
;;
