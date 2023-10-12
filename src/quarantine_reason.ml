module Stable = struct
  open Core.Core_stable
  open Async_smtp_types.Async_smtp_types_stable

  module V1_no_string = struct
    type t =
      { description : string
      ; envelope_sender : Smtp_envelope.Sender.V1.t
      ; from_headers : string
      }
    [@@deriving bin_io, sexp, compare]
  end

  module V1 = struct
    include V1_no_string
    include Core.Sexpable.To_stringable (V1_no_string)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 92af1bccc79f5090a238bf0e15f3fb9c |}]
    ;;
  end
end

open! Core
open Async_smtp_types

type t = Stable.V1.t =
  { description : string
  ; envelope_sender : Smtp_envelope.Sender.t
  ; from_headers : string
  }
[@@deriving sexp_of, compare, fields ~getters]

let to_string t = Sexp.to_string (sexp_of_t t)

let of_envelope ~description envelope =
  let from_headers =
    Smtp_envelope.Bodiless.find_all_headers envelope "From" |> String.concat ~sep:", "
  in
  { description; envelope_sender = Smtp_envelope.Bodiless.sender envelope; from_headers }
;;
