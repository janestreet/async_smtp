module Stable = struct
  open Core.Core_stable
  module Time = Time_float_unix.Stable

  module V1 = struct
    type t = Time.Span.V2.t [@@deriving sexp]
  end

  module V2 = struct
    type t =
      { span : Time.Span.V2.t
      ; jitter : Time.Span.V2.t option
      }
    [@@deriving bin_io, sexp]

    let of_v1 span = { span; jitter = None }

    let t_of_sexp sexp =
      try t_of_sexp sexp with
      | _ -> of_v1 (V1.t_of_sexp sexp)
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 3b06ea6a0b45b9d183bb377f049d7b98 |}]
    ;;
  end
end

open! Core

type t = Stable.V2.t =
  { span : Time_float.Span.t
  ; jitter : Time_float.Span.t option
  }
[@@deriving compare, hash, sexp_of]

let create ?jitter span = { span; jitter }

let to_span { span; jitter } =
  match jitter with
  | None -> span
  | Some jitter ->
    let percent =
      Time_float.Span.to_sec jitter /. Time_float.Span.to_sec span |> Percent.of_mult
    in
    Time_float.Span.randomize span ~percent
;;

let%test_unit _ =
  let base = 10. in
  let jitter = 5. in
  let t =
    { span = Time_float.Span.of_sec base; jitter = Some (Time_float.Span.of_sec jitter) }
  in
  let rec test i =
    if i = 0
    then ()
    else (
      let span = to_span t in
      assert (Time_float.Span.( >= ) span (Time_float.Span.of_sec (base -. jitter)));
      assert (Time_float.Span.( <= ) span (Time_float.Span.of_sec (base +. jitter)));
      test (i - 1))
  in
  test 10
;;
