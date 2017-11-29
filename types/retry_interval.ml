module Stable = struct
  open Core.Core_stable
  module V1 = struct
    type t = Time.Span.V2.t [@@deriving sexp]
  end
  module V2 = struct
    type t =
      { span   : Time.Span.V2.t
      ; jitter : Time.Span.V2.t option
      } [@@deriving bin_io, sexp]

    let of_v1 span = { span; jitter = None }

    let t_of_sexp sexp =
      try t_of_sexp sexp with
      | _ -> of_v1 (V1.t_of_sexp sexp)
  end
end

open! Core

type t = Stable.V2.t =
  { span   : Time.Span.t
  ; jitter : Time.Span.t option
  } [@@deriving compare, hash, sexp_of]

let create ?jitter span = { span; jitter }

let to_span { span; jitter } =
  match jitter with
  | None -> span
  | Some jitter ->
    let percent =
      Time.Span.to_sec jitter /. Time.Span.to_sec span
      |> Percent.of_mult
    in
    Time.Span.randomize span ~percent
;;

let%test_unit _ =
  let base = 10. in
  let jitter = 5. in
  let t = { span = Time.Span.of_sec base; jitter = Some (Time.Span.of_sec jitter) } in
  let rec test i =
    if i = 0 then
      ()
    else (
      let span = to_span t in
      assert (Time.Span.(>=) span (Time.Span.of_sec (base -. jitter)));
      assert (Time.Span.(<=) span (Time.Span.of_sec (base +. jitter)));
      test (i-1))
  in
  test 10
;;
