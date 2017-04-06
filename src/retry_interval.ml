open! Core

type t =
  { span   : Time.Span.t
  ; jitter : Time.Span.t option
  } [@@deriving bin_io, compare, hash, sexp]

let t_of_sexp sexp =
  match Or_error.try_with (fun () -> t_of_sexp sexp) with
  | Ok t -> t
  | Error _ -> { span = Time.Span.t_of_sexp sexp; jitter = None }
;;

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
