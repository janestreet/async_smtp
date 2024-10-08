module Stable = struct
  open Core.Core_stable

  module V1 = struct
    type t = string [@@deriving bin_io, sexp, compare]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
    ;;
  end
end

open! Core
module Unix = Core_unix
module Time = Time_float_unix

include (
  String :
  sig
    type t = string [@@deriving sexp_of]

    val to_string : t -> string
    val of_string : string -> t

    include Comparable.S_plain with type t := t
    include Hashable.S_plain with type t := t
  end)

let urlbase64_encode_float ?(length = 6) f =
  match Int64.of_float f with
  | exception _ -> invalid_arg "cannot encode a float that does not fit in an Int64"
  | n ->
    String.init 9 ~f:(fun i ->
      Int64.shift_right n (64 - (8 * i))
      |> Int64.bit_and 0xffL
      |> Int64.to_int_exn
      |> Char.of_int_exn)
    |> Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
    |> String.sub ~pos:(12 - length) ~len:length
;;

let create () =
  let ( ^- ) a b = a ^ "-" ^ b in
  let time = Time.now () in
  let time_since_epoch = Time.to_span_since_epoch time |> Time.Span.to_sec in
  let integral, fractional =
    let parts = Float.modf time_since_epoch in
    let integral = Float.Parts.integral parts in
    let fractional = Float.Parts.fractional parts /. 0.0005 in
    integral, fractional
  in
  let pid = Unix.getpid () |> Pid.hash in
  let encode = urlbase64_encode_float in
  let t =
    encode integral ^- (Int.to_float pid |> encode) ^- encode ~length:2 fractional
  in
  (* optionally pause until the next time in which a new [t] would be generated *)
  let next_unique_id_time = Time.add time (Time.Span.of_sec 0.0005) in
  let diff = Time.diff next_unique_id_time (Time.now ()) in
  if Time.Span.( > ) diff (Time.Span.of_int_sec 0) then Time.pause diff else ();
  t
;;

module%test [@name "urlbase64_encode_float"] _ = struct
  let%expect_test _ =
    printf "%s" (urlbase64_encode_float 1234.1235453123);
    [%expect_exact {|AAAATS|}]
  ;;

  let%expect_test _ =
    printf "%s" (urlbase64_encode_float 1234.);
    [%expect_exact {|AAAATS|}]
  ;;

  let%expect_test _ =
    printf "%s" (urlbase64_encode_float 1235.);
    [%expect_exact {|AAAATT|}]
  ;;

  let%expect_test _ =
    printf "%s" (urlbase64_encode_float 123456.);
    [%expect_exact {|AAAeJA|}]
  ;;

  let%expect_test _ =
    printf "%s" (urlbase64_encode_float Int64.(to_float (max_value - 1024L)));
    [%expect_exact {|____wA|}]
  ;;
end
