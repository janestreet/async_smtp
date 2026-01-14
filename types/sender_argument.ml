module Stable = struct
  open Core.Core_stable

  module V1 = struct
    type t =
      | Auth of Email_address.Stable.V1.t option
      | Body of [ `Mime_8bit | `Mime_7bit ]
    [@@deriving bin_io, sexp, compare]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ad094f761223ee2e6893364df9c2c5c3 |}]
    ;;
  end
end

open! Core
open! Async

type t = Stable.V1.t =
  | Auth of Email_address.t option
  | Body of [ `Mime_8bit | `Mime_7bit ]
[@@deriving sexp_of, compare, hash]

let of_string = function
  | "AUTH=<>" -> Ok (Auth None)
  | str when String.is_prefix str ~prefix:"AUTH=" ->
    let email_address = String.drop_prefix str 5 |> String.strip in
    (match Email_address.of_string email_address with
     | Ok email_address -> Ok (Auth (Some email_address))
     | Error _ ->
       [%log.info_format "Unparsable argument to AUTH: %s" email_address];
       Ok (Auth None))
  | "BODY=8BITMIME" -> Ok (Body `Mime_8bit)
  | "BODY=7BIT" -> Ok (Body `Mime_7bit)
  | str -> Or_error.errorf "Unrecognized extension to mail command: %s" str
;;

let to_string = function
  | Body `Mime_8bit -> "BODY=8BITMIME"
  | Body `Mime_7bit -> "BODY=7BIT"
  | Auth email_address ->
    (match email_address with
     | None -> "AUTH=<>"
     | Some email_address -> "AUTH=" ^ Email_address.to_string email_address)
;;

let is_valid_arg arg ~allowed_extensions =
  List.exists allowed_extensions ~f:(fun ext ->
    match arg, ext with
    | Auth _, Smtp_extension.Auth _ -> true
    | Body _, Smtp_extension.Mime_8bit_transport -> true
    | _, _ -> false)
;;

let list_of_string ~allowed_extensions str =
  let open Or_error.Let_syntax in
  let%bind args =
    String.split ~on:' ' str
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:of_string
    |> Or_error.all
  in
  let has_invalid_arg =
    List.exists args ~f:(fun arg -> not (is_valid_arg arg ~allowed_extensions))
  in
  if has_invalid_arg
  then Or_error.errorf "Unable to parse MAIL FROM arguments: %s" str
  else Ok args
;;

(* Test parsing of commands to server *)
module%test _ = struct
  let check str extn =
    let e = of_string str |> Or_error.ok_exn in
    Poly.equal e extn
  ;;

  let%test _ = check "AUTH=<>" (Auth None)

  let%test _ =
    check "AUTH=<hello@world>" (Auth (Some (Email_address.of_string_exn "<hello@world>")))
  ;;
end

(* Test to_string and of_string functions for symmetry *)
module%test _ = struct
  let check extn =
    let e = of_string (to_string extn) |> Or_error.ok_exn in
    Poly.equal extn e
  ;;

  let%test _ = check (Auth None)
  let%test _ = check (Auth (Some (Email_address.of_string_exn "<hello@world>")))
end
