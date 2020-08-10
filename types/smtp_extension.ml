open! Core

let all_of_list _ = [ [] ]
let all_of_string = []

type t =
  | Start_tls
  | Auth of string list
  | Mime_8bit_transport
  | Other of string
[@@deriving compare, sexp, enumerate]

let equal = [%compare.equal: t]

let of_string str =
  let t =
    match String.uppercase str |> String.split ~on:' ' with
    | [ "STARTTLS" ] -> Start_tls
    | "AUTH" :: mechs ->
      mechs |> List.map ~f:String.strip |> List.filter ~f:(Fn.non String.is_empty) |> Auth
    | [ "8BITMIME" ] -> Mime_8bit_transport
    | _ -> Other str
  in
  t
;;

let to_string = function
  | Start_tls -> "STARTTLS"
  | Auth mechs -> String.concat ~sep:" " ("AUTH" :: mechs)
  | Mime_8bit_transport -> "8BITMIME"
  | Other str -> str
;;
