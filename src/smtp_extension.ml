open! Core

type t =
  | Start_tls
  | Auth_login
  | Mime_8bit_transport
  | Other of string
[@@deriving compare, sexp]

let equal = [%compare.equal: t]

let of_string str =
  let t =
    match String.uppercase str |> String.split ~on:' ' with
    | ["STARTTLS"] -> Start_tls
    | "AUTH" :: methods when List.mem methods "LOGIN" ~equal:String.equal -> Auth_login
    | ["8BITMIME"] -> Mime_8bit_transport
    | _ -> Other str
  in
  t
;;

let to_string = function
  | Start_tls -> "STARTTLS"
  | Auth_login -> "AUTH LOGIN"
  | Mime_8bit_transport -> "8BITMIME"
  | Other str -> str
;;
