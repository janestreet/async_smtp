open! Core
open Angstrom
open Angstrom.Let_syntax

let null =
  let%map _ = option "" Email_address.Expert.Parser.prefix
  and _ = string "<>" in
  `Null
;;

let email =
  let%map email = Email_address.Expert.Parser.email in
  `Email email
;;

let sender_with_args =
  let%map () = Email_address.Expert.Parser.skip_whitespace
  and sender = null <|> email
  and args = take_while (const true) in
  sender, args
;;
