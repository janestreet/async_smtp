open! Core
open Poly

type t =
  | Hello of string
  | Extended_hello of string
  | Sender of string
  | Recipient of string
  | Auth of string * string option
  | Data
  | Reset
  | Quit
  | Help
  | Noop
  | Start_tls
[@@deriving variants, sexp]

let of_string = function
  | str when String.Caseless.is_prefix str ~prefix:"HELO " ->
    Hello (String.drop_prefix str 5 |> String.lstrip)
  | str when String.Caseless.is_prefix str ~prefix:"EHLO " ->
    Extended_hello (String.drop_prefix str 5 |> String.lstrip)
  | str when String.Caseless.is_prefix str ~prefix:"MAIL FROM:" ->
    Sender (String.drop_prefix str 10 |> String.lstrip)
  | str when String.Caseless.is_prefix str ~prefix:"RCPT TO:" ->
    Recipient (String.drop_prefix str 8 |> String.lstrip)
  | str when String.Caseless.is_prefix str ~prefix:"AUTH " ->
    let str = String.chop_prefix_exn str ~prefix:"AUTH " in
    (match String.lsplit2 str ~on:' ' with
     | None -> Auth (str, None)
     | Some (method_, first_response) -> Auth (method_, Some first_response))
  | str when String.Caseless.equal str "DATA" -> Data
  | str when String.Caseless.equal str "RSET" -> Reset
  | str when String.Caseless.equal str "QUIT" -> Quit
  | str when String.Caseless.equal str "HELP" -> Help
  | str when String.Caseless.equal str "NOOP" -> Noop
  | str when String.Caseless.equal str "STARTTLS" -> Start_tls
  | str -> failwithf "Unrecognized command: %s" str ()
;;

let to_string = function
  | Hello string -> "HELO " ^ string
  | Extended_hello string -> "EHLO " ^ string
  | Sender string -> "MAIL FROM: " ^ string
  | Recipient string -> "RCPT TO: " ^ string
  | Auth (meth, arg) ->
    "AUTH " ^ meth ^ Option.value_map arg ~default:"" ~f:(fun arg -> " " ^ arg)
  | Data -> "DATA"
  | Reset -> "RSET"
  | Quit -> "QUIT"
  | Help -> "HELP"
  | Noop -> "NOOP"
  | Start_tls -> "STARTTLS"
;;

(* Test parsing of commands to server *)
let%test_unit _ =
  let check a b = [%test_eq: t] (of_string a) b in
  Variants.iter
    ~hello:(fun _ ->
      check "HELO hi" (Hello "hi");
      check "helo hi" (Hello "hi"))
    ~extended_hello:(fun _ ->
      check "EHLO hi" (Extended_hello "hi");
      check "ehlo hi" (Extended_hello "hi"))
    ~help:(fun _ ->
      check "HELP" Help;
      check "help" Help)
    ~sender:(fun _ ->
      check "MAIL FROM:hi" (Sender "hi");
      check "mail from:hi" (Sender "hi"))
    ~recipient:(fun _ ->
      check "RCPT TO:hi" (Recipient "hi");
      check "rcpt to:hi" (Recipient "hi"))
    ~auth:(fun _ ->
      check "AUTH LOGIN" (Auth ("LOGIN", None));
      check "AUTH SIMPLE foobar" (Auth ("SIMPLE", Some "foobar")))
    ~data:(fun _ ->
      check "DATA" Data;
      check "data" Data)
    ~reset:(fun _ ->
      check "RSET" Reset;
      check "rset" Reset)
    ~quit:(fun _ ->
      check "QUIT" Quit;
      check "quit" Quit)
    ~noop:(fun _ ->
      check "NOOP" Noop;
      check "noop" Noop)
    ~start_tls:(fun _ ->
      check "STARTTLS" Start_tls;
      check "starttls" Start_tls)
;;

(* Test to_string and of_string functions for symmetry *)
let%test_unit _ =
  let check c = [%test_eq: t] c (of_string (to_string c)) in
  Variants.iter
    ~hello:(fun _ -> check (Hello "Helo World!~"))
    ~extended_hello:(fun _ -> check (Extended_hello "Helo World!~"))
    ~help:(fun _ -> check Help)
    ~sender:(fun _ -> check (Sender "Helo World!~"))
    ~recipient:(fun _ -> check (Recipient "Helo World!~"))
    ~auth:(fun _ ->
      check (Auth ("LOGIN", None));
      check (Auth ("SIMPLE", Some "foobar")))
    ~data:(fun _ -> check Data)
    ~reset:(fun _ -> check Reset)
    ~quit:(fun _ -> check Quit)
    ~noop:(fun _ -> check Noop)
    ~start_tls:(fun _ -> check Start_tls)
;;

(* Mechanical sanity checks *)
let%test_unit _ =
  let check_to_str a b = [%test_eq: string] a (to_string b) in
  let check_of_str a b = [%test_eq: t] a (of_string b) in
  let check_round a b =
    check_to_str a b;
    check_of_str b a;
    check_to_str a (of_string a);
    check_of_str b (to_string b)
  in
  Variants.iter
    ~hello:(fun _ ->
      check_round "HELO Helo World!~" (Hello "Helo World!~");
      check_of_str (Hello "Helo World!~") "helo Helo World!~")
    ~extended_hello:(fun _ ->
      check_round "EHLO Helo World!~" (Extended_hello "Helo World!~");
      check_of_str (Extended_hello "Helo World!~") "ehlo Helo World!~")
    ~sender:(fun _ ->
      check_round "MAIL FROM: Helo World!~" (Sender "Helo World!~");
      check_of_str (Sender "Helo World!~") "mail from: Helo World!~")
    ~recipient:(fun _ ->
      check_round "RCPT TO: Helo World!~" (Recipient "Helo World!~");
      check_of_str (Recipient "Bye World!~") "RCPT TO:Bye World!~";
      check_of_str (Recipient "Bye World!~") "rcpt to:Bye World!~")
    ~auth:(fun _ ->
      check_round "AUTH LOGIN foobar" (Auth ("LOGIN", Some "foobar"));
      check_round "AUTH SIMPLE" (Auth ("SIMPLE", None)))
    ~data:(fun _ ->
      check_round "DATA" Data;
      check_of_str Data "data")
    ~reset:(fun _ ->
      check_round "RSET" Reset;
      check_of_str Reset "rset")
    ~quit:(fun _ ->
      check_round "QUIT" Quit;
      check_of_str Quit "quit")
    ~help:(fun _ ->
      check_round "HELP" Help;
      check_of_str Help "help")
    ~noop:(fun _ ->
      check_round "NOOP" Noop;
      check_of_str Noop "noop")
    ~start_tls:(fun _ ->
      check_round "STARTTLS" Start_tls;
      check_of_str Start_tls "starttls")
;;
