open! Core
open Poly

module Code = struct
  type 'a t_ =
    [ `Service_ready_220
    | `Closing_connection_221
    | `Authentication_successful_235
    | `Ok_completed_250
    | `Start_authentication_input_334
    | `Start_mail_input_354
    | `Service_unavailable_421
    | `Local_error_451
    | `Message_rate_exceeded_452
    | `Tls_temporarily_unavailable_454
    | `Unable_to_accommodate_455
    | `Command_not_recognized_500
    | `Syntax_error_501
    | `Command_not_implemented_502
    | `Bad_sequence_of_commands_503
    | `Parameter_not_implemented_504
    | `Authentication_required_530
    | `Authentication_credentials_invalid_535
    | `Mailbox_unavailable_550
    | `Exceeded_storage_allocation_552
    | `Transaction_failed_554
    | `From_to_parameters_bad_555
    | `Other of 'a
    ]
  [@@deriving bin_io, sexp, enumerate]

  type t = int t_ [@@deriving bin_io, sexp]

  let of_int = function
    | 220 -> `Service_ready_220
    | 221 -> `Closing_connection_221
    | 235 -> `Authentication_successful_235
    | 250 -> `Ok_completed_250
    | 334 -> `Start_authentication_input_334
    | 354 -> `Start_mail_input_354
    | 421 -> `Service_unavailable_421
    | 451 -> `Local_error_451
    | 452 -> `Message_rate_exceeded_452
    | 454 -> `Tls_temporarily_unavailable_454
    | 455 -> `Unable_to_accommodate_455
    | 500 -> `Command_not_recognized_500
    | 501 -> `Syntax_error_501
    | 502 -> `Command_not_implemented_502
    | 503 -> `Bad_sequence_of_commands_503
    | 504 -> `Parameter_not_implemented_504
    | 530 -> `Authentication_required_530
    | 535 -> `Authentication_credentials_invalid_535
    | 550 -> `Mailbox_unavailable_550
    | 552 -> `Exceeded_storage_allocation_552
    | 554 -> `Transaction_failed_554
    | 555 -> `From_to_parameters_bad_555
    | i -> `Other i
  ;;

  let to_int = function
    | `Service_ready_220 -> 220
    | `Closing_connection_221 -> 221
    | `Authentication_successful_235 -> 235
    | `Ok_completed_250 -> 250
    | `Start_authentication_input_334 -> 334
    | `Start_mail_input_354 -> 354
    | `Service_unavailable_421 -> 421
    | `Local_error_451 -> 451
    | `Message_rate_exceeded_452 -> 452
    | `Tls_temporarily_unavailable_454 -> 454
    | `Unable_to_accommodate_455 -> 455
    | `Command_not_recognized_500 -> 500
    | `Syntax_error_501 -> 501
    | `Command_not_implemented_502 -> 502
    | `Bad_sequence_of_commands_503 -> 503
    | `Parameter_not_implemented_504 -> 504
    | `Authentication_required_530 -> 530
    | `Authentication_credentials_invalid_535 -> 535
    | `Mailbox_unavailable_550 -> 550
    | `Exceeded_storage_allocation_552 -> 552
    | `Transaction_failed_554 -> 554
    | `From_to_parameters_bad_555 -> 555
    | `Other i -> i
  ;;

  let all =
    lazy (List.range 100 999 |> List.filter ~f:(fun i -> of_int i = `Other i) |> all_of_t_)
  ;;

  (* Check that every int maps uniquely to a code, and back to itself *)
  let%test_unit _ =
    List.range 100 999
    |> List.iter ~f:([%test_pred: int] (fun i -> to_int (of_int i) = i))
  ;;

  (* Check that every code maps to an int and back to itself *)
  let%test_unit _ =
    Lazy.force all |> List.iter ~f:([%test_pred: t] (fun c -> of_int (to_int c) = c))
  ;;
end

type t =
  { code : Code.t
  ; raw_message : string list
  }
[@@deriving bin_io]

include
  Sexpable.Of_sexpable
    (struct
      type t = int * string list [@@deriving sexp]
    end)
    (struct
      type nonrec t = t

      let of_sexpable (code, raw_message) = { code = Code.of_int code; raw_message }
      let to_sexpable { code; raw_message } = Code.to_int code, raw_message
    end)

let code t = Code.to_int t.code

let create code fmt =
  ksprintf
    (fun raw_message ->
      let raw_message =
        if String.is_empty raw_message then [ "" ] else String.split_lines raw_message
      in
      { code; raw_message })
    fmt
;;

let service_ready_220 greeting = create `Service_ready_220 "%s" greeting
let closing_connection_221 = create `Closing_connection_221 "closing connection"

let authentication_successful_235 =
  create `Authentication_successful_235 "Authentication successful"
;;

let ok_completed_250 msg = create `Ok_completed_250 "Ok: %s" msg
let start_authentication_input_334 msg = create `Start_authentication_input_334 "%s" msg

let start_mail_input_354 =
  create `Start_mail_input_354 "Enter message, ending with \".\" on a line by itself"
;;

let service_unavailable_421 =
  create `Service_unavailable_421 "Service not available, closing transmission channel"
;;

let data_timeout_421 =
  create
    `Service_unavailable_421
    "SMTP incoming data timeout, closing transmission channel"
;;

let command_timeout_421 =
  create `Service_unavailable_421 "SMTP command timeout, closing transmission channel"
;;

let local_error_451 msg = create `Local_error_451 "Local error: %s" msg

(* We use [`Other] here as [`Message_rate_exceeded] reserved [452], yet insufficent system
   storage is also a [452]:

   https://www.rfc-editor.org/rfc/rfc5321#section-4.2.2
*)
let insufficent_system_storage_452 = create (`Other 452) "Insufficent system storage"
let message_rate_exceeded_452 = create `Message_rate_exceeded_452 "Message rate exceeded"

let unable_to_accommodate_455 msg =
  create `Unable_to_accommodate_455 "Unable to accommodate: %s" msg
;;

let command_not_recognized_500 command =
  create `Command_not_recognized_500 !"Unrecognized command: %s" command
;;

let syntax_error_501 error =
  create `Syntax_error_501 "Syntax error in parameters or arguments: %s" error
;;

let command_not_implemented_502 command =
  create
    `Command_not_implemented_502
    !"Smtp_command not implemented: %{Smtp_command}"
    command
;;

let bad_sequence_of_commands_503 command =
  create
    `Bad_sequence_of_commands_503
    !"Bad sequence of commands: %{Smtp_command}"
    command
;;

let authentication_required_530 =
  create `Authentication_required_530 "Authentication required."
;;

let authentication_credentials_invalid_535 =
  create `Authentication_credentials_invalid_535 "Authentication credentials invalid"
;;

let mailbox_unavailable_550 reason =
  create `Mailbox_unavailable_550 "Mailbox unavailable: %s" reason
;;

let exceeded_storage_allocation_552 =
  create `Exceeded_storage_allocation_552 "Exceeded storage allocation"
;;

let transaction_failed_554 message =
  create `Transaction_failed_554 !"Transaction failed: %s" message
;;

let from_to_parameters_bad_555 msg =
  create `From_to_parameters_bad_555 !"From or To parameters bad: %s" msg
;;

let to_string t =
  let code = code t in
  let rec to_string_lines acc = function
    | [] ->
      (* This code should never be reached as [raw_message] is guaranteed to be non empty,
         by [create] and [parse]. *)
      assert false
    | [ s ] -> sprintf "%d %s" code s :: acc |> List.rev
    | s :: ss -> to_string_lines (sprintf "%d-%s" code s :: acc) ss
  in
  to_string_lines [] t.raw_message |> String.concat ~sep:"\r\n"
;;

let of_code_message code raw_message =
  let code = Code.of_int code in
  { code; raw_message }
;;

type partial = string * int * string list

let parse ?partial str =
  let finish ~prefix ~code ~rev_msg =
    let i = String.length prefix in
    let d = str.[i] in
    let rev_msg = String.slice str (i + 1) (String.length str) :: rev_msg in
    match d with
    | ' ' -> `Done (of_code_message code (List.rev rev_msg))
    | '-' -> `Partial (prefix, code, rev_msg)
    | _ -> failwith "Not a valid SMTP Reply separator char, expected ' ' or  '-'"
  in
  match partial with
  | Some (prefix, code, rev_msg) ->
    if String.is_prefix ~prefix str
    then finish ~prefix ~code ~rev_msg
    else failwith "Unexpected prefix on SMTP Reply"
  | None ->
    let rec loop i =
      let d = str.[i] in
      if Char.is_digit d
      then loop (i + 1)
      else (
        let prefix = String.slice str 0 i in
        finish ~prefix ~code:(Int.of_string prefix) ~rev_msg:[])
    in
    loop 0
;;

let of_string str =
  let rec loop ?partial = function
    | [] -> failwith "\"\" is not a valid SMTP Reply"
    | s :: ss ->
      (match parse ?partial s with
       | `Partial partial -> loop ~partial ss
       | `Done res ->
         if List.is_empty ss then res else failwith "More than one SMTP reply")
  in
  String.split_lines str |> loop
;;

let of_bigstring bs = of_string (Bigstring.to_string bs)

let is_ok t =
  let code = code t in
  (200 <= code && code <= 299) || (300 <= code && code <= 399)
;;

let is_permanent_error t =
  let code = code t in
  500 <= code && code <= 599
;;

let decorate t ~additional_lines =
  { t with raw_message = t.raw_message @ additional_lines }
;;

module%test _ = struct
  let check reply = [%test_eq: t] reply (of_string (to_string reply))

  let%test_unit _ =
    List.iter (Lazy.force Code.all) ~f:(fun code ->
      check { code; raw_message = [ "test" ] };
      check { code; raw_message = [ "test0"; "test2"; "test3" ] })
  ;;

  let%test_unit _ = check (service_ready_220 "test")
  let%test_unit _ = check closing_connection_221
  let%test_unit _ = check (ok_completed_250 "test")
  let%test_unit _ = check start_mail_input_354
  let%test_unit _ = check service_unavailable_421
  let%test_unit _ = check (local_error_451 "test")
  let%test_unit _ = check (command_not_recognized_500 "test")
  let%test_unit _ = check (syntax_error_501 "test")
  let%test_unit _ = check (command_not_implemented_502 (Smtp_command.Hello "Test"))
  let%test_unit _ = check (bad_sequence_of_commands_503 (Smtp_command.Hello "Test"))
  let%test_unit _ = check exceeded_storage_allocation_552
  let%test_unit _ = check (transaction_failed_554 "test")
  let%test_unit _ = check (start_authentication_input_334 "")
  let%test_unit _ = check (start_authentication_input_334 "abc")
  let%test_unit _ = check (start_authentication_input_334 "abc\ndef")
  let check_multiline a b = [%test_eq: t] b (of_string a)

  let%test_unit _ =
    check_multiline
      "250-Ok: test1\r\n250-test2\r\n250 test3"
      (ok_completed_250 "test1\ntest2\ntest3")
  ;;

  let%test_unit _ =
    check_multiline
      "250-Ok: test1\r\n250-test2\r\n250 test3"
      (ok_completed_250 "test1\r\ntest2\r\ntest3")
  ;;
end
