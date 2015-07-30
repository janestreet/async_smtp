open Core.Std
open Async.Std
open Async_ssl.Std
open Email_message.Std

module Headers = Email_headers

module Email_address = Email_address

module Sender = struct
  open Or_error.Monad_infix

  type t =
    [ `Null
    | `Email of Email_address.t
    ] with sexp, bin_io, compare

  let of_string ?default_domain s =
    match String.strip s with
    | "<>" -> Ok `Null
    | s ->
      Email_address.of_string ?default_domain s
      >>= fun email ->
      Ok (`Email email)

  let to_string = function
    | `Null -> "<>"
    | `Email email -> Email_address.to_string email

  let map t ~f =
    match t with
    | `Null -> `Null
    | `Email email -> `Email (f email)

  module T = struct
    type nonrec t = t with sexp, bin_io, compare

    let to_string = to_string
    let of_string s = of_string s |> Or_error.ok_exn

    let hash = function
      | `Null -> Hashtbl.hash `Null
      | `Email email -> Email_address.hash email
  end
  include Hashable.Make(T)
  include Comparable.Make(T)
  include Sexpable.Of_stringable(T)
end

module Envelope = struct
  module Id = struct
    include String

    let create () =
      let (^-) a b               = a ^"-"^ b in
      let time                   = Time.now () in
      let time_since_epoch       = Time.to_epoch time in
      let (integral, fractional) =
        let parts      = Float.modf time_since_epoch in
        let integral   = Float.Parts.integral parts in
        let fractional = (Float.Parts.fractional parts /. 0.0005) in
        integral, fractional
      in
      let pid = Unix.getpid () |> Pid.hash in
      let encode = Base64.encode_float in
      let t =
        (encode integral)
        ^- (Int.to_float pid |> encode)
        ^- (encode ~length:2 fractional)
      in
      (* optionally pause until the next time in which a new [t] would be generated *)
      let next_unique_id_time = Time.add time (Time.Span.of_sec 0.0005) in
      let diff = Time.diff next_unique_id_time (Time.now ()) in
      (if Time.Span.(>) diff (Time.Span.of_int_sec 0)
      then Time.pause diff
      else ());
      t
    ;;
  end

  module T = struct
    type t =
      { sender     : Sender.t
      ; recipients : Email_address.t list
      ; rejected_recipients : Email_address.t list
      ; id         : Id.t
      ; email      : Email.t
      } with fields, sexp, bin_io, compare
    ;;

    let compare t1 t2 =
      let t1 = { t1 with id = "" } in
      let t2 = { t2 with id = "" } in
      compare t1 t2

    let hash { sender; recipients; rejected_recipients; id = _; email } =
      let recipient_hash1 =
        List.map recipients ~f:Email_address.hash
        |> List.map ~f:Int.to_string
        |> String.concat ~sep:""
        |> String.hash
      in
      let recipient_hash2 =
        List.map rejected_recipients ~f:Email_address.hash
        |> List.map ~f:Int.to_string
        |> String.concat ~sep:""
        |> String.hash
      in
      let x =
        Sender.hash sender,
        recipient_hash1,
        recipient_hash2,
        Email.hash email
      in
      Hashtbl.hash x
  end

  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let string_sender t = sender t |> Sender.to_string
  let string_recipients t = recipients t |> List.map ~f:Email_address.to_string

  let set
        { sender; id; email; recipients; rejected_recipients }
        ?(sender = sender)
        ?(recipients = recipients)
        ?(rejected_recipients=rejected_recipients)
        ?(email = email)
        () =
    { sender; id; email; recipients; rejected_recipients }

  let create ?id ~sender ~recipients ?(rejected_recipients=[]) ~email () =
    let id =
      let default = Id.create () in
      Option.value ~default id
    in
    Fields.create ~sender ~recipients ~rejected_recipients ~email ~id
  ;;

  let of_email email =
    let open Or_error.Monad_infix in
    let headers = Email.headers email in
    begin match Headers.find_all headers "From" with
    | [sender] -> Sender.of_string sender
    | _ ->
      Or_error.error "Email contains no sender or multiple senders."
        email Email.sexp_of_t
    end
    >>= fun sender ->
    Or_error.try_with (fun () ->
      (Headers.find_all headers "To"
       @ Headers.find_all headers "CC"
       @ Headers.find_all headers "Bcc")
      |> List.map ~f:(String.filter ~f:(function
        | '\n' | '\r' -> false
        | _ -> true))
      |> List.concat_map ~f:Email_address.list_of_string_exn)
    >>= fun recipients ->
    Ok (create ~sender ~recipients ~rejected_recipients:[] ~email ())

  let get_headers t ~name =
    let email = email t in
    Email.headers email
    |> fun headers ->
    Headers.find_all headers name

  let modify_email t ~f =
    let email = email t in
    let email = f email in
    { t with email }

  let modify_headers t ~f =
    modify_email t ~f:(fun email ->
      let headers = Email.headers email in
      let headers = f headers in
      Email.set_headers email headers)

  let add_header t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.add headers ~name ~value)

  let set_header t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.set headers ~name ~value)

  let add_header_at_bottom t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.add_at_bottom headers ~name ~value)

  let set_header_at_bottom t ~name ~value =
    modify_headers t ~f:(fun headers ->
        Headers.set_at_bottom headers ~name ~value)

  let filter_headers t ~f =
    modify_headers t ~f:(fun headers ->
      List.filter headers ~f:(fun (name, value) ->
        f ~name ~value))
end

module Envelope_with_next_hop = struct
  module T = struct
    type t =
      { envelope : Envelope.t
      ; next_hop_choices : Host_and_port.t list
      ; retry_intervals : Time.Span.t list
      } with fields, sexp, bin_io, compare

    let hash { envelope; next_hop_choices; retry_intervals } =
      let x =
        Envelope.hash envelope,
        Hashtbl.hash next_hop_choices,
        Hashtbl.hash retry_intervals
      in
      Hashtbl.hash x
  end

  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let create ~envelope ~next_hop_choices ~retry_intervals =
    Fields.create ~envelope ~next_hop_choices ~retry_intervals

  let act_on_envelope f t = f (envelope t)
  let sender            = act_on_envelope Envelope.sender
  let string_sender     = act_on_envelope Envelope.string_sender
  let recipients        = act_on_envelope Envelope.recipients
  let string_recipients = act_on_envelope Envelope.string_recipients
  let email             = act_on_envelope Envelope.email
  let id                = act_on_envelope Envelope.id

  let set t ?sender ?recipients () =
    let envelope = Envelope.set t.envelope ?sender ?recipients () in
    { t with envelope }
end

module Session = struct
  type t =
    { id : string
    ; remote : Host_and_port.t
    ; local : Host_and_port.t
    ; helo : string option
    ; tls : Ssl.Connection.t option
    } with sexp_of, fields

  let create ?id ~remote ~local ?helo ?tls () =
    let id = match id with
      | Some id -> id
      | None -> Uuid.create () |> Uuid.to_string
    in
    { id; remote; local; helo; tls; }

  let cleanup t =
    match t.tls with
    | None -> return (Ok ())
    | Some tls ->
      Ssl.Connection.close tls;
      Ssl.Connection.closed tls
end

module Command = struct
  type t =
    | Hello of string
    | Extended_hello of string
    | Sender of string
    | Recipient of string
    | Data
    | Reset
    | Quit
    | Help
    | Noop
    | Start_tls

  let of_string = function
    | str when String.is_prefix str ~prefix:"HELO " ->
      Hello (String.drop_prefix str 5 |> String.lstrip)
    | str when String.is_prefix str ~prefix:"EHLO " ->
      Extended_hello (String.drop_prefix str 5 |> String.lstrip)
    | str when String.is_prefix str ~prefix:"MAIL FROM:" ->
      Sender (String.drop_prefix str 10 |> String.lstrip)
    | str when String.is_prefix str ~prefix:"RCPT TO:" ->
      Recipient (String.drop_prefix str 8 |> String.lstrip)
    | "DATA"     -> Data
    | "RESET"    -> Reset
    | "QUIT"     -> Quit
    | "HELP"     -> Help
    | "NOOP"     -> Noop
    | "STARTTLS" -> Start_tls
    | str        -> failwithf "Unrecognized command: %s" str ()

  let to_string = function
    | Hello string -> "HELO " ^ string
    | Extended_hello string -> "EHLO " ^ string
    | Sender string -> "MAIL FROM: " ^ string
    | Recipient string -> "RCPT TO: " ^ string
    | Data -> "DATA"
    | Reset -> "RESET"
    | Quit -> "QUIT"
    | Help -> "HELP"
    | Noop -> "NOOP"
    | Start_tls -> "STARTTLS"
end

module Reply = struct
  type forward_path = string with sexp

  type t =
    (* Ok *)
    | System_status_211 of string
    | Help_214 of string list
    | Service_ready_220 of string
    | Closing_connection_221
    | Ok_completed_250 of string
    | Will_forward_251 of forward_path
    | Will_attempt_252
    | Start_mail_input_354

    (* Transient Errors *)
    | Service_unavailable_421
    | Mailbox_unavailable_450 of string
    | Local_error_451 of string
    | Insufficient_storage_452
    | Unable_to_accommodate_455 of string

    (* Permanent Errors *)
    | Command_not_recognized_500 of string
    | Syntax_error_501 of string
    | Command_not_implemented_502 of string
    | Bad_sequence_of_commands_503 of string
    | Parameter_not_implemented_504 of string
    | Mailbox_unavailable_550 of string
    | User_not_local_551 of string
    | Exceeded_storage_allocation_552
    | Mailbox_name_not_allowed_553 of string
    | Transaction_failed_554 of string
    | From_to_parameters_bad_555 of string
  with sexp

  let my_name = Unix.gethostname ()

  let to_string code =
    let rec to_string_lines acc = function
      | [] -> assert(false)
      | [s] -> ((sprintf "%d %s" code s) :: acc) |> List.rev
      | s::ss ->
        to_string_lines ((sprintf "%d-%s" code s) :: acc) ss
    in
    ksprintf (fun message ->
        String.split_lines message
        |> List.map ~f:String.strip
        |> to_string_lines []
        |> String.concat ~sep:"\n")

  let to_string = function
    | System_status_211 s ->
      to_string 211 "System status: %s" s
    | Help_214 commands ->
      to_string 214 "Commands supported:\n%s" (String.concat ~sep:"\n" commands)
    | Service_ready_220 greeting ->
      to_string 220 "%s" greeting
    | Closing_connection_221 ->
      to_string 221 "%s closing connection" my_name
    | Ok_completed_250 msg ->
      to_string 250 "Ok: %s" msg
    | Will_forward_251 path ->
      to_string 251 "User not local; will forward to %s" path
    | Will_attempt_252 ->
      to_string 252 "Cannot VRFY user, but will accept message and attempt"
    | Start_mail_input_354 ->
      to_string 354 "Enter message, ending with \".\" on a line by iteself"
    | Service_unavailable_421 ->
      to_string 421 "%s Service not available, closing transmission channel" my_name
    | Mailbox_unavailable_450 e ->
      to_string 450 "Mailbox unavailbale: %s" e
    | Local_error_451 e ->
      to_string 451 "Local error: %s" e
    | Insufficient_storage_452 ->
      to_string 452 "Insufficient storage"
    | Unable_to_accommodate_455 e ->
      to_string 455 "Unsable to accomodate: %s" e
    | Command_not_recognized_500 e ->
      to_string 500 "Unrecognized command: %s" e
    | Syntax_error_501 e ->
      to_string 501 "Syntax error in parameters or arguments: %s" e
    | Command_not_implemented_502 e ->
      to_string 502 "Command not implemented: %s" e
    | Bad_sequence_of_commands_503 e->
      to_string 503 "Bad sequence of commands %s" e
    | Parameter_not_implemented_504 e ->
      to_string 504 "Command parameter not implemented: " ^ e
    | Mailbox_unavailable_550 e ->
      to_string 550 "Mailbox unavailable: %s" e
    | User_not_local_551 e ->
      to_string 551 "User not local: %s" e
    | Exceeded_storage_allocation_552 ->
      to_string 552 "Exeeded storage allocation"
    | Mailbox_name_not_allowed_553 e ->
      to_string 553 "Mailbox name not allowed: %s" e
    | Transaction_failed_554 e ->
      to_string 554 "Transaction failed: %s" e
    | From_to_parameters_bad_555 e ->
      to_string 555 "From to To parameters bad: %s" e

  let of_code_message reply_code msg =
    match reply_code with
    | 211 -> System_status_211 msg
    | 214 -> Help_214 (String.split_lines msg |> List.tl_exn)
    | 220 -> Service_ready_220 msg
    | 221 -> Closing_connection_221
    | 250 -> Ok_completed_250 msg
    | 251 -> Will_forward_251 msg
    | 252 -> Will_attempt_252
    | 354 -> Start_mail_input_354

    | 421 -> Service_unavailable_421
    | 450 -> Mailbox_unavailable_450 msg
    | 451 -> Local_error_451 msg
    | 452 -> Insufficient_storage_452
    | 455 -> Unable_to_accommodate_455 msg

    | 500 -> Command_not_recognized_500 msg
    | 501 -> Syntax_error_501 msg
    | 502 -> Command_not_implemented_502 msg
    | 503 -> Bad_sequence_of_commands_503 msg
    | 504 -> Parameter_not_implemented_504 msg
    | 550 -> Mailbox_unavailable_550 msg
    | 551 -> User_not_local_551 msg
    | 552 -> Exceeded_storage_allocation_552
    | 553 -> Mailbox_name_not_allowed_553 msg
    | 554 -> Transaction_failed_554 msg
    | 555 -> From_to_parameters_bad_555 msg
    | x -> failwiths "Invalid reply code" x Int.sexp_of_t

  type partial = string * int * (string list)

  let parse ?partial str =
    let finish ~prefix ~code ~rev_msg =
      let i = String.length prefix in
      let d = String.get str i in
      let rev_msg = (String.slice str (i+1) (String.length str)) :: rev_msg in
      match d with
      | ' ' ->
        let msg = List.rev rev_msg |> String.concat ~sep:"\n" in
        `Done (of_code_message code msg)
      | '-' ->
        `Partial (prefix, code, rev_msg)
      | _ -> assert(false)
    in
    match partial with
    | Some (prefix, code, rev_msg) ->
      if String.is_prefix ~prefix str then
        finish ~prefix ~code ~rev_msg
      else
        assert(false)
    | None ->
      let rec loop i =
        let d = String.get str i in
        if Char.is_digit d then
          loop (i+1)
        else
          let prefix = String.slice str 0 i in
          finish ~prefix ~code:(Int.of_string prefix) ~rev_msg:[]
      in
      loop 0
  ;;

  let of_string str =
    let rec loop ?partial = function
      | [] -> assert(false)
      | s::ss ->
        match parse ?partial s with
        | `Partial partial ->
          loop ~partial ss
        | `Done res ->
          if List.is_empty ss then
            res
          else
            assert(false)
    in
    String.split_lines str |> loop

  let of_bigstring bs =
    of_string (Bigstring.to_string bs)
  ;;

  let code = function
    | System_status_211 _             -> 211
    | Help_214 _                      -> 214
    | Service_ready_220 _             -> 220
    | Closing_connection_221          -> 221
    | Ok_completed_250 _              -> 250
    | Will_forward_251 _              -> 251
    | Will_attempt_252                -> 252
    | Start_mail_input_354            -> 354
    | Service_unavailable_421         -> 421
    | Mailbox_unavailable_450 _       -> 450
    | Local_error_451 _               -> 451
    | Insufficient_storage_452        -> 452
    | Unable_to_accommodate_455 _     -> 455
    | Command_not_recognized_500 _    -> 500
    | Syntax_error_501 _              -> 501
    | Command_not_implemented_502 _   -> 502
    | Bad_sequence_of_commands_503 _  -> 503
    | Parameter_not_implemented_504 _ -> 504
    | Mailbox_unavailable_550 _       -> 550
    | User_not_local_551 _            -> 551
    | Exceeded_storage_allocation_552 -> 552
    | Mailbox_name_not_allowed_553 _  -> 553
    | Transaction_failed_554 _        -> 554
    | From_to_parameters_bad_555 _    -> 555

  let is_ok = function
    | System_status_211 _             -> true
    | Help_214 _                      -> true
    | Service_ready_220 _             -> true
    | Closing_connection_221          -> true
    | Ok_completed_250 _              -> true
    | Will_forward_251 _              -> true
    | Will_attempt_252                -> true
    | Start_mail_input_354            -> true
    | Service_unavailable_421         -> false
    | Mailbox_unavailable_450 _       -> false
    | Local_error_451 _               -> false
    | Insufficient_storage_452        -> false
    | Unable_to_accommodate_455 _     -> false
    | Command_not_recognized_500 _    -> false
    | Syntax_error_501 _              -> false
    | Command_not_implemented_502 _   -> false
    | Bad_sequence_of_commands_503 _  -> false
    | Parameter_not_implemented_504 _ -> false
    | Mailbox_unavailable_550 _       -> false
    | User_not_local_551 _            -> false
    | Exceeded_storage_allocation_552 -> false
    | Mailbox_name_not_allowed_553 _  -> false
    | Transaction_failed_554 _        -> false
    | From_to_parameters_bad_555 _    -> false

  let is_permanent_error = function
    | System_status_211 _             -> false
    | Help_214 _                      -> false
    | Service_ready_220 _             -> false
    | Closing_connection_221          -> false
    | Ok_completed_250 _              -> false
    | Will_forward_251 _              -> false
    | Will_attempt_252                -> false
    | Start_mail_input_354            -> false
    | Service_unavailable_421         -> false
    | Mailbox_unavailable_450 _       -> false
    | Local_error_451 _               -> false
    | Insufficient_storage_452        -> false
    | Unable_to_accommodate_455 _     -> false
    | Command_not_recognized_500 _    -> true
    | Syntax_error_501 _              -> true
    | Command_not_implemented_502 _   -> true
    | Bad_sequence_of_commands_503 _  -> true
    | Parameter_not_implemented_504 _ -> true
    | Mailbox_unavailable_550 _       -> true
    | User_not_local_551 _            -> true
    | Exceeded_storage_allocation_552 -> true
    | Mailbox_name_not_allowed_553 _  -> true
    | Transaction_failed_554 _        -> true
    | From_to_parameters_bad_555 _    -> true

  TEST_MODULE = struct
    let check reply =
      let r = of_string (to_string reply) in
      code r = code reply
    ;;

    TEST = check (System_status_211 "test")
    TEST = check (Service_ready_220 "test")
    TEST = check Closing_connection_221
    TEST = check (Ok_completed_250 "test")
    TEST = check (Will_forward_251 "test")
    TEST = check Will_attempt_252
    TEST = check Start_mail_input_354
    TEST = check Service_unavailable_421
    TEST = check (Mailbox_unavailable_450 "test")
    TEST = check (Local_error_451 "test")
    TEST = check Insufficient_storage_452
    TEST = check (Unable_to_accommodate_455 "test")
    TEST = check (Command_not_recognized_500 "test")
    TEST = check (Syntax_error_501 "test")
    TEST = check (Command_not_implemented_502 "test")
    TEST = check (Bad_sequence_of_commands_503 "test")
    TEST = check (Parameter_not_implemented_504 "test")
    TEST = check (Mailbox_unavailable_550 "test")
    TEST = check (User_not_local_551 "test")
    TEST = check Exceeded_storage_allocation_552
    TEST = check (Mailbox_name_not_allowed_553 "test")
    TEST = check (Transaction_failed_554 "test")
    TEST = check (From_to_parameters_bad_555 "test")

    let check_multiline a b =
      let a' = of_string a in
      Poly.equal a' b

    TEST = check_multiline "250-test1\n250-test2\n250 test3" (Ok_completed_250 "test1\ntest2\ntest3")
  end
end

(* Test parsing of commands to server *)
TEST_MODULE = struct
  let check str comm =
    let c = Command.of_string str in
    Polymorphic_compare.equal c comm

  TEST = check "HELO hi" (Command.Hello "hi")
  TEST = check "EHLO hi" (Command.Extended_hello "hi")
  TEST = check "HELP" Command.Help
  TEST = check "MAIL FROM:hi" (Command.Sender "hi")
  TEST = check "RCPT TO:hi" (Command.Recipient "hi")
  TEST = check "DATA" Command.Data
  TEST = check "QUIT" Command.Quit
  TEST = check "NOOP" Command.Noop
  TEST = check "STARTTLS" Command.Start_tls
end

(* Test to_string and of_string functions for symmetry *)
TEST_MODULE = struct
  let check comm =
    let c = Command.of_string (Command.to_string comm) in
    Polymorphic_compare.equal comm c

  TEST = check (Command.Hello "Helo World!~")
  TEST = check (Command.Extended_hello "Helo World!~")
  TEST = check (Command.Sender "Helo World!~")
  TEST = check (Command.Recipient "Helo World!~")
  TEST = check Command.Data
  TEST = check Command.Quit
  TEST = check Command.Help
  TEST = check Command.Noop
  TEST = check Command.Start_tls
end

module Extension = struct
  type t =
    | Start_tls
    | Other of string
  with sexp

  let of_string str =
    let t =
      match String.uppercase str with
      | "STARTTLS" -> Start_tls
      | _ -> Other str
    in
    t

  let to_string = function
    | Start_tls -> "STARTTLS"
    | Other str -> str
end
