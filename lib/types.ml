open Core.Std
open Async.Std
open Email_message.Std

module Headers = Email_headers

module Email_address = struct
  module Domain = Mimestring.Case_insensitive

  (* [prefix = None] means no brackets. *)
  type t =
    { prefix : string option
    ; local_part : string
    ; domain : Domain.t option
    }
  with fields, sexp, bin_io, compare

  (* Comma-separated list:
     "A, B" <ab@x.com>, "C, D" <cd@x.com>
  *)
  let list_of_string_exn ?default_domain s =
    let module L = Email_address_lexer in
    L.parse_emails (Lexing.from_string s)
    |> List.map ~f:(fun { L. local_part; domain; prefix } ->
      let domain = Option.first_some domain default_domain in
      { local_part; domain; prefix })

  let list_of_string ?default_domain s =
    Or_error.try_with (fun () -> list_of_string_exn ?default_domain s)

  let of_string ?default_domain s =
    let open Or_error.Monad_infix in
    list_of_string ?default_domain s
    >>= function
    | [result] -> Ok result
    | _ -> Or_error.error_string ("Expected single email address: " ^ s)

  let of_string_exn ?default_domain s =
    Or_error.ok_exn (of_string ?default_domain s)

  let compose ~prefix ~address_part =
    match prefix with
    | None -> address_part
    | Some prefix -> sprintf "%s<%s>" prefix address_part

  let to_string t =
    let address_part =
      match t.domain with
      | None -> t.local_part
      | Some domain -> sprintf "%s@%s" t.local_part domain
    in
    compose ~prefix:t.prefix ~address_part

  let list_to_header_value ts =
    String.concat ~sep:",\n\t" (List.map ts ~f:to_string)

  let address_part ?(brackets = false) ?(lowercase_domain = false) t =
    let prefix = if brackets then Some "" else None in
    let domain =
      if not lowercase_domain
      then t.domain
      else Option.map t.domain ~f:String.lowercase
    in
    { t with prefix; domain }

  let address_part_string ?brackets ?lowercase_domain t =
    to_string (address_part ?brackets ?lowercase_domain t)

  let set_address_part t address_part =
    of_string (compose ~prefix:t.prefix ~address_part)

  let set_prefix t prefix =
    { t with prefix }

  TEST_UNIT =
    <:test_result<t>>
      (of_string_exn "local")
      ~expect: { local_part = "local"
               ; domain = None
               ; prefix = None }

  TEST_UNIT =
    <:test_result<t>>
      (of_string_exn "<local>")
      ~expect: { local_part = "local"
               ; domain = None
               ; prefix = Some "" }

  TEST_UNIT =
    <:test_result<t>>
      (of_string_exn " local@janestreet.com ")
      ~expect: { local_part = "local"
               ; domain = Some "janestreet.com"
               ; prefix = None }

  TEST_UNIT =
    <:test_result<t>>
      (of_string_exn " <local@janestreet.com> ")
      ~expect: { local_part = "local"
               ; domain = Some "janestreet.com"
               ; prefix = Some "" }

  TEST_UNIT =
    <:test_result<t>>
      (of_string_exn " John Doe <local> ")
      ~expect: { local_part = "local"
               ; domain = None
               ; prefix = Some "John Doe " }

  TEST_UNIT =
    <:test_result<t>>
      (of_string_exn " John Doe <local@janestreet.com> ")
      ~expect: { local_part = "local"
               ; domain = Some "janestreet.com"
               ; prefix = Some "John Doe " }

  TEST_UNIT =
    <:test_result<t>>
      (of_string_exn " \"Doe, John\" <local@janestreet.com> ")
      ~expect:{ local_part = "local"
              ; domain = Some "janestreet.com"
              ; prefix = Some "\"Doe, John\" " }

  TEST_UNIT =
    <:test_result<t list>> (list_of_string_exn "") ~expect:[]

  TEST_UNIT =
    <:test_result<t list>> (list_of_string_exn "   ") ~expect:[]

  TEST_UNIT =
    <:test_result<t list>>
      (list_of_string_exn " \"Doe, John\" <local@janestreet.com>,
                       \n\t \"Doe, Johnny\" <local@janestreet.com> ")
      ~expect:[ { local_part = "local"
                ; domain = Some "janestreet.com"
                ; prefix = Some "\"Doe, John\" " }
              ; { local_part = "local"
                ; domain = Some "janestreet.com"
                ; prefix = Some "\"Doe, Johnny\" " }]

  TEST_UNIT =
    <:test_result<t list>>
      (list_of_string_exn "x@y.com, \"a@b.com\" <\"mailto:a\"@b.com>")
      ~expect:[ { local_part = "x"
                ; domain = Some "y.com"
                ; prefix = None }
              ; { local_part = "\"mailto:a\""
                ; domain = Some "b.com"
                ; prefix = Some "\"a@b.com\" " } ]

  let must_fail = function
    | Error _ -> ()
    | Ok ts ->
      failwithf "Expected to fail, got %s"
        (Sexp.to_string_hum (<:sexp_of<t list>> ts)) ()

  TEST_UNIT =
    must_fail (list_of_string "mailnull@janestreet.com (Cron Daemon)")

  TEST_UNIT =
     must_fail (list_of_string "a@b.com <a@b.com>")

  TEST_UNIT =
     must_fail (list_of_string "a@@b.com")

  module T = struct
    type nonrec t = t with sexp, bin_io, compare
    let hash = Hashtbl.hash
    let to_string = to_string
    let of_string s = of_string s |> Or_error.ok_exn
  end
  include Hashable.Make(T)
  include Comparable.Make(T)
  include Sexpable.Of_stringable(T)
end

module Sender = struct
  open Or_error.Monad_infix

  type t =
    | Null
    | Email of Email_address.t
  with sexp, bin_io, compare

  let of_string ?default_domain s =
    match String.strip s with
    | "<>" -> Ok Null
    | s ->
      Email_address.of_string ?default_domain s
      >>= fun email ->
      Ok (Email email)

  let to_string = function
    | Null -> "<>"
    | Email email -> Email_address.to_string email

  let map t ~f =
    match t with
    | Null -> Null
    | Email email -> Email (f email)

  module T = struct
    type nonrec t = t with sexp, bin_io, compare

    let to_string = to_string
    let of_string s = of_string s |> Or_error.ok_exn

    let hash = function
      | Null -> Hashtbl.hash Null
      | Email email -> Email_address.hash email
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
      Headers.add headers ~name value)

  let set_header t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.set headers ~name value)

  let add_header_at_bottom t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.add_at_bottom headers ~name value)

  let set_header_at_bottom t ~name ~value =
    modify_headers t ~f:(fun headers ->
        Headers.set_at_bottom headers ~name value)

  let filter_headers t ~f =
    modify_headers t ~f:fun headers ->
      List.filter headers ~f:fun (name, value) ->
        f ~name ~value
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
    } with sexp, fields

  let create ?id ~remote ~local ?helo () =
    let id = match id with
      | Some id -> id
      | None -> Uuid.create () |> Uuid.to_string
    in
    { id; remote; local; helo; }
end

module Command = struct
  include Comm

  let commands = ["HELO";"MAIL";"FROM";"RCPT";"TO";"DATA";"QUIT";"HELP";"NOOP"]

  let of_string_opt str =
    Option.try_with (fun () ->
      match Command_lexer.parse_command (Lexing.from_string str) with
      | Helo s -> Helo (String.lstrip s)
      | Sender s -> Sender (String.lstrip s)
      | Recipient s -> Recipient (String.lstrip s)
      | x -> x)

  let to_string = function
    | Helo string -> "HELO " ^ string
    | Sender string -> "MAIL FROM: " ^ string
    | Recipient string -> "RCPT TO: " ^ string
    | Data -> "DATA"
    | Quit -> "QUIT"
    | Help -> "HELP"
    | Noop -> "NOOP"
end

module Reply = struct
  type forward_path = string with sexp

  type t =
    (* Ok *)
    | System_status_211 of string
    | Help_214
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

  let to_string = function
    | System_status_211 s -> "211 System status: " ^s
    | Help_214 ->
      "214-Commands supported:\n214 "
      ^ (String.concat ~sep:" " Command.commands)
    | Service_ready_220 greeting ->
      "220 "^ greeting
    | Closing_connection_221 -> "221 " ^ my_name ^ " closing connection"
    | Ok_completed_250 msg -> "250 Ok: " ^ msg
    | Will_forward_251 path -> "251 User not local; will forward to " ^ path
    | Will_attempt_252 ->
      "252 Cannot VRFY user, but will accept message and attempt"
    | Start_mail_input_354 ->
      "354 Enter message, ending with \".\" on a line by itself"
    | Service_unavailable_421 ->
      "421 " ^ my_name ^ " Service not available, closing transmission channel"
    | Mailbox_unavailable_450 e -> "450 Mailbox unavailable: " ^ e
    | Local_error_451 e -> "451 Local error: " ^ e
    | Insufficient_storage_452 -> "452 Insufficient storage"
    | Unable_to_accommodate_455 e -> "455 Unable to accomodate: " ^ e
    | Command_not_recognized_500 e -> "500 unrecognized command: " ^e
    | Syntax_error_501 e -> "501 Syntax error in parameters or arguments: " ^ e
    | Command_not_implemented_502 e -> "502 Command not implemented: " ^ e
    | Bad_sequence_of_commands_503 e-> "503 Bad sequence of commands: " ^ e
    | Parameter_not_implemented_504 e ->
      "504 Command parameter not implemented: " ^ e
    | Mailbox_unavailable_550 e -> "550 Mailbox unavailable: " ^ e
    | User_not_local_551 e -> "551 User not local: " ^ e
    | Exceeded_storage_allocation_552 -> "552 Exceeded storage allocation"
    | Mailbox_name_not_allowed_553 e -> "553 Mailbox name not allowed: " ^ e
    | Transaction_failed_554 e -> "554 Transaction failed: " ^ e
    | From_to_parameters_bad_555 e -> "555 From to To parameters bad: " ^ e

  let of_string s =
    match Option.try_with (fun () -> String.sub ~pos:0 ~len:3 s) with
    | None -> failwiths "Reply line too short" s String.sexp_of_t
    | Some substring -> begin
        match Option.try_with (fun () -> Int.of_string substring) with
        | None -> failwiths "Unexpected start character" s String.sexp_of_t
        | Some reply_code -> begin
            let msg = String.drop_prefix s 4 in
            match reply_code with
            | 211 -> System_status_211 msg
            | 214 -> Help_214
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
          end
      end
  ;;

  let of_bigstring bs =
    of_string (Bigstring.to_string bs)
  ;;

  let code = function
    | System_status_211 _             -> 211
    | Help_214                        -> 214
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
    | Help_214                        -> true
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
    | Help_214                        -> false
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
    TEST = check Help_214
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
  end
end

(* Test parsing of commands to server *)
TEST_MODULE = struct
  let check str comm =
    let com = Command.of_string_opt str in
    match com with
    | None -> false
    | Some c -> Polymorphic_compare.equal c comm

  TEST = check "HELO hi" (Command.Helo "hi")
  TEST = check "MAIL FROM:hi" (Command.Sender "hi")
  TEST = check "RCPT TO:hi" (Command.Recipient "hi")
  TEST = check "DATA" Command.Data
  TEST = check "QUIT" Command.Quit
  TEST = check "HELP" Command.Help
  TEST = check "NOOP" Command.Noop
end

(* Test to_string and of_string_opt functions for symmetry *)
TEST_MODULE = struct
  let check comm =
    match Command.of_string_opt (Command.to_string comm) with
    | None -> false
    | Some c -> Polymorphic_compare.equal comm c

  TEST = check (Command.Helo "Helo World!~")
  TEST = check (Command.Sender "Helo World!~")
  TEST = check (Command.Recipient "Helo World!~")
  TEST = check Command.Data
  TEST = check Command.Quit
  TEST = check Command.Help
  TEST = check Command.Noop
end
