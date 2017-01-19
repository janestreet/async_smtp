open Core.Std
open Core_extended.Std
open Async.Std
open Async_ssl.Std
open Email_message.Std

module Headers = Email_headers

module Email_address = Email_address

module Smtp_extension = struct
  type t =
    | Start_tls
    | Auth_login
    | Mime_8bit_transport
    | Other of string
  [@@deriving sexp]

  let of_string str =
    let t =
      match String.uppercase str with
      | "STARTTLS" -> Start_tls
      | "AUTH LOGIN" -> Auth_login
      | "8BITMIME" -> Mime_8bit_transport
      | str -> Other str
    in
    t

  let to_string = function
    | Start_tls -> "STARTTLS"
    | Auth_login -> "AUTH LOGIN"
    | Mime_8bit_transport -> "8BITMIME"
    | Other str -> str
end

module Argument = struct
  open Or_error.Monad_infix
  type t =
    | Auth of Email_address.t option
    | Body of [`Mime_8bit | `Mime_7bit ]
  [@@deriving bin_io, sexp, compare]

  let of_string = function
    | "AUTH=<>" -> Ok (Auth None)
    | str when String.is_prefix str ~prefix:"AUTH=" -> begin
        let email_address = (String.drop_prefix str 5 |> String.strip) in
        match Email_address.of_string email_address with
        | Ok email_address -> Ok (Auth (Some email_address))
        | Error _ ->
          Log.Global.info "Unparsable argument to AUTH: %s" email_address;
          Ok (Auth None)
      end
    | "BODY=8BITMIME" -> Ok (Body `Mime_8bit)
    | "BODY=7BIT" -> Ok (Body `Mime_7bit)
    | str -> Or_error.errorf "Unrecognized extension to mail command: %s" str

  let to_string = function
    | Body `Mime_8bit -> "BODY=8BITMIME"
    | Body `Mime_7bit -> "BODY=7BIT"
    | Auth email_address ->
      match email_address with
      | None -> "AUTH=<>"
      | Some email_address -> "AUTH=" ^ (Email_address.to_string email_address)

  let to_smtp_extension = function
    | Auth _ -> Smtp_extension.Auth_login
    | Body _ -> Smtp_extension.Mime_8bit_transport

  let list_of_string ~allowed_extensions str =
    String.split ~on:' ' str
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:of_string
    |> Or_error.all
    >>= fun args ->
    let has_invalid_arg =
      List.exists args ~f:(fun arg ->
        not (List.mem allowed_extensions (to_smtp_extension arg)))
    in
    if has_invalid_arg then
      Or_error.errorf "Unable to parse MAIL FROM arguments: %s" str
    else
      Ok args


end

module Sender = struct
  open Or_error.Monad_infix
  type t =
    [ `Null
    | `Email of Email_address.t
    ]
  [@@deriving bin_io, sexp, compare]

  let of_string_with_arguments ?default_domain ~allowed_extensions str =
    Or_error.try_with (fun () -> Mail_from_lexer.parse_mail_from (Lexing.from_string str))
    |> Or_error.tag ~tag:(sprintf "Failed to parse [Sender.t] from \"%s\"" str)
    >>= fun mail_from  ->
    Argument.list_of_string ~allowed_extensions mail_from.suffix
    >>= fun all_args ->
    match mail_from.sender with
    | `Null -> Ok (`Null, all_args)
    | `Email email ->
      let domain = Option.first_some email.domain default_domain in
      let email_address =
        Email_address.create ?prefix:mail_from.prefix ?domain email.local_part
      in
      Ok (`Email email_address, all_args)

  let of_string ?default_domain str =
    of_string_with_arguments ?default_domain ~allowed_extensions:[] str
    >>| function
    | email,[] -> email
    | _,(_::_) -> failwithf "impossible, unexpected extension arguments" ()

  let to_string = function
    | `Null -> "<>"
    | `Email email -> Email_address.to_string email

  let to_string_with_arguments (sender,args) =
    to_string sender :: List.map args ~f:Argument.to_string
    |> String.concat ~sep:" "

  let map t ~f =
    match t with
    | `Null -> t
    | `Email email -> `Email (f email)

  module T = struct
    type nonrec t = t [@@deriving sexp, bin_io, compare]

    let to_string = to_string
    let of_string s = of_string s |> Or_error.ok_exn

    let compare a b = match a,b with
      | `Null, `Null -> 0
      | `Email a, `Email b -> Email_address.compare a b
      | `Null, `Email _ -> -1
      | `Email _, `Null -> 1

    let hash = function
      | `Null -> Hashtbl.hash `Null
      | `Email email -> Email_address.hash email
  end
  include Hashable.Make(T)
  include Comparable.Make(T)
  include Sexpable.Of_stringable(T)

  module Caseless = struct
    module T = struct
      type nonrec t = t [@@deriving bin_io, sexp]
      let compare a b = match a,b with
        | `Null, `Null -> 0
        | `Email a, `Email b -> Email_address.Caseless.compare a b
        | `Null, `Email _ -> -1
        | `Email _, `Null -> 1
      let hash = function
        | `Null -> 0
        | `Email a -> Email_address.Caseless.hash a
    end
    include T
    include Comparable.Make(T)
    include Hashable.Make(T)
  end
end

(* Test mail_from_lexer.mll *)
let%test_module _ =
  (module struct
    let test_lexing str = Mail_from_lexer.parse_mail_from (Lexing.from_string str)

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "todd@lubin.us")
        ~expect: { prefix = None
                 ; sender = `Email { local_part = "todd"; domain = Some "lubin.us" }
                 ; suffix = "" }

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "todd@lubin.us suffix")
        ~expect: { prefix = None
                 ; sender = `Email { local_part = "todd"; domain = Some "lubin.us" }
                 ; suffix = " suffix" }

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "<todd@lubin.us> suffix")
        ~expect: { prefix = Some ""
                 ; sender = `Email { local_part = "todd"; domain = Some "lubin.us" }
                 ; suffix = " suffix" }

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "Todd Lubin <todd@lubin.us> suffix")
        ~expect: { prefix = Some "Todd Lubin "
                 ; sender = `Email { local_part = "todd"; domain = Some "lubin.us" }
                 ; suffix = " suffix" }

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "<>")
        ~expect: { prefix = Some ""
                 ; sender = `Null
                 ; suffix = "" }

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "prefix <>")
        ~expect: { prefix = Some "prefix "
                 ; sender = `Null
                 ; suffix = "" }

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "\"Mailer Daemon\" <> AUTH=<>")
        ~expect: { prefix = Some "\"Mailer Daemon\" "
                 ; sender = `Null
                 ; suffix = " AUTH=<>" }
  end)

(* Test parsing of commands to server *)
let%test_module _ =
  (module struct
    let check str extn =
      let e = Argument.of_string str |> Or_error.ok_exn in
      Polymorphic_compare.equal e extn

    let%test _ = check "AUTH=<>" (Argument.Auth None)
    let%test _ = check "AUTH=<hello@world>" (Argument.Auth (Some (Email_address.of_string_exn "<hello@world>")))
  end)

(* Test to_string and of_string functions for symmetry *)
let%test_module _ =
  (module struct
    let check extn =
      let e = Argument.of_string (Argument.to_string extn) |> Or_error.ok_exn in
      Polymorphic_compare.equal extn e

    let%test _ = check (Argument.Auth None)
    let%test _ = check (Argument.Auth (Some (Email_address.of_string_exn "<hello@world>")))
  end)

let%test_module _ =
  (module struct
    let check ~should_fail allowed_extensions str =
      match Sender.of_string_with_arguments ~allowed_extensions str with
      | Ok mail_from ->
        not should_fail &&
        String.equal str (Sender.to_string_with_arguments mail_from)
      | Error _ -> should_fail

    let%test _ = check ~should_fail:false [] "todd@lubin.us"
    let%test _ = check ~should_fail:false [] "<>"
    let%test _ = check ~should_fail:true [] "<> <>"
    let%test _ = check ~should_fail:false [Auth_login] "<> AUTH=<>"
    let%test _ = check ~should_fail:false [Auth_login] "todd lubin <todd@lubin.us> AUTH=<>"
    let%test _ = check ~should_fail:false [Auth_login] "<todd@lubin.us> AUTH=foobar"
    let%test _ = check ~should_fail:false [] "<todd@lubin.us>"
    let%test _ = check ~should_fail:true [] "<todd@lubin.us> AUTH=foobar"
    let%test _ = check ~should_fail:true [Auth_login] "<todd@lubin.us> FOOBAR=foobar"
  end)

let urlbase64_encode_float ?(length=6) f =
  match Int64.of_float f with
  | exception _ -> invalid_arg "cannot encode a float that does not fit in an Int64"
  | n ->
    String.init 9 ~f:(fun i ->
      Int64.shift_right n (64 - 8*(i))
      |> Int64.bit_and 0xffL
      |> Int64.to_int_exn
      |> Char.of_int_exn)
    |> Base64.Websafe.encode
    |> String.sub ~pos:(12-length) ~len:length
;;

let%test_module "urlbase64_encode_float" =
  (module struct
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float 1234.1235453123); [%expect_exact {|AAAATS|}]
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float 1234.); [%expect_exact {|AAAATS|}]
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float 1235.); [%expect_exact {|AAAATT|}]
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float 123456.); [%expect_exact {|AAAeJA|}]
    let%expect_test _ =
      printf "%s" (urlbase64_encode_float Int64.(to_float (max_value - 1024L))); [%expect_exact {|____wA|}]
  end)

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
      let encode = urlbase64_encode_float in
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
      ; sender_args       : Argument.t sexp_list
      ; recipients : Email_address.t list
      ; rejected_recipients : Email_address.t list
      ; id         : Id.t
      ; email      : Email.t
      } [@@deriving fields, sexp, bin_io, compare]
    ;;

    let compare t1 t2 =
      let t1 = { t1 with id = "" } in
      let t2 = { t2 with id = "" } in
      compare t1 t2

    let hash { sender; sender_args; recipients; rejected_recipients; id = _; email } =
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
        sender_args,
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
        { sender; sender_args; id; email; recipients; rejected_recipients }
        ?(sender = sender)
        ?(sender_args = sender_args)
        ?(recipients = recipients)
        ?(rejected_recipients=rejected_recipients)
        ?(email = email)
        () =
    { sender; sender_args; id; email; recipients; rejected_recipients }

  let create ?id ~sender ?(sender_args=[]) ~recipients ?(rejected_recipients=[]) ~email () =
    let id = match id with
      | Some id -> id
      | None -> Id.create ()
    in
    Fields.create ~sender ~sender_args ~recipients ~rejected_recipients ~email ~id
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

  let last_header ?whitespace t name =
    Email.last_header ?whitespace (email t) name

  let find_all_headers ?whitespace t name =
    Email.find_all_headers ?whitespace (email t) name

  let modify_email t ~f =
    let email = email t in
    let email = f email in
    { t with email }

  let modify_headers t ~f =
    modify_email t ~f:(fun email ->
      Email.modify_headers email ~f)

  let add_header ?whitespace t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.add ?whitespace headers ~name ~value)

  let add_headers ?whitespace t ts =
    modify_headers t ~f:(fun headers ->
      Headers.add_all ?whitespace headers ts)

  let set_header ?whitespace t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.set ?whitespace headers ~name ~value)

  let add_header_at_bottom ?whitespace t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.add_at_bottom ?whitespace headers ~name ~value)

  let add_headers_at_bottom ?whitespace t ts =
    modify_headers t ~f:(fun headers ->
      Headers.add_all_at_bottom ?whitespace headers ts)

  let set_header_at_bottom ?whitespace t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.set_at_bottom ?whitespace headers ~name ~value)

  let filter_headers ?whitespace t ~f =
    modify_headers t ~f:(fun headers ->
      Headers.filter ?whitespace headers ~f)

  let map_headers ?whitespace t ~f =
    modify_headers t ~f:(fun headers ->
      Headers.map ?whitespace headers ~f)
end

module Address = struct
  type t = [`Inet of Host_and_port.t | `Unix of string] [@@deriving sexp, compare, bin_io]

  let to_string t = sexp_of_t t |> Sexp.to_string
  let of_string str = Sexp.of_string str |> t_of_sexp
end

module Envelope_with_next_hop = struct
  module T = struct
    type t =
      { envelope : Envelope.t
      ; next_hop_choices : Address.t list
      ; retry_intervals : Time.Span.t list
      } [@@deriving fields, sexp, bin_io, compare]

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
  let sender_args       = act_on_envelope Envelope.sender_args
  let string_sender     = act_on_envelope Envelope.string_sender
  let recipients        = act_on_envelope Envelope.recipients
  let string_recipients = act_on_envelope Envelope.string_recipients
  let email             = act_on_envelope Envelope.email
  let id                = act_on_envelope Envelope.id

  let set t ?sender ?sender_args ?recipients () =
    let envelope = Envelope.set t.envelope ?sender ?sender_args ?recipients () in
    { t with envelope }
end

module Session = struct
  type t =
    { remote : Address.t
    ; local : Address.t
    ; helo : string option
    ; tls : Ssl.Connection.t option
    ; authenticated : string option
    ; advertised_extensions : Smtp_extension.t list
    } [@@deriving sexp_of, fields]

  let create ~remote ~local ?helo ?tls ?authenticated ?(advertised_extensions=[]) () =
    { remote; local; helo; tls; authenticated; advertised_extensions; }

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
    | Auth_login of string option
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
    | str when String.Caseless.is_prefix str ~prefix:"AUTH LOGIN " ->
      Auth_login (Some (String.drop_prefix str 11 |> String.lstrip))
    | str when String.Caseless.equal str "AUTH LOGIN" -> Auth_login None
    | str when String.Caseless.equal str "DATA"     -> Data
    | str when String.Caseless.equal str "RESET"    -> Reset
    | str when String.Caseless.equal str "QUIT"     -> Quit
    | str when String.Caseless.equal str "HELP"     -> Help
    | str when String.Caseless.equal str "NOOP"     -> Noop
    | str when String.Caseless.equal str "STARTTLS" -> Start_tls
    | str -> failwithf "Unrecognized command: %s" str ()

  let to_string = function
    | Hello string -> "HELO " ^ string
    | Extended_hello string -> "EHLO " ^ string
    | Sender string -> "MAIL FROM: " ^ string
    | Recipient string -> "RCPT TO: " ^ string
    | Auth_login arg ->
      "AUTH LOGIN" ^ (Option.value_map arg ~default:"" ~f:(fun arg -> " " ^ arg))
    | Data -> "DATA"
    | Reset -> "RESET"
    | Quit -> "QUIT"
    | Help -> "HELP"
    | Noop -> "NOOP"
    | Start_tls -> "STARTTLS"

  (* Test parsing of commands to server *)
  let%test_unit _ =
    let check a b = [%test_eq:t] (of_string a) b in
    Variants.iter
      ~hello:(fun _ ->
        check "HELO hi" (Hello "hi");
        check "helo hi" (Hello "hi")
      )
      ~extended_hello:(fun _ ->
        check "EHLO hi" (Extended_hello "hi");
        check "ehlo hi" (Extended_hello "hi")
      )
      ~help:(fun _ ->
        check "HELP" Help;
        check "help" Help
      )
      ~sender:(fun _ ->
        check "MAIL FROM:hi" (Sender "hi");
        check "mail from:hi" (Sender "hi")
      )
      ~recipient:(fun _ ->
        check "RCPT TO:hi" (Recipient "hi");
        check "rcpt to:hi" (Recipient "hi")
      )
      ~auth_login:(fun _ ->
        check "AUTH LOGIN" (Auth_login None);
        check "AUTH LOGIN foobar" (Auth_login (Some "foobar"))
      )
      ~data:(fun _ ->
        check "DATA" Data;
        check "data" Data
      )
      ~reset:(fun _ ->
        check "RESET" Reset;
        check "reset" Reset
      )
      ~quit:(fun _ ->
        check "QUIT" Quit;
        check "quit" Quit
      )
      ~noop:(fun _ ->
        check "NOOP" Noop;
        check "noop" Noop
      )
      ~start_tls:(fun _ ->
        check "STARTTLS" Start_tls;
        check "starttls" Start_tls
      )

  (* Test to_string and of_string functions for symmetry *)
  let%test_unit _ =
    let check c = [%test_eq:t] c (of_string (to_string c)) in
    Variants.iter
      ~hello:(fun _ ->
        check (Hello "Helo World!~")
      )
      ~extended_hello:(fun _ ->
        check (Extended_hello "Helo World!~")
      )
      ~help:(fun _ ->
        check Help
      )
      ~sender:(fun _ ->
        check (Sender "Helo World!~")
      )
      ~recipient:(fun _ ->
        check (Recipient "Helo World!~")
      )
      ~auth_login:(fun _ ->
        check (Auth_login None);
        check (Auth_login (Some "foobar"))
      )
      ~data:(fun _ ->
        check Data
      )
      ~reset:(fun _ ->
        check Reset
      )
      ~quit:(fun _ ->
        check Quit
      )
      ~noop:(fun _ ->
        check Noop
      )
      ~start_tls:(fun _ ->
        check Start_tls
      )

  (* Mechanical sanity checks *)
  let%test_unit _ =
    let check_to_str a b = [%test_eq:string] a (to_string b) in
    let check_of_str a b = [%test_eq:t] a (of_string b) in
    let check_round a b =
      check_to_str a b;
      check_of_str b a;
      check_to_str a (of_string a);
      check_of_str b (to_string b)
    in
    Variants.iter
      ~hello:(fun _ ->
        check_round "HELO Helo World!~" (Hello "Helo World!~");
        check_of_str (Hello "Helo World!~") "helo Helo World!~"
      )
      ~extended_hello:(fun _ ->
        check_round "EHLO Helo World!~" (Extended_hello "Helo World!~");
        check_of_str (Extended_hello "Helo World!~") "ehlo Helo World!~"
      )
      ~sender:(fun _ ->
        check_round "MAIL FROM: Helo World!~" (Sender "Helo World!~");
        check_of_str (Sender "Helo World!~") "mail from: Helo World!~"
      )
      ~recipient:(fun _ ->
        check_round "RCPT TO: Helo World!~" (Recipient "Helo World!~");
        check_of_str (Recipient "Bye World!~") "RCPT TO:Bye World!~";
        check_of_str (Recipient "Bye World!~") "rcpt to:Bye World!~")
      ~auth_login:(fun _ ->
        check_round "AUTH LOGIN foobar" (Auth_login (Some "foobar"));
        check_round "AUTH LOGIN" (Auth_login None)
      )
      ~data:(fun _ ->
        check_round "DATA" Data;
        check_of_str Data "data"
      )
      ~reset:(fun _ ->
        check_round "RESET" Reset;
        check_of_str Data "data"
      )
      ~quit:(fun _ ->
        check_round "QUIT" Quit;
        check_of_str Quit "quit"
      )
      ~help:(fun _ ->
        check_round "HELP" Help;
        check_of_str Help "help"
      )
      ~noop:(fun _ ->
        check_round "NOOP" Noop;
        check_of_str Noop "noop")
      ~start_tls:(fun _ ->
        check_round "STARTTLS" Start_tls;
        check_of_str Start_tls "starttls")
end

module Reply = struct
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
      ] [@@deriving sexp, enumerate]

    type t = int t_ [@@deriving sexp]

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

    let all = lazy begin
      List.range 100 999
      |> List.filter ~f:(fun i -> of_int i = `Other i)
      |> all_of_t_
    end

    (* Check that every int maps uniquely to a code, and back to itself *)
    let%test_unit _ =
      List.range 100 999
      |> List.iter ~f:([%test_pred:int] (fun i -> to_int (of_int i) = i))

    (* Check that every code maps to an int and back to itself *)
    let%test_unit _ =
      Lazy.force all
      |> List.iter ~f:([%test_pred:t] (fun c -> of_int (to_int c) = c))
  end

  type t =
    { code : Code.t
    ; raw_message : string list
    }

  include Sexpable.Of_sexpable(struct
      type t = int * string list [@@deriving sexp]
    end )(struct
      type nonrec t = t
      let of_sexpable (code, raw_message) = { code = Code.of_int code; raw_message }
      let to_sexpable { code; raw_message } = (Code.to_int code, raw_message)
    end)

  let code t = Code.to_int t.code

  let my_name = Unix.gethostname ()

  let create code fmt =
    ksprintf (fun raw_message ->
      { code; raw_message = String.split_lines raw_message }) fmt

  let service_ready_220 greeting =
    create `Service_ready_220 "%s" greeting

  let closing_connection_221 =
    create `Closing_connection_221 "%s closing connection" my_name

  let authentication_successful_235 =
    create `Authentication_successful_235 "Authentication successful"

  let ok_completed_250 msg =
    create `Ok_completed_250 "Ok: %s" msg

  let start_authentication_input_334 msg =
    create `Start_authentication_input_334 "%s" msg

  let start_mail_input_354 =
    create `Start_mail_input_354 "Enter message, ending with \".\" on a line by itself"

  let service_unavailable_421 =
    create `Service_unavailable_421 "%s Service not available, closing transmission channel" my_name

  let local_error_451 msg =
    create `Local_error_451 "Local error: %s" msg

  let message_rate_exceeded_452 =
    create `Message_rate_exceeded_452 "Message rate exceeded"

  let unable_to_accommodate_455 msg =
    create `Unable_to_accommodate_455 "Unable to accommodate: %s" msg

  let command_not_recognized_500 command =
    create `Command_not_recognized_500 !"Unrecognized command: %s" command

  let syntax_error_501 error =
    create `Syntax_error_501 "Syntax error in parameters or arguments: %s" error

  let command_not_implemented_502 command =
    create `Command_not_implemented_502 !"Command not implemented: %{Command}" command

  let bad_sequence_of_commands_503 command =
    create `Bad_sequence_of_commands_503 !"Bad sequence of commands: %{Command}" command

  let authentication_required_530 =
    create `Authentication_required_530 "Authentication required."

  let authentication_credentials_invalid_535 =
    create `Authentication_credentials_invalid_535 "Authentication credentials invalid"

  let mailbox_unavailable_550 reason =
    create `Mailbox_unavailable_550 "Mailbox unavailable: %s" reason

  let exceeded_storage_allocation_552 =
    create `Exceeded_storage_allocation_552 "Exceeded storage allocation"

  let transaction_failed_554 message =
    create `Transaction_failed_554 !"Transaction failed: %s" message

  let from_to_parameters_bad_555 msg =
    create `From_to_parameters_bad_555 !"From or To parameters bad: %s" msg

  let to_string t =
    let code = code t in
    let rec to_string_lines acc = function
      | [] -> assert(false)
      | [s] -> ((sprintf "%d %s" code s) :: acc) |> List.rev
      | s::ss ->
        to_string_lines ((sprintf "%d-%s" code s) :: acc) ss
    in
    to_string_lines [] t.raw_message
    |> String.concat ~sep:"\n"

  let of_code_message code raw_message =
    let code = Code.of_int code in
    { code; raw_message }

  type partial = string * int * (string list)

  let parse ?partial str =
    let finish ~prefix ~code ~rev_msg =
      let i = String.length prefix in
      let d = String.get str i in
      let rev_msg = (String.slice str (i+1) (String.length str)) :: rev_msg in
      match d with
      | ' ' ->
        `Done (of_code_message code (List.rev rev_msg))
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

  let is_ok t =
    let code = code t in
    (200 <= code && code <= 299)
    || (300 <= code && code <= 399)

  let is_permanent_error t =
    let code = code t in
    (500 <= code && code <= 599)

  let%test_module _ =
    (module struct
      let check reply =
        [%test_eq:t] reply (of_string (to_string reply))
      ;;

      let%test_unit _ =
        List.iter (Lazy.force Code.all)
          ~f:(fun code ->
            check { code; raw_message = [ "test" ] };
            check { code; raw_message = [ "test0"; "test2"; "test3" ] }
          )

      let%test_unit _ = check (service_ready_220 "test")
      let%test_unit _ = check closing_connection_221
      let%test_unit _ = check (ok_completed_250 "test")
      let%test_unit _ = check start_mail_input_354
      let%test_unit _ = check service_unavailable_421
      let%test_unit _ = check (local_error_451 "test")
      let%test_unit _ = check (command_not_recognized_500 "test")
      let%test_unit _ = check (syntax_error_501 "test")
      let%test_unit _ = check (command_not_implemented_502 (Command.Hello "Test"))
      let%test_unit _ = check (bad_sequence_of_commands_503 (Command.Hello "Test"))
      let%test_unit _ = check exceeded_storage_allocation_552
      let%test_unit _ = check (transaction_failed_554 "test")

      let check_multiline a b =
        [%test_eq:t] b (of_string a)

      let%test_unit _ = check_multiline "250-Ok: test1\n250-test2\n250 test3" (ok_completed_250 "test1\ntest2\ntest3")
    end)
end

module Credentials = struct
  type t =
    { username : string
    ; password : string;
    }
  [@@deriving sexp, fields]

  let create = Fields.create
end
