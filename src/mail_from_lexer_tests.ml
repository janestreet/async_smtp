open! Core

let%test_module _ =
  (module struct
    let test_lexing str = Mail_from_lexer.parse_mail_from (Lexing.from_string str)

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "foo@bar.com")
        ~expect: { prefix = None
                 ; sender = `Email { local_part = "foo"; domain = Some "bar.com" }
                 ; suffix = "" }

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "foo@bar.com suffix")
        ~expect: { prefix = None
                 ; sender = `Email { local_part = "foo"; domain = Some "bar.com" }
                 ; suffix = " suffix" }

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "<foo@bar.com> suffix")
        ~expect: { prefix = Some ""
                 ; sender = `Email { local_part = "foo"; domain = Some "bar.com" }
                 ; suffix = " suffix" }

    let%test_unit _ =
      [%test_result: Mail_from_lexer_types.email_with_suffix]
        (test_lexing "Foo Bar <foo@bar.com> suffix")
        ~expect: { prefix = Some "Foo Bar "
                 ; sender = `Email { local_part = "foo"; domain = Some "bar.com" }
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
