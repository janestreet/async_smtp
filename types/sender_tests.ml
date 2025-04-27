open! Core

module%test _ = struct
  let test_lexing str =
    Sender.of_string_with_arguments_string str
    |> [%sexp_of: (Sender.For_test.t * string) Or_error.t]
    |> print_s
  ;;

  let%expect_test _ =
    test_lexing "foo@bar.com";
    [%expect {| (Ok ((Email ((prefix ()) (local_part foo) (domain (bar.com)))) "")) |}]
  ;;

  let%expect_test _ =
    test_lexing "foo@bar.com suffix";
    [%expect
      {| (Ok ((Email ((prefix ()) (local_part foo) (domain (bar.com)))) " suffix")) |}]
  ;;

  let%expect_test _ =
    test_lexing "<foo@bar.com> suffix";
    [%expect
      {| (Ok ((Email ((prefix ("")) (local_part foo) (domain (bar.com)))) " suffix")) |}]
  ;;

  let%expect_test _ =
    test_lexing "Foo Bar <foo@bar.com> suffix";
    [%expect
      {|
      (Ok
       ((Email ((prefix ("Foo Bar ")) (local_part foo) (domain (bar.com))))
        " suffix"))
      |}]
  ;;

  let%expect_test _ =
    test_lexing "<>";
    [%expect {| (Ok (Null "")) |}]
  ;;

  let%expect_test _ =
    test_lexing "prefix <>";
    [%expect {| (Ok (Null "")) |}]
  ;;

  let%expect_test _ =
    test_lexing "\"Mailer Daemon\" <> AUTH=<>";
    [%expect {| (Ok (Null " AUTH=<>")) |}]
  ;;

  let%expect_test _ =
    test_lexing "<user@domain> AUTH=user@domain";
    [%expect
      {|
      (Ok
       ((Email ((prefix ("")) (local_part user) (domain (domain))))
        " AUTH=user@domain"))
      |}]
  ;;

  let%expect_test _ =
    test_lexing "<double-barrelled-name@domain> AUTH=double-barrelled-name@domain";
    [%expect
      {|
      (Ok
       ((Email
         ((prefix ("")) (local_part double-barrelled-name) (domain (domain))))
        " AUTH=double-barrelled-name@domain"))
      |}]
  ;;
end
