open! Core
open! Async
open! Async_smtp.Smtp_expect_test_helper
open! Expect_test_helpers_core
open! Expect_test_helpers_async

let%expect_test "Smtp_expect_test_helper.smtp" =
  let%bind () = smtp [ envelope () ] in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250 8BITMIME
    > MAIL FROM: <sender@example.com>
    < 250 Ok: continue
    > RCPT TO: <recipient@example.com>
    < 250 Ok: continue
    > DATA
    < 354 Enter message, ending with "." on a line by itself
    > Subject: TEST EMAIL
    >
    > TEST EMAIL
    > .
    < 554 Transaction failed: Message processing not implemented
    > RSET
    < 250 Ok: continue
    > QUIT
    < 221 closing connection
           |}]
;;

let%test_unit "Smtp_expect_test_helper.manual_client" =
  Thread_safe.block_on_async_exn (fun () ->
    manual_client (fun ~client ~server ->
      let%bind () = server "220 [SMTP TEST SERVER]" in
      let%bind () = client "EHLO custom client commands" in
      let%bind () = server "250-Ok: Continue, extensions follow:\n250 8BITMIME" in
      let%bind () = client "MAIL FROM: <sender@example.com>" in
      let%bind () = server "250 Ok: continue" in
      let%bind () = client "RCPT TO: <recipient@example.com>" in
      let%bind () = server "250 Ok: continue" in
      let%bind () = client "DATA" in
      let%bind () =
        server "354 Enter message, ending with \".\" on a line by itself"
      in
      let%bind () = client "." in
      let%bind () = server "250 Ok: id=SENT-1" in
      let%bind () = client "QUIT" in
      server "221 closing connection"))
;;

let%test_unit "Smtp_expect_test_helper.manual_server" =
  Thread_safe.block_on_async_exn (fun () ->
    manual_server [ envelope () ] (fun ~client ~server ->
      let%bind () = server "220 custom server commands" in
      let%bind () = client "EHLO [SMTP TEST CLIENT]" in
      let%bind () = server "250 continue" in
      let%bind () = client "MAIL FROM: <sender@example.com>" in
      let%bind () = server "250 continue" in
      let%bind () = client "RCPT TO: <recipient@example.com>" in
      let%bind () = server "250 continue" in
      let%bind () = client "DATA" in
      let%bind () = server "354 transmit data followed by ." in
      let%bind () = client "Subject: TEST EMAIL\n\nTEST EMAIL\n." in
      let%bind () = server "250 ok" in
      let%bind () = client "RSET" in
      let%bind () = server "250 continue" in
      let%bind () = client "QUIT" in
      server "221 goodbye"))
;;
