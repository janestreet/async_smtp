open! Core
open! Async
open! Async_smtp.Smtp_expect_test_helper
open! Expect_test_helpers_core
open! Expect_test_helpers_async

let%expect_test ("Smtp_expect_test_helper.smtp" [@tags "disabled"]) =
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
    |}];
  return ()
;;

let%expect_test ("Smtp_expect_test_helper.manual_client" [@tags "disabled"]) =
  manual_client (fun ~client ~server ~expect_server_close ->
    let%bind () = server "220 [SMTP TEST SERVER]" in
    let%bind () = client "EHLO custom client commands" in
    let%bind () = server "250-Ok: Continue, extensions follow:\n250 8BITMIME" in
    let%bind () = client "MAIL FROM: <sender@example.com>" in
    let%bind () = server "250 Ok: continue" in
    let%bind () = client "RCPT TO: <recipient@example.com>" in
    let%bind () = server "250 Ok: continue" in
    let%bind () = client "DATA" in
    let%bind () = server "354 Enter message, ending with \".\" on a line by itself" in
    let%bind () = client "." in
    (* Server does not implement [Envelope.process]. *)
    let%bind () = server "554 Transaction failed: Message processing not implemented" in
    let%bind () = client "QUIT" in
    let%bind () = server "221 closing connection" in
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 1.0) in
    let%bind () = expect_server_close () in
    return ())
;;

let%expect_test ("Smtp_expect_test_helper.manual_server" [@tags "disabled"]) =
  manual_server
    [ envelope () ]
    (fun ~client ~server ~expect_client_close ->
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
      let%bind () = server "221 goodbye" in
      let%bind () = Clock_ns.after (Time_ns.Span.of_sec 1.0) in
      let%bind () = expect_client_close () in
      return ())
;;

let%expect_test ("server-side command timeout" [@tags "disabled"]) =
  manual_client (fun ~client ~server ~expect_server_close ->
    let%bind () = server "220 [SMTP TEST SERVER]" in
    let%bind () = client "EHLO custom client commands" in
    let%bind () = server "250-Ok: Continue, extensions follow:\n250 8BITMIME" in
    let%bind () = client "MAIL FROM: <sender@example.com>" in
    let%bind () = server "250 Ok: continue" in
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 2.0) in
    let%bind () = server "421 SMTP command timeout, closing transmission channel" in
    let%bind () = expect_server_close () in
    return ())
;;

let%expect_test ("server-side data timeout" [@tags "disabled"]) =
  manual_client (fun ~client ~server ~expect_server_close ->
    let%bind () = server "220 [SMTP TEST SERVER]" in
    let%bind () = client "EHLO custom client commands" in
    let%bind () = server "250-Ok: Continue, extensions follow:\n250 8BITMIME" in
    let%bind () = client "MAIL FROM: <sender@example.com>" in
    let%bind () = server "250 Ok: continue" in
    let%bind () = client "RCPT TO: <recipient@example.com>" in
    let%bind () = server "250 Ok: continue" in
    let%bind () = client "DATA" in
    let%bind () = server "354 Enter message, ending with \".\" on a line by itself" in
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 2.0) in
    let%bind () = server "421 SMTP incoming data timeout, closing transmission channel" in
    let%bind () = expect_server_close () in
    return ())
;;
