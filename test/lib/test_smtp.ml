open! Core
open! Async
open! Async_smtp.Smtp_expect_test_helper
open! Expect_test_helpers

let%expect_test "Smtp_expect_test_helper.smtp" =
  smtp
    [ envelope () ]
  >>= fun () ->
  [%expect {|
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
    < 250 Ok: id=SENT-1
    > RSET
    < 250 Ok: continue
    > QUIT
    < 221 closing connection
           |}]

let%test_unit "Smtp_expect_test_helper.manual_client" =
  Thread_safe.block_on_async_exn (fun () ->
    manual_client
      (fun ~client ~server ->
         server "220 [SMTP TEST SERVER]"
         >>= fun () ->
         client "EHLO custom client commands"
         >>= fun () ->
         server "250-Ok: Continue, extensions follow:\n\
                 250 8BITMIME"
         >>= fun () ->
         client "MAIL FROM: <sender@example.com>"
         >>= fun () ->
         server "250 Ok: continue"
         >>= fun () ->
         client "RCPT TO: <recipient@example.com>"
         >>= fun () ->
         server "250 Ok: continue"
         >>= fun () ->
         client "DATA"
         >>= fun () ->
         server "354 Enter message, ending with \".\" on a line by itself"
         >>= fun () ->
         client "."
         >>= fun () ->
         server "250 Ok: id=SENT-1"
         >>= fun () ->
         client "QUIT"
         >>= fun () ->
         server "221 closing connection"
      ))

let%test_unit "Smtp_expect_test_helper.manual_server" =
  Thread_safe.block_on_async_exn (fun () ->
    manual_server
      [ envelope () ]
      (fun ~client ~server ->
         server "220 custom server commands"
         >>= fun () ->
         client "EHLO [SMTP TEST CLIENT]"
         >>= fun () ->
         server "250 continue"
         >>= fun () ->
         client "MAIL FROM: <sender@example.com>"
         >>= fun () ->
         server "250 continue"
         >>= fun () ->
         client "RCPT TO: <recipient@example.com>"
         >>= fun () ->
         server "250 continue"
         >>= fun () ->
         client "DATA"
         >>= fun () ->
         server "354 transmit data followed by ."
         >>= fun () ->
         client "Subject: TEST EMAIL\n\
                 \n\
                 TEST EMAIL\n\
                 ."
         >>= fun () ->
         server "250 ok"
         >>= fun () ->
         client "RSET"
         >>= fun () ->
         server "250 continue"
         >>= fun () ->
         client "QUIT"
         >>= fun () ->
         server "221 goodbye"
      ))
