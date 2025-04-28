open! Core
open Poly
open Async
open Async_smtp

let () = Dynamic.set_root Backtrace.elide true
let valid_username = "username"
let valid_password = "password"

let plugin ~login ~plain =
  (module struct
    open Smtp_monad.Let_syntax
    module State = Unit

    module Session = struct
      include Smtp_server.Plugin.Simple.Session

      let greeting _ = "[SMTP TEST SERVER]"
      let check username password = username = valid_username && password = valid_password

      let extensions ~state:() _ =
        List.filter_opt
          [ Some
              (Smtp_server.Plugin.Extension.Start_tls
                 (module (
                 struct
                   type session = t

                   let upgrade_to_tls ~log:_ t = return { t with tls = true }
                 end :
                   Smtp_server.Plugin.Start_tls with type session = t)))
          ; Option.some_if
              plain
              (Smtp_server.Plugin.Extension.Auth
                 (module Smtp_auth.Plain.Server (struct
                     type nonrec t = t

                     let authenticate ~log:_ ?on_behalf_of:_ t ~username ~password =
                       if check username password
                       then return { t with authenticated = Some username }
                       else
                         Smtp_monad.reject
                           ~here:[%here]
                           Smtp_reply.authentication_credentials_invalid_535
                     ;;
                   end)))
          ; Option.some_if
              login
              (Smtp_server.Plugin.Extension.Auth
                 (module Smtp_auth.Login.Server (struct
                     type nonrec t = t

                     let authenticate ~log:_ t ~username ~password =
                       if check username password
                       then return { t with authenticated = Some username }
                       else
                         Smtp_monad.reject
                           ~here:[%here]
                           Smtp_reply.authentication_credentials_invalid_535
                     ;;
                   end)))
          ]
      ;;
    end

    module Envelope = struct
      include Smtp_server.Plugin.Simple.Envelope

      let mail_from ~state ~log session sender args =
        match session.Session.authenticated with
        | None -> Smtp_monad.reject ~here:[%here] Smtp_reply.authentication_required_530
        | Some _ -> mail_from ~state ~log session sender args
      ;;
    end

    let rpcs () = []
  end : Smtp_server.Plugin.S
    with type State.t = unit)
;;

let anon = Smtp_client.Credentials.anon

let valid =
  Smtp_client.Credentials.login ~username:valid_username ~password:valid_password ()
;;

let valid_with_delegation =
  Smtp_client.Credentials.login
    ~on_behalf_of:"bob"
    ~username:valid_username
    ~password:valid_password
    ()
;;

let invalid =
  Smtp_client.Credentials.login
    ~username:valid_username
    ~password:("X" ^ valid_password)
    ()
;;

let%expect_test "cowardly refuse to authenticate because of insecure transport" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:false
      ~plugin:(plugin ~login:true ~plain:true)
      ~credentials:valid
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH PLAIN LOGIN
    > QUIT
    < 221 closing connection
    client_ERROR: ("No common auth mechanism available and ANON authentication not allowed by client"
     ((client_mechs ()) (server_mechs (PLAIN LOGIN))))
    |}];
  return ()
;;

let%expect_test "fail if no supported authentication mechanisms are found" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:false ~plain:false)
      ~credentials:valid
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250 8BITMIME
    > QUIT
    < 221 closing connection
    client_ERROR: ("No common auth mechanism available and ANON authentication not allowed by client"
     ((client_mechs (PLAIN LOGIN)) (server_mechs ())))
    |}];
  return ()
;;

let%expect_test "ANON doesn't require mechanisms" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:false ~plain:false)
      ~credentials:anon
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250 8BITMIME
    > QUIT
    < 221 closing connection
    |}];
  return ()
;;

let%expect_test "AUTH LOGIN success with correct credentials" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:true ~plain:false)
      ~credentials:valid
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH LOGIN
    > AUTH LOGIN
    < 334 VXNlcm5hbWU6
    > dXNlcm5hbWU=
    < 334 UGFzc3dvcmQ6
    > cGFzc3dvcmQ=
    < 235 Authentication successful
    > QUIT
    < 221 closing connection
    |}];
  return ()
;;

let%expect_test "AUTH LOGIN rejected with invalid credentials" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:true ~plain:false)
      ~credentials:invalid
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH LOGIN
    > AUTH LOGIN
    < 334 VXNlcm5hbWU6
    > dXNlcm5hbWU=
    < 334 UGFzc3dvcmQ6
    > WHBhc3N3b3Jk
    < 535 Authentication credentials invalid
    > QUIT
    < 221 closing connection
    client_ERROR: (monitor.ml.Error "AUTH failed: 535 Authentication credentials invalid"
     ("<backtrace elided in test>"))
    |}];
  return ()
;;

let%expect_test "AUTH PLAIN success with correct credentials" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:false ~plain:true)
      ~credentials:valid
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH PLAIN
    > AUTH PLAIN AHVzZXJuYW1lAHBhc3N3b3Jk
    < 235 Authentication successful
    > QUIT
    < 221 closing connection
    |}];
  return ()
;;

let%expect_test "AUTH PLAIN rejected with invalid credentials" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:false ~plain:true)
      ~credentials:invalid
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH PLAIN
    > AUTH PLAIN AHVzZXJuYW1lAFhwYXNzd29yZA==
    < 535 Authentication credentials invalid
    > QUIT
    < 221 closing connection
    client_ERROR: (monitor.ml.Error "AUTH failed: 535 Authentication credentials invalid"
     ("<backtrace elided in test>"))
    |}];
  return ()
;;

let%expect_test "prefer AUTH PLAIN" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:true ~plain:true)
      ~credentials:valid
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH PLAIN LOGIN
    > AUTH PLAIN AHVzZXJuYW1lAHBhc3N3b3Jk
    < 235 Authentication successful
    > QUIT
    < 221 closing connection
    |}];
  return ()
;;

let%expect_test "AUTH PLAIN with delegation" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:true ~plain:true)
      ~credentials:valid_with_delegation
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH PLAIN LOGIN
    > AUTH PLAIN Ym9iAHVzZXJuYW1lAHBhc3N3b3Jk
    < 235 Authentication successful
    > QUIT
    < 221 closing connection
    |}];
  return ()
;;

let%expect_test "AUTH LOGIN does not allow delegation" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:true ~plain:false)
      ~credentials:valid_with_delegation
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH LOGIN
    > QUIT
    < 221 closing connection
    client_ERROR: ("No common auth mechanism available and ANON authentication not allowed by client"
     ((client_mechs (PLAIN)) (server_mechs (LOGIN))))
    |}];
  return ()
;;

let%expect_test "prefer AUTH over ANON" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:true ~plain:true)
      ~credentials:(anon @ valid)
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH PLAIN LOGIN
    > AUTH PLAIN AHVzZXJuYW1lAHBhc3N3b3Jk
    < 235 Authentication successful
    > QUIT
    < 221 closing connection
    |}];
  return ()
;;

let%expect_test "use ANON if no supported AUTH methods" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:true ~plain:false)
      ~credentials:(anon @ valid_with_delegation)
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH LOGIN
    > QUIT
    < 221 closing connection
    |}];
  return ()
;;

let%expect_test "ANON does not bypass invalid credential failure" =
  let%bind () =
    Smtp_expect_test_helper.smtp
      ~tls:true
      ~plugin:(plugin ~login:true ~plain:true)
      ~credentials:(anon @ invalid)
      []
  in
  [%expect
    {|
    < 220 [SMTP TEST SERVER]
    > EHLO [SMTP TEST CLIENT]
    < 250-Ok: Continue, extensions follow:
    < 250-8BITMIME
    < 250 AUTH PLAIN LOGIN
    > AUTH PLAIN AHVzZXJuYW1lAFhwYXNzd29yZA==
    < 535 Authentication credentials invalid
    > QUIT
    < 221 closing connection
    client_ERROR: (monitor.ml.Error "AUTH failed: 535 Authentication credentials invalid"
     ("<backtrace elided in test>"))
    |}];
  return ()
;;

let%expect_test "AUTH LOGIN with client going first" =
  Smtp_expect_test_helper.manual_client
    ~plugin:(plugin ~login:true ~plain:true)
    (fun ~client ~server ~expect_server_close:_ ->
       let%bind () = server "220 [SMTP TEST SERVER]" in
       let%bind () = client "EHLO [SMTP TEST CLIENT]" in
       let%bind () =
         server "250-Ok: Continue, extensions follow:\n250-8BITMIME\n250 AUTH PLAIN LOGIN"
       in
       let%bind () = client "AUTH LOGIN dXNlcm5hbWU=" in
       let%bind () = server "334 UGFzc3dvcmQ6" in
       let%bind () = client "cGFzc3dvcmQ=" in
       let%bind () = server "235 Authentication successful" in
       client "QUIT")
;;

let%expect_test "AUTH PLAIN with server going first" =
  Smtp_expect_test_helper.manual_client
    ~plugin:(plugin ~login:true ~plain:true)
    (fun ~client ~server ~expect_server_close:_ ->
       let%bind () = server "220 [SMTP TEST SERVER]" in
       let%bind () = client "EHLO [SMTP TEST CLIENT]" in
       let%bind () =
         server "250-Ok: Continue, extensions follow:\n250-8BITMIME\n250 AUTH PLAIN LOGIN"
       in
       let%bind () = client "AUTH PLAIN" in
       let%bind () =
         server
           "334 UmVxdWlyZSAke09OX0JFSEFMRn1cTlVMTCR7VVNFUk5BTUV9XE5VTEwke1BBU1NXT1JEfQ=="
       in
       let%bind () = client "Ym9iAHVzZXJuYW1lAHBhc3N3b3Jk" in
       let%bind () = server "235 Authentication successful" in
       client "QUIT")
;;

let%expect_test "AUTH LOGIN when not advertised" =
  Smtp_expect_test_helper.manual_client
    ~plugin:(plugin ~login:false ~plain:false)
    (fun ~client ~server ~expect_server_close:_ ->
       let%bind () = server "220 [SMTP TEST SERVER]" in
       let%bind () = client "EHLO [SMTP TEST CLIENT]" in
       let%bind () = server "250-Ok: Continue, extensions follow:\n250 8BITMIME" in
       let%bind () = client "AUTH LOGIN" in
       let%bind () = server "502 Smtp_command not implemented: AUTH LOGIN" in
       client "QUIT")
;;

let%expect_test "AUTH PLAIN when not advertised" =
  Smtp_expect_test_helper.manual_client
    ~plugin:(plugin ~login:false ~plain:false)
    (fun ~client ~server ~expect_server_close:_ ->
       let%bind () = server "220 [SMTP TEST SERVER]" in
       let%bind () = client "EHLO [SMTP TEST CLIENT]" in
       let%bind () = server "250-Ok: Continue, extensions follow:\n250 8BITMIME" in
       let%bind () = client "AUTH PLAIN 1234" in
       let%bind () = server "502 Smtp_command not implemented: AUTH PLAIN" in
       client "QUIT")
;;
