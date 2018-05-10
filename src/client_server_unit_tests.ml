open Core
open Async
open Async_smtp_types

let%test_module _ =
  (module struct

    let log =
      Lazy.force Async.Log.Global.log
      |> Mail_log.adjust_log_levels ~minimum_level:`Error


    let sender = Smtp_envelope.Sender.of_string "<>" |> Or_error.ok_exn
    let recipients = [Email_address.of_string_exn "mailcore-unit-test-recipient@example.com"]

    let email message =
      let subject = String.filter message ~f:(fun c -> c <> '\n') in
      Email.Simple.create
        ~to_:recipients
        ~subject:("Async_smtp unit test: " ^ subject)
        (Email.Simple.Content.text
           ("Async_smtp unit test:\n  " ^ message))

    let envelope message =
      Smtp_envelope.create ~sender ~recipients ~email:(email message) ()

    let test_client_and_server
          ~tmp_dir ~server_config ~client_config message ~expect_tls =
      Log.Global.set_level `Error;
      let server_config = { server_config with
                            (* let the system choose a port *)
                            Server.Config.where_to_listen = [`Port 0]
                          ; rpc_port=0
                          ; spool_dir = tmp_dir ^/ "spool"
                          ; max_concurrent_receive_jobs_per_port = 1
                          }
      in
      let finished = Ivar.create () in
      let envelope = envelope message in
      let module Server =
        Server.Make(struct
          open Smtp_monad.Let_syntax
          module Session = struct
            include Server.Plugin.Simple.Session
            let extensions _ =
              if expect_tls then
                [ Server.Plugin.Extension.Start_tls (module struct
                    type session = t
                    let upgrade_to_tls ~log:_ session =
                      return { session with tls=true }
                  end : Server.Plugin.Start_tls with type session=t)
                ]
              else []
          end
          module Envelope = struct
            include Server.Plugin.Simple.Envelope
            let process ~log:_ _session t email =
              let envelope = smtp_envelope t email in
              Ivar.fill finished envelope;
              return (`Consume "done")
          end
          let rpcs () = []
        end)
      in
      Server.start ~log ~config:server_config
      >>| Or_error.ok_exn
      >>= fun server ->
      let port = Server.ports server |> List.hd_exn in
      Clock.with_timeout (Time.Span.of_sec 10.) begin
        Deferred.all_unit
          [ begin
            Client.Tcp.with_
              ~log
              ~config:client_config
              (`Inet (Host_and_port.create ~host:"localhost" ~port))
              ~f:(fun client ->
                [%test_result: bool] (Client.is_using_tls client) ~expect:expect_tls;
                Client.send_envelope ~log client envelope
                >>|? Client.Envelope_status.ok_exn ~allow_rejected_recipients:false
                >>|? ignore)
            >>| Or_error.ok_exn
          end
          ; begin
            Ivar.read finished
            >>| fun envelope' ->
            if Smtp_envelope.compare envelope envelope' <> 0
            then failwithf !"Envelope mangled in transit: \
                             \ngot: %{sexp:Smtp_envelope.t}\
                             \nexpected: %{sexp:Smtp_envelope.t}"
                   envelope' envelope ()
          end ]
      end
      >>| (function `Result a -> a | `Timeout -> failwith "Timeout, email test should not take this long, maybe try again")
      >>= fun () ->
      Server.close server
      >>| Or_error.ok_exn

    let async_test_with_tmp_dir ~f =
      Thread_safe.block_on_async_exn (fun () ->
        Unix.mkdtemp ((Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp/") ^/ "mailcore-unit-tests")
        >>= fun dir ->
        Monitor.protect ~finally:(fun () ->
          Process.run ~prog:"/bin/rm" ~args:["-rf";"--";dir] ()
          >>|? ignore
          >>| Or_error.ok_exn)
          (fun () -> f dir))

    let non_tls_test message =
      async_test_with_tmp_dir ~f:(fun tmp_dir ->
        let server_config = Server.Config.default in
        let client_config = Client_config.default in
        test_client_and_server ~tmp_dir ~server_config ~client_config ~expect_tls:false
          message)

    let%test_unit _ =
      non_tls_test "Simple NON SSL round trip test\n"

    let%test_unit _ =
      non_tls_test "\nSimple NON SSL round trip test\n"

    let tls_test message =
      async_test_with_tmp_dir ~f:(fun tmp_dir ->
        let server_config =
          { Server.Config.default with
            tls_options =
              Some
                { Server.Config.Tls_options.
                  version = None
                ; options = None
                ; name = Some "localhost"
                ; allowed_ciphers = `Secure
                ; crt_file = "test-server.crt"
                ; key_file = "test-server.key"
                ; ca_file = Some "test-ca.crt"
                ; ca_path = None
                }
          }
        in
        let client_config =
          { Client_config.default with
            tls =
              [ Client_config.Domain_suffix.of_string ""
              , { Client_config.Tls.
                  version = None
                ; options = None
                ; name = None
                ; allowed_ciphers = `Secure
                ; ca_file = Some "test-ca.crt"
                ; ca_path = None
                ; mode = `Required
                ; certificate_mode = `Verify
                }
              ]
          }
        in
        test_client_and_server ~tmp_dir ~server_config ~client_config ~expect_tls:true
          message)

    let%test_unit _ =
      tls_test "Simple TLS Secured round trip test"

    let%test_unit _ =
      tls_test "\nSimple TLS Secured round trip test\n"

    ;;
  end)
