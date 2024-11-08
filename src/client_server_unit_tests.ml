open Core
open Poly
open Async
open Async_smtp_types

module%test _ = struct
  let log =
    Lazy.force Async.Log.Global.log |> Mail_log.adjust_log_levels ~minimum_level:`Error
  ;;

  let sender = Smtp_envelope.Sender.of_string "<>" |> Or_error.ok_exn

  let recipients =
    [ Email_address.of_string_exn "mailcore-unit-test-recipient@example.com" ]
  ;;

  let email message =
    let subject = String.filter message ~f:(fun c -> c <> '\n') in
    Email.Simple.create
      ~to_:recipients
      ~subject:("Async_smtp unit test: " ^ subject)
      (Email.Simple.Content.text_utf8 ("Async_smtp unit test:\n  " ^ message))
  ;;

  let envelope message =
    Smtp_envelope.create ~sender ~recipients ~email:(email message) ()
  ;;

  let test_client_and_server ~server_config ~client_config message ~expect_tls =
    Log.Global.set_level `Error;
    let server_config =
      { server_config with
        Server.Config.where_to_listen = [ Localhost_on_port_chosen_by_os ]
      ; rpc_port = 0
      ; max_concurrent_receive_jobs_per_port = 1
      }
    in
    let finished = Ivar.create () in
    let envelope = envelope message in
    let module Server =
      Server.Make (struct
        open Smtp_monad.Let_syntax
        module State = Unit

        module Session = struct
          include Server.Plugin.Simple.Session

          let extensions ~state:() _ =
            if expect_tls
            then
              [ Server.Plugin.Extension.Start_tls
                  (module struct
                    type session = t

                    let upgrade_to_tls ~log:_ session = return { session with tls = true }
                  end : Server.Plugin.Start_tls
                    with type session = t)
              ]
            else []
          ;;
        end

        module Envelope = struct
          include Server.Plugin.Simple.Envelope

          let process ~state:() ~log:_ ~flows:_ _session t email =
            let envelope = smtp_envelope t email in
            Ivar.fill_exn finished envelope;
            return "done"
          ;;
        end

        let rpcs () = []
      end)
    in
    let%bind server =
      Server.start ~server_state:() ~log ~config:server_config >>| Or_error.ok_exn
    in
    let port = Server.ports server |> List.hd_exn in
    let%bind () =
      match%map
        Clock.with_timeout
          (Time_float.Span.of_sec 10.)
          (Deferred.all_unit
             [ Client.Tcp.with_
                 ~log
                 ~config:client_config
                 (Host_and_port.create ~host:"localhost" ~port)
                 ~f:(fun client ->
                   [%test_result: bool] (Client.is_using_tls client) ~expect:expect_tls;
                   Client.send_envelope ~log client envelope
                   >>|? Client.Envelope_status.ok_exn ~allow_rejected_recipients:false
                   >>|? ignore)
               >>| Or_error.ok_exn
             ; (let%map envelope' = Ivar.read finished in
                if Smtp_envelope.compare envelope envelope' <> 0
                then
                  failwithf
                    !"Envelope mangled in transit: \n\
                      got: %{sexp:Smtp_envelope.t}\n\
                      expected: %{sexp:Smtp_envelope.t}"
                    envelope'
                    envelope
                    ())
             ])
      with
      | `Result a -> a
      | `Timeout ->
        failwith "Timeout, email test should not take this long, maybe try again"
    in
    Server.close server >>| Or_error.ok_exn
  ;;

  let non_tls_test message =
    Thread_safe.block_on_async_exn (fun () ->
      let server_config = Server.Config.default in
      let client_config = Client_config.default in
      test_client_and_server ~server_config ~client_config ~expect_tls:false message)
  ;;

  let%test_unit _ = non_tls_test "Simple NON SSL round trip test\n"
  let%test_unit _ = non_tls_test "\nSimple NON SSL round trip test\n"

  let tls_test message =
    Thread_safe.block_on_async_exn (fun () ->
      let server_config =
        { Server.Config.default with
          tls_options =
            Some
              { Server.Config.Tls_options.version = None
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
            [ ( Client_config.Domain_suffix.of_string ""
              , { Client_config.Tls.version = None
                ; options = None
                ; name = None
                ; allowed_ciphers = `Secure
                ; ca_file = Some "test-ca.crt"
                ; ca_path = None
                ; mode = `Required
                ; certificate_mode = `Verify
                } )
            ]
        }
      in
      test_client_and_server ~server_config ~client_config ~expect_tls:true message)
  ;;

  let%test_unit _ = tls_test "Simple TLS Secured round trip test"
  let%test_unit _ = tls_test "\nSimple TLS Secured round trip test\n"
end
