open Core
open Async
open Async_smtp
open Async_smtp_types

let server_config =
  { Smtp_server.Config.where_to_listen =
      [ Localhost_on_port 2200; Localhost_on_port 2201 ]
  ; max_concurrent_receive_jobs_per_port = 1
  ; timeouts = Smtp_server.Config.default.timeouts
  ; rpc_port = 2210
  ; rpc_heartbeat_config = None
  ; malformed_emails = `Reject
  ; max_message_size = Byte_units.of_megabytes 1.
  ; tls_options =
      Some
        { Smtp_server.Config.Tls_options.version = None
        ; options = None
        ; name = None
        ; allowed_ciphers = `Secure
        ; crt_file = "/tmp/mailcore.crt"
        ; key_file = "/tmp/mailcore.key"
        ; ca_file = None
        ; ca_path = None
        }
  ; tcp_options = None
  }
;;

let spool_config =
  { Smtp_spool.Config.spool_dir = "/tmp/spool-mailcore"
  ; tmp_dir = None
  ; connection_cache =
      Resource_cache.Address_config.default
      |> Resource_cache.Address_config.Stable.V2.of_v3
      |> Resource_cache.Address_config.Stable.V1.of_v2
  ; connection_cache_warming = None
  ; client = Smtp_client.Config.default
  }
;;

let the_spool = Set_once.create ()

module Server = Smtp_server.Make (struct
    open Smtp_monad.Let_syntax
    module State = Smtp_server.Plugin.Simple.State
    module Session = Smtp_server.Plugin.Simple.Session

    module Envelope = struct
      include Smtp_server.Plugin.Simple.Envelope

      let next_hop_choices = [ Host_and_port.create ~host:"localhost" ~port:25 ]

      let retry_intervals =
        let minute x = Smtp_envelope.Retry_interval.create (Time_float.Span.of_min x) in
        [ minute 1.; minute 2.; minute 2.; minute 5. ]
      ;;

      let process ~state:_ ~log:_ ~flows _session t email =
        let spool = Set_once.get_exn the_spool in
        let envelope = smtp_envelope t email in
        let routed_envelope =
          Smtp_envelope.Routed.create ~envelope ~next_hop_choices ~retry_intervals
          |> Smtp_envelope.Routed.Batch.single_envelope
        in
        let%bind _spooled_ids =
          Smtp_spool.add spool ~flows ~original_msg:envelope [ routed_envelope ]
          |> Smtp_monad.of_or_error ~here:[%here]
        in
        return (Smtp_envelope.id envelope |> Smtp_envelope.Id.to_string)
      ;;
    end

    let rpcs () = []
  end)

let handle_signals () =
  Signal.handle [ Signal.term; Signal.int ] ~f:(fun signal ->
    [%log.info_format !"shutting down upon receiving signal %{Signal}" signal];
    shutdown 0)
;;

let main () =
  handle_signals ();
  let log = Lazy.force Log.Global.log in
  let%bind spool = Smtp_spool.create ~config:spool_config ~log () >>| Or_error.ok_exn in
  Set_once.set_exn the_spool spool;
  let%bind server =
    Server.start ~server_state:() ~log:(Lazy.force Log.Global.log) ~config:server_config
    >>| Or_error.ok_exn
  in
  let ports =
    Server.ports server |> List.map ~f:Int.to_string |> String.concat ~sep:", "
  in
  [%log.info_format "mailcore listening on ports %s" ports];
  Shutdown.set_default_force Deferred.never;
  let timeout = Clock.after (sec 60.) in
  Shutdown.at_shutdown (fun () ->
    Deferred.all_unit
      [ Server.close ~timeout server >>| Or_error.ok_exn
      ; Smtp_spool.kill_and_flush ~timeout spool >>| Or_error.ok_exn
      ]);
  Deferred.never ()
;;

let run () =
  don't_wait_for (main ());
  never_returns (Scheduler.go ())
;;

let () = run ()
