open Core
open Async
open Async_smtp

let config =
  { Smtp_server.Config.
    spool_dir = "/tmp/spool-mailcore"
  ; tmp_dir = None
  ; where_to_listen = [`Port 2200; `Port 2201 ]
  ; max_concurrent_send_jobs = 1
  ; max_concurrent_receive_jobs_per_port = 1
  ; rpc_port = 2210
  ; malformed_emails = `Reject
  ; max_message_size = Byte_units.create `Megabytes 1.
  ; tls_options =
      Some
        { Smtp_server.Config.Tls_options.
          version = None
        ; options = None
        ; name = None
        ; allowed_ciphers = `Secure
        ; crt_file = "/tmp/mailcore.crt"
        ; key_file = "/tmp/mailcore.key"
        ; ca_file = None
        ; ca_path  = None
        }
  ; tcp_options = None
  ; client = Smtp_client.Config.default
  }

module Server = Smtp_server.Make(struct
    open Smtp_monad.Let_syntax
    module Session = Smtp_server.Plugin.Simple.Session
    module Envelope = struct
      include Smtp_server.Plugin.Simple.Envelope

      let destination =
        `Inet (Host_and_port.create ~host:"localhost" ~port:25)

      let process ~log:_ _session t email =
        let envelope =
          Smtp_envelope.Routed.create
            ~envelope:(smtp_envelope t email)
            ~next_hop_choices:[destination]
            ~retry_intervals:[]
        in
        return (`Send [Smtp_envelope.Routed.Batch.single_envelope envelope])
    end
    let rpcs () = []
  end)

let main () =
  Server.start ~log:(Lazy.force Log.Global.log) ~config
  >>| Or_error.ok_exn
  >>= fun server ->
  let ports =
    Server.ports server
    |> List.map ~f:Int.to_string
    |> String.concat ~sep:", "
  in
  Log.Global.info "mailcore listening on ports %s" ports;
  Shutdown.set_default_force Deferred.never;
  Shutdown.at_shutdown (fun () ->
    Server.close ~timeout:(Clock.after (sec 60.)) server
    >>| Or_error.ok_exn);
  Deferred.never ()
;;

let run () =
  don't_wait_for (main ());
  never_returns (Scheduler.go ())
;;

run ()
