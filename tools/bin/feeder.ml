open Core.Std;;
open Async.Std;;
open Async_smtp.Std;;

module Envelope = Smtp_envelope
module Reply = Smtp_reply

let main ~smtp_config ~raw ~dest () =
  Log.Global.set_output [Log.Output.stderr ()];
  let reader = Lazy.force Reader.stdin in
  Smtp_client.Tcp.with_ ~config:smtp_config dest ~f:fun client ->
    Deferred.Or_error.try_with (fun () ->
      if raw then begin
        let is_data = ref false in
        Pipe.iter (Reader.lines reader) ~f:(fun line ->
          if String.equal line "." then is_data:=false;
          Writer.write_line (Smtp_client.Raw.writer client) line;
          if not (!is_data) then begin
            Writer.flushed (Smtp_client.Raw.writer client)
            >>= fun () ->
            Smtp_client.Raw.receive client
            >>| Or_error.ok_exn
            >>| function
            | `Bsmtp -> assert false
            | `Received reply ->
              printf !"%{Smtp_reply}\n" reply;
              match reply with
              | Smtp_reply.Start_mail_input_354 ->
                is_data:=true
              | _ -> ()
          end else Deferred.unit)
      end else begin
        Pipe.iter (Smtp_server.read_bsmtp reader) ~f:fun envelope ->
          let envelope = Or_error.ok_exn envelope in
          Smtp_client.send_envelope client envelope
          >>| Or_error.ok_exn
          >>| function
          | (Ok _) as status ->
            Log.Global.info
              !"Envelope accepted:\n%{Smtp_client.Envelope_status}"
              status
          | (Error _) as status ->
            Log.Global.info
              !"Envelope rejected:\n%{Smtp_client.Envelope_status}"
              status
      end
    )
;;

let command =
  Command.async_or_error
    ~summary:"Read a BSMTP transcript and send it to a target host"
    Command.Spec.(
      empty
      ++ Async_smtp_tools.Util.Smtp_client_config.arg_spec ()
      ++ step (fun m raw -> m ~raw)
      +> flag "-raw" no_arg ~doc:"Send raw SMTP command rather then envelopes"
      ++ step (fun m dest -> m ~dest)
      +> anon ("target" %: Async_smtp_tools.Util.Host_and_port.arg_type_with_port 25)
    )
    main
;;

Command.run command
