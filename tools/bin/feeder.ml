open Core.Std;;
open Async.Std;;
open Async_smtp.Std;;

module Envelope = Smtp_envelope
module Reply = Smtp_reply

let main ~host ~port () =
  Log.Global.set_output [Log.Output.stdout ()];
  let dest = Host_and_port.create ~host ~port in
  let reader = Lazy.force Reader.stdin in
  let rec loop () =
    Smtp_client.send_raw dest reader
    >>= function
    | Error e ->
      Log.Global.error "%s" (Error.to_string_hum e);
      shutdown 1;
      return ()
    | Ok (Error e) ->
      Log.Global.error "%s" (Smtp_error.to_string e);
      loop ()
    | Ok (Ok `Eof) ->
      return ()
    | Ok (Ok (`Ok_summary msg)) ->
      Log.Global.info "Sent %s" msg;
      loop ()
  in
  loop ()
;;

let command =
  Command.async
    ~summary:"Read a BSMTP transcript and send it to a target host"
    Command.Spec.(
      empty
      ++ step (fun m host -> m ~host)
      +> flag "-host" (required string)
           ~doc:"Host to send emails to (default: localhost)"
      ++ step (fun m port -> m ~port)
      +> flag "-port" (optional_with_default 25 int)
           ~doc:"Port to send emails to (default: 25)"
    )
    main
;;

Command.run command
