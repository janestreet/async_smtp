open Core.Std
open Async.Std
open Async_smtp.Std

module Reply = Smtp_reply

let check_connection reader writer =
  let send_one_cmd cmd =
    Writer.write_line writer (Smtp_command.to_string cmd);
    Writer.flushed writer
  in
  let receive_one_cmd () =
    Reader.read_line reader
    >>| function
    | `Eof -> Error (Error.of_string "Got 'Eof' on connection")
    | `Ok resp ->
      let r = Reply.of_string resp in
      if Reply.is_ok r then Ok r
      else Error (Error.of_string (sprintf !"Failed to send message: %{Reply}" r))
  in
  let send_and_receive_one_cmd cmd =
    send_one_cmd cmd >>= fun () -> receive_one_cmd ()
  in
  receive_one_cmd ()
  >>=? fun _ ->
  send_and_receive_one_cmd (Smtp_command.Helo (Unix.gethostname ()))
  >>=? fun _ ->
  send_and_receive_one_cmd Smtp_command.Quit
  >>|? fun _ -> ()

let check ~host ~port ~handshake =
  Monitor.try_with (fun () ->
      let destination = Tcp.to_host_and_port host port in
      Tcp.with_connection destination
        (fun _socket reader writer ->
           if handshake then
             check_connection reader writer
           else
             Deferred.Or_error.ok_unit
    ))
  >>| function
  | Ok ok -> ok
  | Error exn -> Or_error.of_exn exn

let rec main ~host ~port ~sleep ~trys ~handshake () =
  match trys with
  | 0 ->
    exit 1
  | _ ->
    check ~host ~port ~handshake
    >>= function
    | Ok () ->
      printf "ok\n";
      exit 0
    | Error _err ->
      Clock.after sleep
      >>= fun () ->
      main ~host ~port ~sleep ~trys:(trys - 1) ~handshake ()

let command =
  Command.async
    ~summary:("Repeatedly attempt to connect to an smtp server until handshake succeeds.")
    Command.Spec.(
      empty
      ++ step (fun m host -> m ~host)
      +> flag "host" (optional_with_default "localhost" string)
        ~doc:"HOST target host to wait for (default localhost)"
      ++ step (fun m port -> m ~port)
      +> flag "port" (optional_with_default 25 int)
        ~doc:"PORT target port to wait for (default localhost)"
      ++ step (fun m sleep -> m ~sleep)
      +> flag "sleep" (optional_with_default (sec 0.1) time_span)
        ~doc:"SLEEP how long to wait before retrying, for local testing small
               values are probably okay (default 0.1s)"
      ++ step (fun m trys -> m ~trys)
      +> flag "trys" (optional_with_default (-1) int)
        ~doc:"TRYS how often to try before giving up.
               -1 means don't give up (default -1)"
      ++ step (fun m no_handshake -> m ~handshake:(not no_handshake))
      +> flag "-no-handshake" no_arg
        ~doc:" skip connection handshake"
    )
    main
;;

Command.run command
