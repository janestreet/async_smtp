open Core.Std
open Async.Std
open Async_smtp.Std

module Reply = Smtp_reply

let check_handshake ~smtp_config ~dest () =
  Deferred.Or_error.try_with_join (fun () ->
      Smtp_client.Tcp.with_ ~config:smtp_config dest ~f:fun _client ->
        Deferred.Or_error.ok_unit
    )

let check_no_handshake ~dest () =
  Deferred.Or_error.try_with_join (fun () ->
      let destination = Async_smtp_tools.Util.Host_and_port.inet_address dest in
      Tcp.with_connection destination
        (fun _socket _reader _writer ->
           Deferred.Or_error.ok_unit
    ))

let rec loop ~sleep ~trys ~check =
  match trys with
  | 0 ->
    exit 1
  | _ ->
    check ()
    >>= function
    | Ok () ->
      printf "ok\n";
      exit 0
    | Error _err ->
      Clock.after sleep
      >>= fun () ->
      loop ~sleep ~trys:(trys - 1) ~check

let main ~smtp_config ~dest ~sleep ~trys ~handshake () =
  let check =
    if handshake then
      check_handshake ~smtp_config ~dest
    else
      check_no_handshake ~dest
  in
  loop ~sleep ~trys ~check

let command =
  Command.async
    ~summary:("Repeatedly attempt to connect to an smtp server until handshake succeeds.")
    Command.Spec.(
      empty
      ++ Async_smtp_tools.Util.Smtp_client_config.arg_spec ()
      ++ step (fun m dest -> m ~dest)
      +> anon ("HOST[:PORT]" %: Async_smtp_tools.Util.Host_and_port.arg_type_with_port 25)
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
