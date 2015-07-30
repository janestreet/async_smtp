open Core.Std
open Async.Std
open Async_smtp.Std

module Envelope = Smtp_envelope

let main ~name ~value () =
  let stdin  = Lazy.force Reader.stdin in
  let stdout = Lazy.force Writer.stdout in
  let pipe = Smtp_server.read_bsmtp stdin in
  let pipe = Pipe.map pipe ~f:Or_error.ok_exn in
  let pipe = Pipe.map pipe ~f:(Envelope.add_header ~name ~value) in
  Smtp_client.Bsmtp.write stdout pipe
;;

let command =
  Command.async_or_error
    ~summary:("Add a header to each message in a bsmtp file")
    Command.Spec.(
      empty
      ++ step (fun m name -> m ~name)
      +> flag "name" (required string) ~doc:""
      ++ step (fun m value -> m ~value)
      +> flag "value" (required string) ~doc:""
    )
    main
;;

Command.run command
