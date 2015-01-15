open Core.Std
open Async.Std
open Async_smtp.Std
open Email_message.Std

let command =
  Command.async_basic ~summary:"simple mail sending utility for testing"
    Command.Spec.(empty
                  +> anon ("EMAIL" %: file)
                  +> flag "-host" (required string) ~doc:"HOST"
                  +> flag "-port" (required int) ~doc:"PORT"
    )
    (fun email host port () ->
      Reader.file_contents email
      >>= fun email ->
      Email.of_string email
      |> Smtp_envelope.of_email
      |> Or_error.ok_exn
      |> fun envelope ->
        let dest = Host_and_port.create ~host ~port in
        Smtp_client.send dest envelope
        |> Smtp_result.ok_exn
        >>= fun () ->
        return (printf "OK\n"))
;;

let () = Command.run command
