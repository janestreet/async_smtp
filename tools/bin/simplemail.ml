open Core.Std
open Async.Std
open Async_smtp.Std
open Email_message.Std

let command =
  Command.async_or_error ~summary:"simple mail sending utility for testing"
    Command.Spec.(empty
                  ++ Async_smtp_tools.Util.Smtp_client_config.arg_spec ()
                  ++ step (fun m dest -> m ~dest)
                  +> flag "-server" (required (Async_smtp_tools.Util.Host_and_port.arg_type_with_port 25))
                       ~doc:"HOST[:PORT] to send the email to to"
                  +> anon ("EMAIL" %: file)
                 )
    (fun ~smtp_config ~dest email () ->
       Deferred.Or_error.try_with_join (fun () ->
         Reader.file_contents email
         >>= fun email ->
         Email.of_string email
         |> Smtp_envelope.of_email
         |> Or_error.ok_exn
         |> fun envelope ->
         Smtp_client.Tcp.with_ ~config:smtp_config dest ~f:(fun client ->
             Smtp_client.send_envelope client envelope
             >>|? Smtp_client.Envelope_status.ok_or_error ~allow_rejected_recipients:false
             >>| Or_error.join
           )
         >>|? fun msg_id ->
         printf "OK: %s\n" msg_id))
;;

let () = Command.run command
