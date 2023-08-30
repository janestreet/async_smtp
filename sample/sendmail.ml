open Core
open Async
open Async_smtp

let key_value_pair = Command.Param.Arg_type.create (String.lsplit2_exn ~on:':')
let email_address = Command.Param.Arg_type.create Email_address.of_string_exn

let command =
  let open Command.Let_syntax in
  Command.async_or_error
    ~summary:"simple demo of Async_smtp.Simplemail.send"
    [%map_open
      let server =
        flag "server" ~doc:"HOST:PORT server to send mail to" (optional host_and_port)
      and from = flag "from" ~doc:"EMAIL sender of the email" (optional email_address)
      and subject = flag "subject" ~doc:"STRING subject of the email" (required string)
      and to_ =
        flag
          "to"
          ~doc:"EMAIL recipients of the message"
          (one_or_more_as_list email_address)
      and cc = flag "cc" ~doc:"EMAIL CC'd recipient" (listed email_address)
      and bcc = flag "bcc" ~doc:"EMAIL BCC'd recipient" (listed email_address)
      and attachments =
        flag
          "attachment"
          ~doc:"FILE to include as an attachment"
          (listed Filename_unix.arg_type)
      and content_type =
        Simplemail.(
          flag
            "content-type"
            ~doc:
              (sprintf
                 !"STRING content-type of the message (default: %s)"
                 (Mimetype.text_utf8 :> string))
            (optional_with_default Mimetype.text_utf8 Mimetype.arg_type))
      and extra_headers =
        flag
          "extra-header"
          ~doc:"NAME:VALUE Additional headers to include"
          (listed key_value_pair)
      and body = anon ("BODY" %: string) in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind attachments =
          Deferred.List.map ~how:`Sequential attachments ~f:(fun file ->
            let%map content = Simplemail.Content.of_file file in
            Filename.basename file, content)
        in
        let content = Simplemail.Content.create_custom ~content_type body in
        Simplemail.send
          ?server
          ?from
          ~subject
          ~to_
          ~cc
          ~bcc
          ~attachments
          ~extra_headers
          content]
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
