open Core
open Async
open Async_smtp

let key_value_pair = Command.Param.Arg_type.create (String.lsplit2_exn ~on:':')
let email_address = Command.Param.Arg_type.create (Email_address.of_string_exn)

let command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"simple demo of Async_smtp.Simplemail.send"
    [%map_open
      let server =
        flag "server" ~doc:"HOST:PORT server to send mail to"
          (optional host_and_port)
      and from =
        flag "from" ~doc:"EMAIL sender of the email"
          (optional email_address)
      and subject =
        flag "subject" ~doc:"STRING subject of the email"
          (required string)
      and to_ =
        flag "to" ~doc:"EMAIL recipients of the message"
          (one_or_more email_address)
        |> map ~f:(fun (l,ls) -> l::ls)
      and cc =
        flag "cc" ~doc:"EMAIL CC'd recipient"
          (listed email_address)
      and bcc =
        flag "bcc" ~doc:"EMAIL BCC'd recipient"
          (listed email_address)
      and attachments =
        flag "attachment" ~doc:"FILE to include as an attachment"
          (listed file)
      and content_type =
        flag "content-type"
          ~doc:(sprintf "STRING content-type of the message (default: %s)"
                  Simplemail.Mimetype.text)
          (optional_with_default Simplemail.Mimetype.text string)
      and extra_headers =
        flag "extra-header"
          ~doc:"NAME:VALUE Additional headers to include"
          (listed key_value_pair)
      and body =
        anon ("BODY" %: string)
      in
      fun () ->
        Deferred.List.map attachments ~f:(fun file ->
          Simplemail.Content.of_file file
          >>| fun content ->
          Filename.basename file, content)
        >>= fun attachments ->
        let content = Simplemail.Content.create ~content_type body in
        Simplemail.send ?server ?from ~subject ~to_ ~cc ~bcc ~attachments ~extra_headers content
    ]
;;

let () = Command.run command
