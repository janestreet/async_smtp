open Core
open Async
open Async_smtp.Std

let key_value_pair = Command.Spec.Arg_type.create (String.lsplit2_exn ~on:':')
let email_address = Command.Spec.Arg_type.create (Email_address.of_string_exn)

let command =
  Command.async_or_error ~summary:"simple demo of Async_smtp.Std.Simplemail.send"
    Command.Spec.(
      empty
      ++ step (fun m v -> m ?server:v)
      +> flag "server" ~doc:"HOST:PORT server to send mail to"
           (optional host_and_port)
      ++ step (fun m v -> m ?from:v)
      +> flag "from" ~doc:"EMAIL sender of the email"
           (optional email_address)
      ++ step (fun m v -> m ~subject:v)
      +> flag "subject" ~doc:"STRING subject of the email"
           (required string)
      ++ step (fun m (r, l) -> m ~to_:(r :: l))
      +> flag "to" ~doc:"EMAIL recipients of the message"
           (one_or_more email_address)
      ++ step (fun m v -> m ~cc:v)
      +> flag "cc" ~doc:"EMAIL CC'd recipient"
           (listed email_address)
      ++ step (fun m v -> m ~bcc:v)
      +> flag "bcc" ~doc:"EMAIL BCC'd recipient"
           (listed email_address)
      ++ step (fun m v -> m ~attachments:v)
      +> flag "attachment" ~doc:"FILE to include as an attachment"
           (listed file)
      ++ step (fun m v -> m ~content_type:v)
      +> flag "content-type"
           ~doc:(sprintf "STRING content-type of the message (default: %s)"
                   Simplemail.Mimetype.text)
           (optional_with_default Simplemail.Mimetype.text string)
      ++ step (fun m v -> m ~extra_headers:v)
      +> flag "extra-header"
           ~doc:"NAME:VALUE Additional headers to include"
           (listed key_value_pair)
      +> anon ("BODY" %: string)
    )
    (fun ?server ?from ~subject ~to_ ~cc ~bcc ~attachments ~content_type ~extra_headers body () ->
       Deferred.List.map attachments ~f:(fun file ->
         Simplemail.Content.of_file file
         >>| fun content ->
         Filename.basename file, content)
       >>= fun attachments ->
       let content = Simplemail.Content.create ~content_type body in
       Simplemail.send ?server ?from ~subject ~to_ ~cc ~bcc ~attachments ~extra_headers content)
;;

let () = Command.run command
