open! Core
open Async
open Async_smtp

let html_string : string =
  {|
<html><body>
<div style="white-space:pre; font-family:courier">
  Here is an html email with an inline image (png):
  <h4>H4 heading</h4>
<img src="cid:png_id"/><br>
</div>
</body></html>
           |}
;;

let body : Email.Simple.Content.t = Email.Simple.Content.html_utf8 html_string

let png ~file : Email.Simple.Content.t =
  Email.Simple.Content.create_custom
    ~content_type:Email.Simple.Mimetype.png
    ~encoding:`Base64
    (In_channel.read_all file)
;;

let email ~png_file : Email.Simple.Content.t =
  Email.Simple.Content.with_related ~resources:[ "png_id", png ~file:png_file ] body
;;

let command =
  let open Command.Let_syntax in
  Command.async_or_error
    ~summary:"send an email with an inline picture (png)"
    [%map_open
      let png_file = flag "png" (required Filename_unix.arg_type) ~doc:"FILE png file"
      and to_ =
        flag "to" (required string) ~doc:"EMAIL who to send it to"
        |> map ~f:Email_address.of_string_exn
      in
      fun () ->
        Simplemail.send
          ~to_:[ to_ ]
          ~subject:"email with an inline image"
          (email ~png_file)]
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
