open! Core
open! Async

include struct
  open Async_smtp_types
  module Envelope = Smtp_envelope
  module Selector = Smtp_envelope.Selector
end

include struct
  open Email_message
  module Email = Email
end

let command =
  Command.async
    ~summary:"Test Envelope_selector with a real message."
    [%map_open.Command
      let envelope =
        choose_one
          ~if_nothing_chosen:Raise
          [ (let%map_open.Command eml_file =
               flag
                 "-eml-file"
                 (optional Filename_unix.arg_type)
                 ~doc:"FILE read test email from FILE in eml format"
             and sender =
               flag
                 "-sender"
                 (optional (Command.Arg_type.create Envelope.Sender.of_string_exn))
                 ~doc:
                   "SENDER envelope sender, only with -eml-file, default is to guess \
                    from the headers"
             and recipients =
               flag
                 "-recipients"
                 (optional (Command.Arg_type.create Email_address.list_of_string_exn))
                 ~doc:
                   "RECIPIENTS envelope recipients, only with -eml-file, default is to \
                    guess from the headers"
             in
             if Option.is_none eml_file
                && (Option.is_some sender || Option.is_some recipients)
             then raise_s [%message "Can't use -sender or -recipients without -eml-file"];
             Option.map eml_file ~f:(fun eml_file () ->
               let open Deferred.Let_syntax in
               let%map envelope =
                 Reader.file_contents eml_file
                 >>| Email.of_string
                 >>| Envelope.of_email
                 >>| Or_error.ok_exn
               in
               Envelope.set envelope ?sender ?recipients ()))
          ; (let%map_open.Command bsmtp_file =
               flag
                 "-bsmtp-file"
                 (optional Filename_unix.arg_type)
                 ~doc:
                   "FILE read test email from FILE in BSMTP format (SMTP session \
                    transcript)"
             in
             Option.map bsmtp_file ~f:(fun bsmtp_file () ->
               let open Deferred.Let_syntax in
               match%map
                 Reader.with_file bsmtp_file ~f:(fun r ->
                   Async_smtp.Smtp_server.read_bsmtp r |> Pipe.to_list)
                 >>| Or_error.all
                 >>| Or_error.ok_exn
               with
               | [ t ] -> t
               | [] | _ :: _ :: _ -> raise_s [%message "Expected exactly one envelope"]))
          ]
      and selector =
        flag
          "-selector"
          (required (sexp_conv Selector.Stable.V1.t_of_sexp))
          ~doc:"SELECTOR test sexp-formatted SELECTOR"
      in
      fun () ->
        let%bind envelope = envelope () in
        match Selector.matches selector envelope with
        | true ->
          printf "The selector matches the envelope\n";
          exit 0
        | false ->
          printf "The selector DOES NOT match the envelope\n";
          exit 1]
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
