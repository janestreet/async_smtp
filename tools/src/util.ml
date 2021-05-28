open Core
open Async
open Async_smtp

module Host_and_port = struct
  include Host_and_port

  let of_string ~port str =
    match String.rsplit2 ~on:':' str with
    | None -> Host_and_port.create ~host:str ~port
    | Some (host, "") -> Host_and_port.create ~host ~port
    | Some (host, port) ->
      let port = Int.of_string port in
      Host_and_port.create ~host ~port
  ;;

  let inet_address addr = Tcp.Where_to_connect.of_host_and_port addr
end

module Address = struct
  let param dest =
    let open Command.Let_syntax in
    [%map_open
      let dest = dest in
      Host_and_port.of_string ~port:25 dest]
  ;;

  let param_anon = param Command.Param.(anon ("HOST[:PORT]" %: string))

  let param_server =
    param
      Command.Param.(
        flag
          "-server"
          (required string)
          ~doc:"HOST[:PORT]  Address of SMTP server to connect to")
  ;;
end

module Smtp_client_config = struct
  include Smtp_client.Config

  let load file = Sexp.load_sexp_conv_exn file t_of_sexp

  let default =
    lazy
      (try load ".js-smtp.sexp" with
       | _ ->
         (try
            let home = Option.value (Sys.getenv "HOME") ~default:"~" in
            load (home ^/ ".js-smtp.sexp")
          with
          | _ -> default))
  ;;

  let param =
    let open Command.Let_syntax in
    [%map_open
      let config_file =
        flag
          "-smtp-config"
          (optional Filename_unix.arg_type)
          ~doc:
            "File with config for the mailcore smtp client (defaults to ./.js-smtp.sexp \
             or ~/.js-smtp.sexp or system defaults)"
      in
      match config_file with
      | None -> Lazy.force default
      | Some config_file -> load config_file]
  ;;
end

module Hex = struct
  let to_hex digest =
    let result = Bytes.create (String.length digest * 2) in
    let hex = "0123456789ABCDEF" in
    for i = 0 to String.length digest - 1 do
      let c = int_of_char digest.[i] in
      Bytes.set result (2 * i) hex.[c lsr 4];
      Bytes.set result ((2 * i) + 1) hex.[c land 0xF]
    done;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:result
  ;;
end
