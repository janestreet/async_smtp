open Core
open Async
open Async_smtp

module Host_and_port = struct
  include Host_and_port

  let of_string ~port str =
    match String.rsplit2 ~on:':' str with
    | None ->
      Host_and_port.create ~host:str ~port
    | Some (host,"") ->
      Host_and_port.create ~host ~port
    | Some (host,port) ->
      let port = Int.of_string port in
      Host_and_port.create ~host ~port

  let inet_address addr =
    Tcp.to_host_and_port (host addr) (port addr)
end

module Address =struct
  let arg_spec () =
    Command.Spec.(
      step (fun m inet unix dest ->
        let dest =
          if inet && unix then
            failwithf "can't sepecify both -inet and -unix" ()
          else if unix then `Unix dest
          else if inet then `Inet (Host_and_port.of_string ~port:25 dest)
          else if String.mem dest '/' then `Unix dest
          else `Inet (Host_and_port.of_string ~port:25 dest)
        in
        m ~dest)
      +> flag "-inet" no_arg
           ~doc:" Intepret the address as a HOST[:PORT]"
      +> flag "-unix" no_arg
           ~doc:" Intepret the address as a FILE"
      +> anon ("HOST[:PORT] | FILE" %: string))
end

module Smtp_client_config = struct
  include Smtp_client.Config
  let file_arg_type =
    Command.Spec.Arg_type.file
      (fun file -> Sexp.load_sexp_conv_exn file t_of_sexp)

  let load_sync path =
    match Core.Sys.file_exists path with
    | `Yes -> Some (Sexp.load_sexp_conv_exn path t_of_sexp)
    | `No | `Unknown -> None

  let default =
    match load_sync "./.js-smtp.sexp" with
    | Some default -> default
    | None ->
      ( match Sys.getenv "HOME" with
        | Some home ->
          (match load_sync (home ^/ ".js-smtp.sexp") with
           | Some default -> default
           | None -> default)
        | None -> default)

  let arg_spec () =
    Command.Spec.(
      step (fun m smtp_config -> m ~smtp_config)
      +> flag "-smtp-config" (optional_with_default default file_arg_type)
           ~doc:"File with config for the mailcore smtp client \
                 (defaults to ~/.js-smtp.sexp if it exists or system \
                 defaults otherwise)")
end

module Hex = struct
  let to_hex digest =
    let result = Bytes.create (String.length digest * 2) in
    let hex = "0123456789ABCDEF" in
    for i = 0 to String.length digest - 1 do
      let c = int_of_char digest.[i] in
      Bytes.set result (2*i) hex.[c lsr 4];
      Bytes.set result (2*i+1) hex.[c land 0xF]
    done;
    result
end
