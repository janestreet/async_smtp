open Core.Std
open Async.Std
open Async_smtp.Std
open Async_smtp_tools.Std

module Envelope = Smtp_envelope

module Headers = Transform_headers

module Config = struct
  type t =
    { headers  : Headers.Config.t with default( Headers.Config.default );
      truncate_after_n_messages : int option;
      add_tracking_id : string option
    } with sexp

  let load file =
    Reader.load_sexp_exn file t_of_sexp

  let default =
    { headers = Headers.Config.default
    ; truncate_after_n_messages = None
    ; add_tracking_id = None
    }
end

let main ?config () =
  begin match config with
  | Some config -> Config.load config
  | None -> return Config.default
  end
  >>= fun config ->
  let stdin  = Lazy.force Reader.stdin in
  let stdout = Lazy.force Writer.stdout in
  let pipe = Smtp_server.read_mbox stdin in
  let pipe = Pipe.map pipe ~f:Or_error.ok_exn in
  let pipe = Pipe.map pipe ~f:(Headers.transform config.Config.headers) in
  let pipe =
    match config.Config.truncate_after_n_messages with
    | None -> pipe
    | Some n ->
      Pipe.init (fun out ->
        Pipe.to_list pipe
        >>= fun messages ->
        let messages = List.take messages n in
        Deferred.List.iter messages ~f:(Pipe.write out))
  in
  let pipe =
    match config.Config.add_tracking_id with
    | None -> pipe
    | Some name ->
      let counter = ref 0 in
      Pipe.map pipe ~f:(fun envelope ->
        incr counter;
        Envelope.add_header envelope ~name ~value:(Int.to_string !counter))
  in
  Smtp_client.Bsmtp.write stdout pipe
;;

let command =
  Command.async_or_error
    ~summary:("Convert mbox to bsmtp")
    Command.Spec.(
      empty
      ++ step (fun m config -> m ?config)
      +> flag "-config" (optional string) ~doc:"CONFIG"
    )
    main
;;

Command.run command
