open Core.Std
open Async.Std
open Async_smtp.Std

module T = Async_smtp_tools.Std.Transform_email_stream

(* We use this tool for testing Async_smtp.Server, but we use the server itself
   to read in the test outputs. This clearly has the potential to mask bugs. To
   deal with this we check in our test setup that reading and writing the exim
   output without any sorting or filtering has no effect. *)

module Config = struct
  include T.Config
  let load = function
    | Some config -> T.Config.load config
    | None -> return T.Config.default
end


let process ~config input output =
  let pipe =
    Smtp_server.read_bsmtp input
    |> Pipe.map ~f:Or_error.ok_exn
    |> Pipe.map ~f:(T.transform_without_sort config)
    |> T.sort config
  in
  Smtp_client.Bsmtp.write output pipe
  >>| Or_error.ok_exn
;;

let run_pipe ?config () =
  Config.load config
  >>= fun config ->
  let input = Reader.stdin |> Lazy.force in
  let output = Writer.stdout |> Lazy.force in
  process ~config input output

let run_file ~config input output =
  Reader.with_file ~exclusive:true input ~f:(fun input ->
    Unix.mkdir ~p:() (Filename.dirname output)
    >>= fun () ->
    Writer.with_file ~exclusive:true output ~f:(fun output ->
      process ~config input output))

let rec run_recursive ~config input output =
  Sys.is_directory_exn input
  >>= function
  | false -> run_file ~config input output
  | true ->
    Sys.ls_dir input
    >>= Deferred.List.iter ~f:(fun name ->
      run_recursive ~config (input ^/ name) (output ^/ name))

let run_recursive ?config input output () =
  Config.load config
  >>= fun config ->
  run_recursive ~config input output

let command_pipe =
  Command.async
    ~summary:("Sort and normalize emails in bsmtp format")
    Command.Spec.(
      empty
      ++ step (fun m config -> m ?config)
      +> flag "-config" (optional string)
        ~doc:"CONFIG"
    )
    run_pipe
;;

let command_recursive =
  Command.async
    ~summary:("normailize files or folders recursively")
    Command.Spec.(
      empty
      ++ step (fun m config -> m ?config)
      +> flag "-config" (optional string)
        ~doc:"CONFIG"
      +> anon ("src" %: string)
      +> anon ("dst" %: string)
    )
    run_recursive

let command =
  Command.group
   ~summary:"Tools for normalizing BSMTP formatted emails"
   [ "pipe", command_pipe;
     "recursive", command_recursive;
   ]

let () = Command.run command
