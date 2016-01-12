open Core.Std
open! Async.Std
open Types

let eof_error = Error.of_string "Saw EOF on Reader"

let mlog ~severity ?message ?dir fmt =
  let module Glog = Async.Std.Log.Global in
  let dir_string = match dir with
    | Some `In -> "<="
    | Some `Out -> "=>"
    | None -> "--"
  in
  let f =
    match severity with
    | `Error -> Glog.error
    | `Info  -> Glog.info
    | `Debug -> Glog.debug
  in
  let msg_id =
    match message with
    | Some m -> (Envelope.Id.to_string (Envelope.id m))
    | None -> "------------"
  in
  ksprintf (fun s -> f "%s %s %s" dir_string msg_id s) fmt
;;

let exchange reader writer message =
  Writer.write writer (message ^ "\n");
  Writer.flushed writer
  >>= fun () ->
  mlog ~dir:`Out ~severity:`Debug "Sent: \"%s\"" message;
  Reader.read_line reader
  >>= function
  | `Ok s ->
    begin
      mlog ~dir:`Out ~severity:`Debug "Recvd: \"%s\"" s;
      return (`Ok s)
    end
  | `Eof ->
    begin
      mlog ~dir:`Out ~severity:`Debug "Recvd EOF";
      return `Eof
    end
;;

let is_accessible_directory ?create_if_missing dir =
  Sys.file_exists dir
  >>= fun exists ->
  if exists = `No && Option.is_some create_if_missing
  then Unix.mkdir dir >>| Or_error.return
  else begin
    Sys.is_directory dir
    >>= function
    | `Unknown ->
      Deferred.Or_error.error_string
        (sprintf
           "could not determine whether or not %s was an accessible directory"
           dir)
    | `No ->
      Deferred.Or_error.error_string
        (sprintf "%s does not exist or is not a directory" dir)
    | `Yes -> begin
        Unix.access dir [ `Write; `Read; `Exec ]
        >>| function
        | Ok ()     -> Ok ()
        | Error exn ->
          Error.tag (Error.of_exn exn)
            (sprintf "%s is not writable, readable, and executable by running user" dir)
          |> fun e ->
          Error e
      end
  end

let safely_ls_dir dir =
  is_accessible_directory dir
  >>= function
  | Error e -> return (Error e)
  | Ok ()   ->
    Sys.ls_dir dir
    >>| fun contents ->
    Ok contents
;;

let unlink file =
  Deferred.Or_error.try_with (fun () -> Unix.unlink file)
;;

let rename ~src ~dst =
  Deferred.Or_error.try_with (fun () -> Unix.rename ~src ~dst)
