open Core
open! Async

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
            ~tag:(sprintf "%s is not writable, readable, and executable by running user" dir)
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
