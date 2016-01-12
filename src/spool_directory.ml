open Core.Std
open Async.Std
open Common

type t = string [@@deriving bin_io, sexp, compare]

let active_dir     spool_dir = spool_dir ^/ "active"
let tmp_dir        spool_dir = spool_dir ^/ "tmp"
let frozen_dir     spool_dir = spool_dir ^/ "frozen"
let removed_dir    spool_dir = spool_dir ^/ "removed"
let quarantine_dir spool_dir = spool_dir ^/ "quarantine"
let lock_file      spool_dir = spool_dir ^/ "lock"

let lock t = Lock_file.create ~message:"async_smtp spool" (lock_file t)

let init ~path =
  is_accessible_directory path
  >>=? fun () ->
  lock path
  >>= function
  | false ->
    return
      (Or_error.error_string
         "Spool directory already locked; Is there another async_smtp instance running \
          with this config?")
  | true ->
    is_accessible_directory ~create_if_missing:() (active_dir path)
    >>=? fun () ->
    is_accessible_directory ~create_if_missing:() (tmp_dir path)
    >>=? fun () ->
    is_accessible_directory ~create_if_missing:() (frozen_dir path)
    >>=? fun () ->
    is_accessible_directory ~create_if_missing:() (removed_dir path)
    >>=? fun () ->
    is_accessible_directory ~create_if_missing:() (quarantine_dir path)
    >>=? fun () ->
    Sys.ls_dir (tmp_dir path)
    >>| (function
    | [] -> Ok path
    | _nonempty ->
      Or_error.error_string
        (sprintf "tmp dir %s is not empty, shutdown was not clean" (tmp_dir path)))

(* do not show messages that have been removed, only active and frozen *)
let ls t =
  let ls path =
    safely_ls_dir path
    >>|? fun entries ->
    List.map entries ~f:(fun entry -> path ^/ entry)
  in
  ls (active_dir t)
  >>=? fun active_dir ->
  ls (frozen_dir t)
  >>|? fun frozen_dir ->
  active_dir @ frozen_dir
