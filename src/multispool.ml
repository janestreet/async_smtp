open Core
open Async
open Common

(* Various shared utility functions *)
module Utils = struct
  let replace_unix_error error replacement ~f =
    match%bind Monitor.try_with ~rest:`Raise f with
    | Error e ->
      begin match Monitor.extract_exn e with
      | Unix.Unix_error (err, _, _) when err = error ->
        Deferred.Or_error.return replacement
      | e -> Deferred.Or_error.of_exn e
      end
    | Ok retval -> Deferred.Or_error.return (`Ok retval)
  ;;

  let try_to_rename ~src ~dst =
    replace_unix_error Unix.ENOENT `Not_found ~f:(fun () -> Unix.rename ~src ~dst)
  ;;

  let open_creat_excl_wronly ?perm path =
    replace_unix_error Unix.EEXIST `Exists ~f:(fun () ->
      Unix.openfile path ?perm ~mode:[`Creat; `Excl; `Wronly]
    )
  ;;

  let unlink path =
    Deferred.Or_error.try_with (fun () -> Unix.unlink path)
  ;;

  let stat_or_notfound path =
    replace_unix_error Unix.ENOENT `Not_found ~f:(fun () -> Unix.stat path)
  ;;

  let ls dir =
    Deferred.Or_error.try_with (fun () -> Sys.ls_dir dir)
  ;;

  let ls_sorted dir =
    let open Deferred.Or_error.Let_syntax in
    let%bind entries = ls dir in
    return (List.sort ~cmp:String.compare entries)
  ;;

  let is_dir path =
    match%map Sys.is_directory path with
    | `Yes           -> true
    | `No | `Unknown -> false
  ;;

  let is_dir_empty dir =
    let open Deferred.Or_error.Let_syntax in
    let%map entries = ls dir in
    List.is_empty entries
  ;;
end

module Make_raw (S : Multispool_intf.Spoolable.S) = struct
  type t = string [@@deriving sexp_of]

  type spool = t [@@deriving sexp_of]

  let dir t = t

  let reg_dir_of  dir = dir ^/ ".registry"
  let co_dir_of   dir = dir ^/ ".checkout"
  let tmp_dir_of  dir = dir ^/ ".tmp"
  let data_dir_of dir = dir ^/ ".data"

  let dir_looks_like_a_spool dir =
    let%bind reg_dir_exists  = Utils.is_dir (reg_dir_of dir)  in
    let%bind co_dir_exists   = Utils.is_dir (co_dir_of dir)   in
    let%bind tmp_dir_exists  = Utils.is_dir (tmp_dir_of dir)  in
    return (reg_dir_exists && co_dir_exists && tmp_dir_exists)
  ;;

  let visit_spool_directories ?create_if_missing dir =
    let open Deferred.Or_error.Let_syntax in
    let%bind () =
      if%bind Utils.is_dir_empty dir
      then
        Deferred.Or_error.ok_unit
      else
        Deferred.Or_error.error_s
          [%message "dir is not empty while creating new spool"
                      (dir : string)]
    in
    let reg_dir  = reg_dir_of  dir in
    let co_dir   = co_dir_of   dir in
    let tmp_dir  = tmp_dir_of  dir in
    let data_dir = data_dir_of dir in
    let%bind () = is_accessible_directory ?create_if_missing    reg_dir  in
    let%bind () = is_accessible_directory ?create_if_missing    co_dir   in
    let%bind () = is_accessible_directory ?create_if_missing    tmp_dir  in
    let%bind () = is_accessible_directory ~create_if_missing:() data_dir in
    Deferred.Or_error.ok_unit
  ;;

  let visit_queues ?create_if_missing dir =
    Deferred.Or_error.List.iter
      S.Queue.all
      ~f:(fun queue ->
        let queue_dir = dir ^/ (S.Queue.to_dirname queue) in
        is_accessible_directory ?create_if_missing queue_dir)
  ;;

  let load ?create_if_missing dir =
    is_accessible_directory ?create_if_missing dir
    >>=? fun () ->
    dir_looks_like_a_spool dir
    >>= begin function
    | true  -> Deferred.Or_error.ok_unit
    | false ->
      (* Spool directory does not look like a spool.  Fail if the directory is non-empty,
         otherwise create registry and checkout directories if specified.  This protects
         against creating spools in random directories with existing data *)
      visit_spool_directories ?create_if_missing dir
    end
    >>=? fun () ->
    visit_queues ?create_if_missing dir
    >>=? fun () ->
    Deferred.Or_error.return dir
  ;;

  let load_unsafe dir = dir

  let create = load ~create_if_missing:()

  let load_metadata path =
    let open Deferred.Or_error.Let_syntax in
    S.Throttle.enqueue (fun () ->
      let%bind contents = Deferred.Or_error.try_with (fun () ->
        Reader.file_contents path)
      in
      return (S.Metadata.of_string contents))
  ;;

  let save_metadata ?temp_file ~contents path =
    S.Throttle.enqueue (fun () ->
      Deferred.Or_error.try_with (fun () ->
        Writer.save path ?temp_file ~contents ~fsync:true))
  ;;

  module Data_file = struct
    type t =
      { spool : spool
      ; name  : string
      }

    let create spool name =
      { spool; name }
    ;;

    let load t =
      S.Throttle.enqueue (fun () ->
        S.Data.load (data_dir_of t.spool ^/ t.name))
    ;;

    let save t ~contents =
      S.Throttle.enqueue (fun () ->
        S.Data.save contents (data_dir_of t.spool ^/ t.name))
    ;;

    let stat t =
      Deferred.Or_error.try_with (fun () ->
        S.Throttle.enqueue (fun () ->
          Unix.stat (data_dir_of t.spool ^/ t.name)))
    ;;
  end

  (* An entry in a particular queue *)
  module Entry = struct
    type t =
      { spool : spool
      ; queue : S.Queue.t
      ; name  : string }
    [@@deriving fields, sexp_of]

    let create spool queue ~name =
      Fields.create ~spool ~queue ~name
    ;;

    let stat t =
      let open Deferred.Or_error.Let_syntax in
      match%bind
        S.Throttle.enqueue (fun () ->
          Utils.stat_or_notfound (reg_dir_of t.spool ^/ t.name))
      with
      | `Ok stat   -> Deferred.return (Ok stat)
      | `Not_found ->
        Deferred.Or_error.error_s [%message "No such entry" (t : t)]
    ;;

    module Direct = struct
      let contents t =
        let queue_dir = t.spool ^/ S.Queue.to_dirname t.queue in
        let path = queue_dir ^/ t.name in
        load_metadata path
      ;;

      let data_file t =
        Data_file.create t.spool t.name
      ;;

      let save t metadata =
        let path = t.spool ^/ S.Queue.to_dirname t.queue ^/ t.name in
        let temp_file = tmp_dir_of t.spool ^/ t.name in
        let contents = S.Metadata.to_string metadata in
        save_metadata ~temp_file ~contents path
      ;;

      let remove t =
        let open Deferred.Or_error.Let_syntax in
        let path = t.spool ^/ S.Queue.to_dirname t.queue ^/ t.name in
        S.Throttle.enqueue (fun () ->
          let%bind () = Utils.unlink path in
          let%bind () = Utils.unlink (data_dir_of t.spool ^/ t.name) in
          Utils.unlink (reg_dir_of t.spool ^/ t.name))
      ;;
    end
  end

  let list t queue =
    let queue_dir = t ^/ S.Queue.to_dirname queue in
    S.Throttle.enqueue (fun () -> Utils.ls_sorted queue_dir)
    >>|? fun entries ->
    List.map entries ~f:(fun name -> Entry.create t queue ~name)
  ;;

  module Unique_name = struct
    type t = string

    let reserve t opaque =
      let open Deferred.Or_error.Let_syntax in
      let rec try_name ~attempt =
        let%bind name =
          Deferred.Or_error.try_with
            (fun () -> Deferred.return (S.Name_generator.next opaque ~attempt))
        in
        let path = reg_dir_of t ^/ name in
        match%bind Utils.open_creat_excl_wronly path with
        | `Exists -> try_name ~attempt:(attempt + 1)
        | `Ok fd ->
          let%bind () = Deferred.Or_error.try_with (fun () -> Unix.close fd) in
          return name
      in
      S.Throttle.enqueue (fun () -> try_name ~attempt:0)
    ;;
  end

  let enqueue t queue metadata data unique_name =
    begin match unique_name with
    | `Reserve opaque -> Unique_name.reserve t opaque
    | `Use generated  -> Deferred.Or_error.return generated
    end
    >>=? fun name ->
    let entry = Entry.create t queue ~name in
    let meta = S.Metadata.to_string metadata in
    let temp_path = tmp_dir_of t ^/ name in
    let meta_path = t ^/ S.Queue.to_dirname queue ^/ name in
    let data_path = data_dir_of t ^/ name in
    Deferred.Or_error.try_with_join (fun () ->
      save_metadata temp_path ~contents:meta
      >>=? fun () ->
      S.Data.save data data_path
      >>=? fun () ->
      S.Throttle.enqueue (fun () ->
        Deferred.Or_error.try_with (fun () -> Unix.rename ~src:temp_path ~dst:meta_path)))
    >>= function
    | Error _  as error ->
      (* Best-effort cleanup: Unlink both files and ignore any errors *)
      S.Throttle.enqueue (fun () ->
        let%bind (_ : unit Or_error.t) = Utils.unlink temp_path in
        let%bind (_ : unit Or_error.t) = Utils.unlink meta_path in
        let%bind (_ : unit Or_error.t) = Utils.unlink data_path in
        let%bind (_ : unit Or_error.t) = Utils.unlink (reg_dir_of t ^/ name) in
        Deferred.unit)
      >>| fun () ->
      error
    | Ok () ->
      Deferred.Or_error.return entry
  ;;

  (* A checkout of an entry *)
  module Checked_out_entry = struct
    type t =
      { spool    : spool
      ; name     : string
      ; contents : S.Metadata.t }
    [@@deriving fields]

    let update t ~f = { t with contents = f t.contents }

    let data_file t =
      Data_file.create t.spool t.name
    ;;

    let save t queue =
      let open Deferred.Or_error.Let_syntax in
      let path = co_dir_of t.spool ^/ t.name in
      let temp_file = tmp_dir_of t.spool ^/ t.name in
      let contents = S.Metadata.to_string t.contents in
      let%bind () = save_metadata ~temp_file ~contents path in
      let queue_dir = t.spool ^/ S.Queue.to_dirname queue in
      let dst = queue_dir ^/ t.name in
      S.Throttle.enqueue (fun () ->
        Deferred.Or_error.try_with (fun () -> Unix.rename ~src:path ~dst))
    ;;

    let remove t =
      let open Deferred.Or_error.Let_syntax in
      S.Throttle.enqueue (fun () ->
        let%bind () = Utils.unlink (co_dir_of t.spool ^/ t.name) in
        let%bind () = Utils.unlink (data_dir_of t.spool ^/ t.name) in
        Utils.unlink (reg_dir_of t.spool ^/ t.name))
    ;;

    let create spool contents ~name = { spool; name; contents }
  end

  let checkout' (entry : Entry.t) =
    let open Deferred.Or_error.Let_syntax in
    let src = entry.spool ^/ S.Queue.to_dirname entry.queue ^/ entry.name in
    let dst = co_dir_of entry.spool ^/ entry.name in
    match%bind S.Throttle.enqueue (fun () -> Utils.try_to_rename ~src ~dst) with
    | `Not_found as x -> return x
    | `Ok () ->
      let%bind contents = load_metadata dst in
      return (`Ok (Checked_out_entry.create entry.spool ~name:entry.name contents))
  ;;

  (* Common handler for user-supplied callback functionality.  The low-level user-facing
     [with_entry] function uses this, as well as the higher-level [iter] and
     [iter_available] functions *)
  let with_checkout ~f checkout =
    let metadata = Checked_out_entry.contents checkout in
    let data_file = Checked_out_entry.data_file checkout in
    match%bind f metadata data_file with
    | `Remove, result ->
      Checked_out_entry.remove checkout
      >>=? fun () ->
      Deferred.Or_error.return result
    | `Save (new_contents, dst_queue), result ->
      let checkout = Checked_out_entry.update checkout ~f:(Fn.const new_contents) in
      Checked_out_entry.save checkout dst_queue
      >>=? fun () ->
      Deferred.Or_error.return result
  ;;

  let checkout (entry : Entry.t) =
    let open Deferred.Or_error.Let_syntax in
    match%bind checkout' entry with
    | `Ok file_co -> return file_co
    | `Not_found  ->
      Deferred.Or_error.error_s
        [%message "Entry disappeared before checkout"
                    (entry : Entry.t)]
  ;;

  let with_entry' ~f entry =
    let open Deferred.Or_error.Let_syntax in
    match%bind checkout' entry with
    | `Not_found as x -> return x
    | `Ok checkout ->
      let%map result = with_checkout ~f checkout in
      `Ok result
  ;;

  let with_entry ~f entry =
    let open Deferred.Or_error.Let_syntax in
    let%bind checkout = checkout entry in
    with_checkout ~f checkout
  ;;

  module Queue_reader_raw = struct
    type t =
      { spool                     : spool
      ; queue                     : S.Queue.t
      ; queue_dir                 : string
      ; lazy_inotify_pipe         : Async_inotify.Event.t Pipe.Reader.t Deferred.t Lazy.t
      ; entry_cache               : Entry.t list
      ; test_mode_last_moved_into : [ `Disabled | `Enabled of string option ]
      }

    let create_internal ~test_mode spool queue =
      let open Deferred.Or_error.Let_syntax in
      let queue_dir = spool ^/ S.Queue.to_dirname queue in
      let lazy_inotify_pipe = lazy (
        let open Deferred.Let_syntax in
        let%bind inotify, _ =
          Async_inotify.create queue_dir
            ~modify_event_selector:`Closed_writable_fd
        in
        return (Async_inotify.pipe inotify)
      ) in
      let test_mode_last_moved_into = match test_mode with
        | false -> `Disabled
        | true  -> `Enabled None
      in
      return
        { spool
        ; queue
        ; queue_dir
        ; lazy_inotify_pipe
        ; entry_cache = []
        ; test_mode_last_moved_into
        }
    ;;

    let most_recent_moved_into queue =
      let i = ref (Queue.length queue - 1) in
      let found = ref None in
      while !i >= 0 && !found = None do
        match (Queue.get queue !i : Async_inotify.Event.t) with
        | Moved Into path -> found := Some (Filename.basename path)
        | Queue_overflow
        | Unlinked   _
        | Modified   _
        | Moved Away _
        | Moved Move _
        | Created    _ -> decr i
      done;
      !found
    ;;

    let clear_inotify_pipe t =
      let open Deferred.Or_error.Let_syntax in
      let%bind inotify_pipe = Lazy.force t.lazy_inotify_pipe |> Deferred.ok in
      match Pipe.read_now' inotify_pipe with
      | `Eof ->
        Deferred.Or_error.error "inotify pipe closed unexpectedly"
          t.queue_dir [%sexp_of: string]
      | `Nothing_available -> return t
      | `Ok queue          ->
        match t.test_mode_last_moved_into with
        | `Disabled  -> return t
        | `Enabled _ ->
          let test_mode_last_moved_into = `Enabled (most_recent_moved_into queue) in
          return { t with test_mode_last_moved_into }
    ;;

    let wait_for_inotify_event t stop =
      let%bind inotify_pipe = Lazy.force t.lazy_inotify_pipe in
      let choices =
        [ choice (Pipe.read inotify_pipe) (fun x  -> `Pipe_read x)
        ; choice stop                     (fun () -> `Stopped)
        ]
      in
      match%map choose choices with
      | `Stopped -> Ok ()
      | `Pipe_read x ->
        (match x with
         | `Ok Queue_overflow
         | `Ok Unlinked   _
         | `Ok Modified   _
         | `Ok Moved Away _
         | `Ok Moved Move _
         | `Ok Moved Into _
         | `Ok Created    _ -> Ok ()
         | `Eof             ->
           Or_error.error "inotify pipe closed unexpectedly"
             t.queue_dir [%sexp_of: string])
    ;;

    let update_entry_cache t =
      let open Deferred.Or_error.Let_syntax in
      (* DO NOT CHANGE THE ORDER OF THESE OPERATIONS.  It is important to clear the
         inotify pipe _BEFORE_ listing the directory entries.  If the directory is read
         before clearing inotify, this may happen:

         - Directory listing
         - File is added and inotity event appears
         - inotify pipe is cleared, losing the above event

         The new file would not be seen until another inotify even triggers the next
         directory listing, which could be a long time!

         Why clear the inotify pipe at all?  We can't rely on the pipe having every event
         ever, as it is possible for the queue to overflow (the [Queue_overflow] event).
         So, we ultimately use inotify to tell us something has changed, and that change
         will be reflected in the directory listing.  Clearing the pipe is an optimization
         so that we don't iterate over a bunch of extraneous events. *)
      let%bind t = clear_inotify_pipe t in
      let%bind entry_cache = list t.spool t.queue in
      return { t with entry_cache }
    ;;

    let dequeue ?(stop = Deferred.never ()) t =
      let rec dequeue_loop t stop =
        let open Deferred.Or_error.Let_syntax in
        (* Poll for an interruption in case we're processing a large backlog of files *)
        match Deferred.peek stop with
        | Some () -> return `Stopped
        | None ->
          match t.entry_cache with
          | [] ->
            let%bind t = update_entry_cache t in
            let%bind () =
              if List.is_empty t.entry_cache
              then
                (* Directory is currently empty, wait for inotify to notify us of queue
                   activity.  We really use this as an indication that something has
                   changed to trigger a new directory listing.  Any inotify event will
                   satisfy [wait_for_inotify_event], even though we probably only care
                   about [Moved Into] events (and [Queue_overflow], in theory). *)
                wait_for_inotify_event t stop
              else Deferred.Or_error.ok_unit
            in
            dequeue_loop t stop
          | entry :: entry_cache ->
            let entry_cache =
              match t.test_mode_last_moved_into with

              | `Enabled (Some last_moved_into) when Entry.name entry = last_moved_into ->
                (* TESTING ONLY.  Treat the file referenced by the most recent [Moved
                   Into] inotify event as the last cache entry so that we do not process
                   any files that appear in a queue during a traversal with readdir(3).
                   This lets a test validate that order is preserved if readdir(3)'s
                   non-determinism is eliminated.  For this to work, the test must use a
                   [Name_generator.next] implementation that guarantees that lexicographic
                   ordering is the same as time ordering
                   (e.g. [For_testing.Lexicographic_time_order_name_generator].  See
                   test/feature_subtree/feature_test_multispool.ml for dependent tests. *)
                []
              | _ -> entry_cache
            in
            let t = { t with entry_cache } in
            let open Deferred.Or_error.Let_syntax in
            match%bind checkout' entry with
            | `Ok checkout -> return (`Checked_out (checkout, t))
            | `Not_found   -> dequeue_loop t stop
      in
      dequeue_loop t stop
    ;;

    let rec dequeue_available_loop t =
      match t.entry_cache with
      | [] ->
        Deferred.Or_error.return (`Nothing_available, t)
      | entry :: entry_cache ->
        let open Deferred.Or_error.Let_syntax in
        let t = { t with entry_cache } in
        match%bind checkout' entry with
        | `Ok checkout -> return (`Checked_out checkout, t)
        | `Not_found   -> dequeue_available_loop t
    ;;

    let dequeue_available t =
      let open Deferred.Or_error.Let_syntax in
      let%bind entry_cache = list t.spool t.queue in
      dequeue_available_loop { t with entry_cache }
    ;;

    let run_iteration_unit f s data_file =
      let%map result = f s data_file in
      result, ()
    ;;

    let rec iter ?stop ~f t =
      let open Deferred.Or_error.Let_syntax in
      match%bind dequeue ?stop t with
      | `Stopped -> return ()
      | `Checked_out (checkout, t) ->
        let%bind () = with_checkout checkout ~f:(run_iteration_unit f) in
        iter ?stop ~f t
    ;;

    let rec iter_available_loop ~f t =
      let open Deferred.Or_error.Let_syntax in
      let%bind maybe_checkout, t = dequeue_available_loop t in
      match maybe_checkout with
      | `Nothing_available -> return ()
      | `Checked_out checkout ->
        let%bind () = with_checkout checkout ~f:(run_iteration_unit f) in
        iter_available_loop ~f t
    ;;

    let iter_available ~f t =
      let open Deferred.Or_error.Let_syntax in
      let%bind entry_cache = list t.spool t.queue in
      iter_available_loop ~f { t with entry_cache }
    ;;
  end

  module Expert = struct
    module Checked_out_entry = Checked_out_entry
    let checkout = checkout
    let checkout' = checkout'
    module Queue_reader = struct
      let dequeue = Queue_reader_raw.dequeue
      let dequeue_available = Queue_reader_raw.dequeue_available
    end
  end
end

module Make (S : Multispool_intf.Spoolable.S) = struct
  include Make_raw(S)

  module Queue_reader = struct
    include Queue_reader_raw
    let create spool queue =
      Queue_reader_raw.create_internal spool queue ~test_mode:false
    ;;
  end
end

module For_testing = struct
  module Lexicographic_time_order_name_generator = struct
    type t = int

    (* Ensure lexicographic ordering is the same as time ordering. *)
    let next i ~attempt =
      let now = Time.to_filename_string (Time.now ()) ~zone:Time.Zone.utc in
      let attempt_str = sprintf "%06d" attempt in
      now ^  "_" ^ attempt_str ^ "__" ^ Int.to_string i ^ "__"
    ;;
  end

  module Make (S : Multispool_intf.Spoolable.S) = struct
    include Make_raw(S)

    module Queue_reader = struct
      include Queue_reader_raw
      let create spool queue =
        Queue_reader_raw.create_internal spool queue ~test_mode:true
      ;;
    end
  end
end
