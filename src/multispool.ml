open Core
open Poly
open Async
open Common
module Time = Time_float_unix

(* Various shared utility functions *)
module Utils = struct
  let replace_unix_error ~error ~replacement = function
    | Error e ->
      let exn = Monitor.extract_exn (Error.to_exn e) in
      (match exn with
       | Unix.Unix_error (err, _, _) when err = error -> Ok replacement
       | e -> Or_error.of_exn e)
    | Ok retval -> Ok (`Ok retval)
  ;;

  let open_creat_excl_wronly ?perm path =
    Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
      Unix.openfile path ?perm ~mode:[ `Creat; `Excl; `Wronly ])
    >>| replace_unix_error ~error:EEXIST ~replacement:`Exists
  ;;

  let touch path =
    Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
      let tod = Unix.gettimeofday () in
      Unix.utimes path ~access:tod ~modif:tod)
  ;;

  let unlink path =
    Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () -> Unix.unlink path)
  ;;

  let stat_or_notfound path =
    Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () -> Unix.stat path)
    >>| replace_unix_error ~error:ENOENT ~replacement:`Not_found
  ;;

  let ls dir =
    Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () -> Sys.ls_dir dir)
  ;;

  let ls_sorted dir =
    let open Deferred.Or_error.Let_syntax in
    let%bind entries = ls dir in
    return (List.sort ~compare:String.compare entries)
  ;;

  let is_dir path =
    match%map Sys.is_directory path with
    | `Yes -> true
    | `No | `Unknown -> false
  ;;

  let is_dir_empty dir =
    let open Deferred.Or_error.Let_syntax in
    let%map entries = ls dir in
    List.is_empty entries
  ;;
end

(* Spool loading and creation functionality shared by [Make] and [Monitor.Make] functors *)
module Shared = struct
  type spool = string [@@deriving sexp]

  let registry = ".registry"
  let tmp = ".tmp"
  let checkout = ".checkout"
  let data = ".data"
  let reg_dir_of dir = dir ^/ registry
  let co_dir_of dir queue = dir ^/ queue ^/ checkout
  let tmp_dir_of dir = dir ^/ tmp
  let data_dir_of dir = dir ^/ data

  let dir_looks_like_a_spool dir =
    let%bind reg_dir_exists = Utils.is_dir (reg_dir_of dir) in
    let%bind tmp_dir_exists = Utils.is_dir (tmp_dir_of dir) in
    return (reg_dir_exists && tmp_dir_exists)
  ;;

  let visit_queues ?create_if_missing dir queue_dirnames =
    let open Deferred.Or_error.Let_syntax in
    Deferred.Or_error.List.iter ~how:`Sequential queue_dirnames ~f:(fun queue_dirname ->
      let queue_dir = dir ^/ queue_dirname in
      let%bind () = is_accessible_directory ?create_if_missing queue_dir in
      let%bind () = is_accessible_directory ?create_if_missing (queue_dir ^/ checkout) in
      return ())
  ;;

  let visit_spool_directories ?create_if_missing dir =
    let open Deferred.Or_error.Let_syntax in
    let%bind () =
      if%bind Utils.is_dir_empty dir
      then Deferred.Or_error.ok_unit
      else
        Deferred.Or_error.error_s
          [%message "dir is not empty while creating new spool" (dir : string)]
    in
    let reg_dir = reg_dir_of dir in
    let tmp_dir = tmp_dir_of dir in
    let data_dir = data_dir_of dir in
    let%bind () = is_accessible_directory ?create_if_missing reg_dir in
    let%bind () = is_accessible_directory ?create_if_missing tmp_dir in
    let%bind () = is_accessible_directory ~create_if_missing:() data_dir in
    Deferred.Or_error.ok_unit
  ;;

  let load_spool ?create_if_missing ~queue_dirnames dir =
    is_accessible_directory ?create_if_missing dir
    >>=? fun () ->
    (match%bind dir_looks_like_a_spool dir with
     | true -> Deferred.Or_error.ok_unit
     | false ->
       (* Spool directory does not look like a spool. Fail if the directory is non-empty,
          otherwise create registry and checkout directories if specified. This protects
          against creating spools in random directories with existing data *)
       visit_spool_directories ?create_if_missing dir)
    >>=? fun () ->
    visit_queues ?create_if_missing dir queue_dirnames
    >>=? fun () -> Deferred.Or_error.return dir
  ;;
end

module Make_base (S : Multispool_intf.Spoolable.S) = struct
  include Shared

  type t = spool [@@deriving sexp]

  let dir t = t

  let load =
    let queue_dirnames =
      List.map S.Queue.all ~f:(fun queue -> S.Queue.to_dirname queue)
    in
    load_spool ~queue_dirnames
  ;;

  let load_unsafe dir = dir
  let create = load ~create_if_missing:()

  let load_metadata path =
    S.Throttle.enqueue (fun () ->
      Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
        let%bind contents = Reader.file_contents path in
        return (S.Metadata.of_string contents)))
  ;;

  let save_metadata ?temp_file ~contents path =
    S.Throttle.enqueue (fun () ->
      Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
        Writer.save path ?temp_file ~contents ~fsync:true))
  ;;

  module Data_file = struct
    type t =
      { spool : spool
      ; name : string
      }

    let create spool name = { spool; name }
    let path t = data_dir_of t.spool ^/ t.name
    let load t = S.Throttle.enqueue (fun () -> S.Data.load (path t))
    let save t ~contents = S.Throttle.enqueue (fun () -> S.Data.save contents (path t))

    let stat t =
      Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
        S.Throttle.enqueue (fun () -> Unix.stat (data_dir_of t.spool ^/ t.name)))
    ;;
  end

  module With_touch = struct
    (* [With_touch] encapsulates filesystem operations and supports [Monitor]'s
       [alert_after_cycles] parameter. The mtimes on .registry/ files and any files
       involved in a rename are updated, creating a distinct (potential) [Problem.t] for
       each phase of an entry's life. This way, similar phases of an entry's life won't
       create identical [Problem.t]s (which could lead to spurious alerts). *)

    let unlink ~dir ~filename t =
      let path = dir ^/ filename in
      let reg_path = reg_dir_of t ^/ filename in
      let data_path = data_dir_of t ^/ filename in
      S.Throttle.enqueue (fun () ->
        Utils.touch reg_path
        >>=? fun () ->
        Utils.unlink data_path
        >>=? fun () -> Utils.unlink path >>=? fun () -> Utils.unlink reg_path)
    ;;

    let rename ~src_dir ~dst_dir ~filename t =
      let src = src_dir ^/ filename in
      let dst = dst_dir ^/ filename in
      let reg_path = reg_dir_of t ^/ filename in
      S.Throttle.enqueue (fun () ->
        Utils.touch reg_path
        >>=? fun () ->
        Utils.touch src
        >>=? fun () ->
        Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
          Unix.rename ~src ~dst))
    ;;

    let rename' ~src_dir ~dst_dir ~filename t =
      rename ~src_dir ~dst_dir ~filename t
      >>| Utils.replace_unix_error ~error:ENOENT ~replacement:`Not_found
    ;;
  end

  (* An entry in a particular queue *)
  module Entry = struct
    type t =
      { spool : spool
      ; queue : S.Queue.t
      ; name : string
      }
    [@@deriving fields ~getters ~iterators:create, sexp_of]

    let create spool queue ~name = Fields.create ~spool ~queue ~name

    let stat t =
      let open Deferred.Or_error.Let_syntax in
      match%bind
        S.Throttle.enqueue (fun () ->
          Utils.stat_or_notfound (reg_dir_of t.spool ^/ t.name))
      with
      | `Ok stat -> Deferred.return (Ok stat)
      | `Not_found -> Deferred.Or_error.error_s [%message "No such entry" (t : t)]
    ;;

    module Direct = struct
      let contents t =
        let queue_dir = t.spool ^/ S.Queue.to_dirname t.queue in
        let path = queue_dir ^/ t.name in
        load_metadata path
      ;;

      let data_file t = Data_file.create t.spool t.name

      let save t metadata =
        let path = t.spool ^/ S.Queue.to_dirname t.queue ^/ t.name in
        let temp_file = tmp_dir_of t.spool ^/ t.name in
        let contents = S.Metadata.to_string metadata in
        save_metadata ~temp_file ~contents path
      ;;

      let remove t =
        With_touch.unlink
          t.spool
          ~dir:(t.spool ^/ S.Queue.to_dirname t.queue)
          ~filename:t.name
      ;;
    end
  end

  let list t queue =
    let queue_dir = t ^/ S.Queue.to_dirname queue in
    S.Throttle.enqueue (fun () -> Utils.ls_sorted queue_dir)
    >>|? fun entries ->
    List.filter_map entries ~f:(fun name ->
      (* Checkout directories live within queue directories. We want to skip over these. *)
      if String.equal name checkout then None else Some (Entry.create t queue ~name))
  ;;

  module Unique_name = struct
    let reserve t opaque =
      let open Deferred.Or_error.Let_syntax in
      let rec try_name ~attempt =
        let%bind name =
          Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
            Deferred.return (S.Name_generator.next opaque ~attempt))
        in
        let path = reg_dir_of t ^/ S.Name_generator.Unique_name.to_string name in
        match%bind Utils.open_creat_excl_wronly path with
        | `Exists -> try_name ~attempt:(attempt + 1)
        | `Ok fd ->
          let%bind () =
            Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () -> Unix.close fd)
          in
          return name
      in
      S.Throttle.enqueue (fun () -> try_name ~attempt:0)
    ;;
  end

  let enqueue t queue metadata data unique_name =
    (match unique_name with
     | `Reserve opaque -> Unique_name.reserve t opaque
     | `Use generated -> Deferred.Or_error.return generated)
    >>=? fun name ->
    let name = S.Name_generator.Unique_name.to_string name in
    let entry = Entry.create t queue ~name in
    let meta = S.Metadata.to_string metadata in
    let temp_path = tmp_dir_of t ^/ name in
    let meta_path = t ^/ S.Queue.to_dirname queue ^/ name in
    let data_path = data_dir_of t ^/ name in
    match%bind
      Deferred.Or_error.try_with_join ~run:`Schedule ~rest:`Log (fun () ->
        save_metadata temp_path ~contents:meta
        >>=? fun () ->
        S.Data.save data data_path
        >>=? fun () ->
        S.Throttle.enqueue (fun () ->
          Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
            Unix.rename ~src:temp_path ~dst:meta_path)))
    with
    | Error _ as error ->
      (* Best-effort cleanup: Unlink both files and ignore any errors *)
      let%map () =
        S.Throttle.enqueue (fun () ->
          let%bind (_ : unit Or_error.t) = Utils.unlink temp_path in
          let%bind (_ : unit Or_error.t) = Utils.unlink meta_path in
          let%bind (_ : unit Or_error.t) = Utils.unlink data_path in
          let%bind (_ : unit Or_error.t) = Utils.unlink (reg_dir_of t ^/ name) in
          Deferred.unit)
      in
      error
    | Ok () -> Deferred.Or_error.return entry
  ;;

  (* A checkout of an entry *)
  module Checked_out_entry = struct
    type t =
      { spool : spool
      ; queue : S.Queue.t
      ; name : string
      ; contents : S.Metadata.t
      }

    let create spool queue contents ~name = { spool; queue; name; contents }
    let name t = t.name
    let queue t = t.queue
    let contents t = t.contents
    let update t ~f = { t with contents = f t.contents }
    let data_file t = Data_file.create t.spool t.name
    let co_dir t = co_dir_of t.spool (S.Queue.to_dirname t.queue)

    let save t queue =
      let open Deferred.Or_error.Let_syntax in
      let path = co_dir t ^/ t.name in
      let temp_file = tmp_dir_of t.spool ^/ t.name in
      let contents = S.Metadata.to_string t.contents in
      let%bind () = save_metadata ~temp_file ~contents path in
      With_touch.rename
        t.spool
        ~src_dir:(co_dir t)
        ~dst_dir:(t.spool ^/ S.Queue.to_dirname queue)
        ~filename:t.name
    ;;

    let remove t = With_touch.unlink t.spool ~dir:(co_dir t) ~filename:t.name
  end

  let checkout' (entry : Entry.t) =
    let open Deferred.Or_error.Let_syntax in
    let checkout_dir = co_dir_of entry.spool (S.Queue.to_dirname entry.queue) in
    match%bind
      With_touch.rename'
        entry.spool
        ~src_dir:(entry.spool ^/ S.Queue.to_dirname entry.queue)
        ~dst_dir:checkout_dir
        ~filename:entry.name
    with
    | `Not_found as x -> return x
    | `Ok () ->
      let path = checkout_dir ^/ entry.name in
      let%bind contents = load_metadata path in
      return
        (`Ok (Checked_out_entry.create entry.spool entry.queue ~name:entry.name contents))
  ;;

  (* Common handler for user-supplied callback functionality. The low-level user-facing
     [with_entry] function uses this, as well as the higher-level [iter] and
     [iter_available] functions *)
  let with_checkout ~f checkout =
    let metadata = Checked_out_entry.contents checkout in
    let data_file = Checked_out_entry.data_file checkout in
    match%bind f metadata data_file with
    | `Remove, result ->
      Checked_out_entry.remove checkout >>=? fun () -> Deferred.Or_error.return result
    | `Save (new_contents, dst_queue), result ->
      let checkout = Checked_out_entry.update checkout ~f:(Fn.const new_contents) in
      Checked_out_entry.save checkout dst_queue
      >>=? fun () -> Deferred.Or_error.return result
  ;;

  let checkout (entry : Entry.t) =
    let open Deferred.Or_error.Let_syntax in
    match%bind checkout' entry with
    | `Ok file_co -> return file_co
    | `Not_found ->
      Deferred.Or_error.error_s
        [%message "Entry disappeared before checkout" (entry : Entry.t)]
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

  let list_checkouts_unsafe spool queue =
    let open Deferred.Or_error.Let_syntax in
    let checkout_dir = co_dir_of spool (S.Queue.to_dirname queue) in
    let%bind entries = S.Throttle.enqueue (fun () -> Utils.ls_sorted checkout_dir) in
    Deferred.Or_error.List.map ~how:`Sequential entries ~f:(fun name ->
      let%map metadata = load_metadata (checkout_dir ^/ name) in
      Checked_out_entry.create spool queue metadata ~name)
  ;;

  module Queue_reader_raw = struct
    type t =
      { spool : spool
      ; queue : S.Queue.t
      ; queue_dir : string
      ; lazy_inotify_pipe : Async_inotify.Event.t Pipe.Reader.t Deferred.t Lazy.t
      ; entry_cache : Entry.t list
      ; test_mode_last_moved_into : [ `Disabled | `Enabled of string option ]
      }

    let create_internal ~test_mode spool queue =
      let open Deferred.Or_error.Let_syntax in
      let queue_dir = spool ^/ S.Queue.to_dirname queue in
      let lazy_inotify_pipe =
        lazy
          (let open Deferred.Let_syntax in
           let%bind _, _, pipe =
             Async_inotify.create queue_dir ~modify_event_selector:`Closed_writable_fd
           in
           return pipe)
      in
      let test_mode_last_moved_into =
        match test_mode with
        | false -> `Disabled
        | true -> `Enabled None
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
        | Moved (Into path) -> found := Some (Filename.basename path)
        | Queue_overflow | Unlinked _ | Modified _
        | Moved (Away _)
        | Moved (Move _)
        | Created _ -> decr i
      done;
      !found
    ;;

    let clear_inotify_pipe t =
      let open Deferred.Or_error.Let_syntax in
      let%bind inotify_pipe = Lazy.force t.lazy_inotify_pipe |> Deferred.ok in
      match Pipe.read_now' inotify_pipe with
      | `Eof ->
        Deferred.Or_error.error
          "inotify pipe closed unexpectedly"
          t.queue_dir
          [%sexp_of: string]
      | `Nothing_available -> return t
      | `Ok queue ->
        (match t.test_mode_last_moved_into with
         | `Disabled -> return t
         | `Enabled _ ->
           let test_mode_last_moved_into = `Enabled (most_recent_moved_into queue) in
           return { t with test_mode_last_moved_into })
    ;;

    let wait_for_inotify_event t stop =
      let%bind inotify_pipe = Lazy.force t.lazy_inotify_pipe in
      let choices =
        [ choice (Pipe.read inotify_pipe) (fun x -> `Pipe_read x)
        ; choice stop (fun () -> `Stopped)
        ]
      in
      match%map choose choices with
      | `Stopped -> Ok ()
      | `Pipe_read x ->
        (match x with
         | `Ok Queue_overflow
         | `Ok (Unlinked _)
         | `Ok (Modified _)
         | `Ok (Moved (Away _))
         | `Ok (Moved (Move _))
         | `Ok (Moved (Into _))
         | `Ok (Created _) -> Ok ()
         | `Eof ->
           Or_error.error
             "inotify pipe closed unexpectedly"
             t.queue_dir
             [%sexp_of: string])
    ;;

    let update_entry_cache t =
      let open Deferred.Or_error.Let_syntax in
      (* DO NOT CHANGE THE ORDER OF THESE OPERATIONS. It is important to clear the inotify
         pipe _BEFORE_ listing the directory entries. If the directory is read before
         clearing inotify, this may happen:

         - Directory listing
         - File is added and inotity event appears
         - inotify pipe is cleared, losing the above event

         The new file would not be seen until another inotify even triggers the next
         directory listing, which could be a long time!

         Why clear the inotify pipe at all? We can't rely on the pipe having every event
         ever, as it is possible for the queue to overflow (the [Queue_overflow] event).
         So, we ultimately use inotify to tell us something has changed, and that change
         will be reflected in the directory listing. Clearing the pipe is an optimization
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
          (match t.entry_cache with
           | [] ->
             let%bind t = update_entry_cache t in
             let%bind () =
               if List.is_empty t.entry_cache
               then
                 (* Directory is currently empty, wait for inotify to notify us of queue
                    activity. We really use this as an indication that something has
                    changed to trigger a new directory listing. Any inotify event will
                    satisfy [wait_for_inotify_event], even though we probably only care
                    about [Moved Into] events (and [Queue_overflow], in theory). *)
                 wait_for_inotify_event t stop
               else Deferred.Or_error.ok_unit
             in
             dequeue_loop t stop
           | entry :: entry_cache ->
             let entry_cache =
               match t.test_mode_last_moved_into with
               | `Enabled (Some last_moved_into) when Entry.name entry = last_moved_into
                 ->
                 (* TESTING ONLY. Treat the file referenced by the most recent
                    [Moved Into] inotify event as the last cache entry so that we do not
                    process any files that appear in a queue during a traversal with
                    readdir(3). This lets a test validate that order is preserved if
                    readdir(3)'s non-determinism is eliminated. For this to work, the test
                    must use a [Name_generator.next] implementation that guarantees that
                    lexicographic ordering is the same as time ordering (e.g.
                    [For_testing.Lexicographic_time_order_name_generator]. See
                    test/feature_subtree/feature_test_multispool.ml for dependent tests. *)
                 []
               | _ -> entry_cache
             in
             let t = { t with entry_cache } in
             let open Deferred.Or_error.Let_syntax in
             (match%bind checkout' entry with
              | `Ok checkout -> return (`Checked_out (checkout, t))
              | `Not_found -> dequeue_loop t stop))
      in
      dequeue_loop t stop
    ;;

    let rec dequeue_available_loop t =
      match t.entry_cache with
      | [] -> Deferred.Or_error.return (`Nothing_available, t)
      | entry :: entry_cache ->
        let open Deferred.Or_error.Let_syntax in
        let t = { t with entry_cache } in
        (match%bind checkout' entry with
         | `Ok checkout -> return (`Checked_out checkout, t)
         | `Not_found -> dequeue_available_loop t)
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
    let list_checkouts_unsafe = list_checkouts_unsafe

    module Queue_reader = struct
      let dequeue = Queue_reader_raw.dequeue
      let dequeue_available = Queue_reader_raw.dequeue_available
    end
  end
end

module Make (S : Multispool_intf.Spoolable.S) = struct
  include Make_base (S)

  module Queue_reader = struct
    include Queue_reader_raw

    let create spool queue = Queue_reader_raw.create_internal spool queue ~test_mode:false
  end
end

module Monitor = struct
  module Make (S : Multispool_intf.Spoolable.S) = struct
    include Shared

    module File_with_mtime = struct
      module T = struct
        type t =
          { filename : string
          ; mtime : Time.t
          }
        [@@deriving sexp_of, compare]

        let sexp_of_t t =
          if Ppx_inline_test_lib.am_running
          then [%sexp { filename = (t.filename : string); mtime = "<omitted>" }]
          else sexp_of_t t
        ;;
      end

      include T
      include Comparable.Make_plain (T)
    end

    module Dir = struct
      module T = struct
        type t =
          | Registry
          | Tmp
          | Data
          | Queue of S.Queue.t
          | Queue_checkout of S.Queue.t
        [@@deriving sexp_of, enumerate, compare]
      end

      include T
      include Comparable.Make_plain (T)

      let name_on_disk = function
        | Registry -> registry
        | Tmp -> tmp
        | Data -> data
        | Queue q -> S.Queue.to_dirname q
        | Queue_checkout q -> S.Queue.to_dirname q ^/ checkout
      ;;
    end

    module Problem = struct
      module T = struct
        type t =
          | Too_old of File_with_mtime.t * Dir.t
          | Orphaned of File_with_mtime.t * Dir.t
          | Duplicated of File_with_mtime.t * Dir.t list
        [@@deriving sexp_of, compare]
      end

      include T
      include Comparable.Make_plain (T)
    end

    module Event = struct
      module T = struct
        type t =
          | Start of Time.t * Problem.t
          | End of Time.t * Problem.t
        [@@deriving sexp_of, compare]
      end

      include T
      include Comparable.Make_plain (T)
    end

    module Limits = struct
      type t =
        { max_checked_out_age : Time.Span.t
        ; max_tmp_file_age : Time.Span.t
        ; max_queue_ages : (S.Queue.t * Time.Span.t) list
        }
      [@@deriving sexp]

      module Defaults = struct
        let max_checked_out_age = Time.Span.of_min 10.
        let max_tmp_file_age = Time.Span.of_min 10.
      end

      let create
        ?(max_checked_out_age = Defaults.max_checked_out_age)
        ?(max_tmp_file_age = Defaults.max_tmp_file_age)
        ?(max_queue_ages = [])
        ()
        =
        { max_checked_out_age; max_tmp_file_age; max_queue_ages }
      ;;

      let param =
        let open Command.Let_syntax in
        [%map_open
          let max_checked_out_age =
            flag_optional_with_default_doc_sexp
              "-max-checked-out-age"
              Time.Span.arg_type
              Time.Span.sexp_of_t
              ~default:Defaults.max_checked_out_age
              ~doc:"SPAN alert once idle checkout reaches age SPAN"
          and max_tmp_file_age =
            flag_optional_with_default_doc_sexp
              "-max-tmp-file-age"
              Time.Span.arg_type
              Time.Span.sexp_of_t
              ~default:Defaults.max_tmp_file_age
              ~doc:"SPAN alert once temporary file reaches age SPAN"
          and max_queue_ages =
            all
              (List.map S.Queue.all ~f:(fun queue ->
                 let dirname = S.Queue.to_dirname queue in
                 let name = sprintf "-max-%s-age" dirname in
                 flag
                   name
                   (optional Time.Span.arg_type)
                   ~doc:
                     (sprintf
                        "SPAN alert if file in queue `%s' reaches age SPAN (default: No \
                         limit)"
                        dirname)
                 |> map ~f:(fun span -> queue, span)))
          in
          let max_queue_ages =
            List.filter_map max_queue_ages ~f:(function
              | _, None -> None
              | queue, Some span -> Some (queue, span))
          in
          create ~max_checked_out_age ~max_tmp_file_age ~max_queue_ages ()]
      ;;
    end

    module Spec = struct
      type t =
        { spool_dir : string
        ; limits : Limits.t
        }
      [@@deriving fields ~iterators:create, sexp]

      let create = Fields.create

      let param =
        let open Command.Let_syntax in
        [%map_open
          let spool_dir = anon ("SPOOL_DIR" %: string)
          and limits = Limits.param in
          create ~spool_dir ~limits]
      ;;
    end

    type t =
      { spool : spool
      ; limits : Limits.t
      ; mutable private_problems : int Problem.Map.t
      ; mutable public_problems : Problem.Set.t
      }

    let create { Spec.spool_dir; limits } =
      let open Deferred.Or_error.Let_syntax in
      let queue_dirnames =
        List.map S.Queue.all ~f:(fun queue -> S.Queue.to_dirname queue)
      in
      let%bind spool = load_spool ~queue_dirnames spool_dir in
      return
        { spool
        ; limits
        ; private_problems = Problem.Map.empty
        ; public_problems = Problem.Set.empty
        }
    ;;

    module Spool_files = struct
      type t = File_with_mtime.t String.Map.t Dir.Map.t

      let collect spool : t Deferred.Or_error.t =
        let open Deferred.Or_error.Let_syntax in
        let%bind dir_alist =
          Deferred.Or_error.List.map ~how:`Sequential Dir.all ~f:(fun dir ->
            let path = spool ^/ Dir.name_on_disk dir in
            let%bind files = Utils.ls path in
            let%bind filename_alist =
              Deferred.Or_error.List.filter_map ~how:`Sequential files ~f:(fun filename ->
                match%map Utils.stat_or_notfound (path ^/ filename) with
                | `Not_found -> None
                | `Ok stats ->
                  (* Checkout directories live within queue directories. We want to skip
                     over these. *)
                  if String.equal filename checkout
                  then None
                  else (
                    let mtime = Unix.Stats.mtime stats in
                    let fwm = { File_with_mtime.filename; mtime } in
                    Some (filename, fwm)))
            in
            let filename_map = String.Map.of_alist_exn filename_alist in
            return (dir, filename_map))
        in
        return (Dir.Map.of_alist_exn dir_alist)
      ;;

      let filename_map t dir = Map.find_exn t dir
    end

    let fsck t ~now =
      let open Deferred.Or_error.Let_syntax in
      let%bind spool_files = Spool_files.collect t.spool in
      let filter_fold_over_dir dir ~f init =
        let filename_map = Spool_files.filename_map spool_files dir in
        Map.fold filename_map ~init ~f:(fun ~key:_ ~data:fwm accum ->
          match f fwm with
          | None -> accum
          | Some a -> a :: accum)
      in
      let find_file_too_old ~age dir =
        filter_fold_over_dir dir ~f:(fun fwm ->
          Option.some_if (Time.diff now fwm.mtime > age) (Problem.Too_old (fwm, dir)))
      in
      let find_queue_file_too_old queue =
        match
          List.Assoc.find t.limits.max_queue_ages queue ~equal:[%compare.equal: S.Queue.t]
        with
        | None -> Fn.id
        | Some age -> find_file_too_old (Dir.Queue queue) ~age
      in
      let find_file_without_registry_file dir =
        let registry_map = Spool_files.filename_map spool_files Registry in
        filter_fold_over_dir dir ~f:(fun fwm ->
          Option.some_if
            (not (Map.mem registry_map fwm.filename))
            (Problem.Orphaned (fwm, dir)))
      in
      let find_orphaned_registry_file_and_duplicates =
        let dupe_dirs =
          List.concat_map S.Queue.all ~f:(fun q -> [ Dir.Queue q; Dir.Queue_checkout q ])
        in
        filter_fold_over_dir Dir.Registry ~f:(fun fwm ->
          let dupes =
            List.filter_map dupe_dirs ~f:(fun dir ->
              let filename_map = Spool_files.filename_map spool_files dir in
              Option.some_if (Map.mem filename_map fwm.filename) dir)
          in
          match List.length dupes with
          | 0 -> Some (Problem.Orphaned (fwm, Registry))
          | 1 -> None
          | _ -> Some (Problem.Duplicated (fwm, dupes)))
      in
      []
      |> find_file_too_old Dir.Tmp ~age:t.limits.max_tmp_file_age
      |> find_file_without_registry_file Dir.Tmp
      |> find_file_without_registry_file Dir.Data
      |> fun init ->
      List.fold S.Queue.all ~init ~f:(fun problems queue ->
        problems
        |> find_queue_file_too_old queue
        |> find_file_too_old (Dir.Queue_checkout queue) ~age:t.limits.max_checked_out_age
        |> find_file_without_registry_file (Dir.Queue queue)
        |> find_file_without_registry_file (Dir.Queue_checkout queue))
      |> find_orphaned_registry_file_and_duplicates
      |> return
    ;;

    let fsck_and_update_private_problems t ~now =
      let open Deferred.Or_error.Let_syntax in
      let%bind problems = fsck t ~now in
      let old_private_problems = t.private_problems in
      let private_problems =
        List.fold problems ~init:Problem.Map.empty ~f:(fun private_problems p ->
          let old_count = Map.find old_private_problems p |> Option.value ~default:0 in
          Map.set private_problems ~key:p ~data:(old_count + 1))
      in
      t.private_problems <- private_problems;
      Deferred.Or_error.ok_unit
    ;;

    let fsck_and_eventify t ~alert_after_cycles =
      let open Deferred.Or_error.Let_syntax in
      let now = Time.now () in
      let%bind () = fsck_and_update_private_problems t ~now in
      let old_problems = t.public_problems in
      let new_problems =
        Map.fold
          t.private_problems
          ~init:Problem.Set.empty
          ~f:(fun ~key:problem ~data:count new_problems ->
            if count > alert_after_cycles
            then Set.add new_problems problem
            else new_problems)
      in
      let starts = Set.to_list (Set.diff new_problems old_problems) in
      let ends = Set.to_list (Set.diff old_problems new_problems) in
      let start_events = List.map starts ~f:(fun p -> Event.Start (now, p)) in
      let end_events = List.map ends ~f:(fun p -> Event.End (now, p)) in
      t.public_problems <- new_problems;
      return (end_events @ start_events)
    ;;

    let run_once t = fsck t ~now:(Time.now ())

    module Daemon = struct
      type monitor = t

      type t =
        { check_every : Time.Span.t
        ; alert_after_cycles : int
        }

      module Defaults = struct
        let check_every = Time.Span.of_sec 15.
        let alert_after_cycles = 2
      end

      let create
        ?(check_every = Defaults.check_every)
        ?(alert_after_cycles = Defaults.alert_after_cycles)
        ()
        =
        { check_every; alert_after_cycles }
      ;;

      let param =
        let open Command.Let_syntax in
        [%map_open
          let check_every =
            flag_optional_with_default_doc_sexp
              "-check-every"
              Time.Span.arg_type
              Time.Span.sexp_of_t
              ~default:Defaults.check_every
              ~doc:"SPAN run fsck at intervals of SPAN"
          and alert_after_cycles =
            flag_optional_with_default_doc_sexp
              "-alert-after-cycles"
              int
              Int.sexp_of_t
              ~default:Defaults.alert_after_cycles
              ~doc:"INT alert after problem is seen INT times"
          in
          create ~check_every ~alert_after_cycles ()]
      ;;

      let start { check_every; alert_after_cycles } ~monitor ~f =
        Clock.every' check_every (fun () ->
          let%bind events =
            fsck_and_eventify monitor ~alert_after_cycles |> Deferred.Or_error.ok_exn
          in
          Deferred.List.iter ~how:`Sequential events ~f:(fun e -> f e))
      ;;
    end
  end
end

module For_testing = struct
  module Lexicographic_time_order_name_generator = struct
    module Unique_name = String

    type t = int

    (* Ensure lexicographic ordering is the same as time ordering. *)
    let next i ~attempt =
      let now = Time.to_filename_string (Time.now ()) ~zone:Time.Zone.utc in
      let attempt_str = sprintf "%06d" attempt in
      now ^ "_" ^ attempt_str ^ "__" ^ Int.to_string i ^ "__"
    ;;
  end

  module Make (S : Multispool_intf.Spoolable.S) = struct
    include Make_base (S)

    module Queue_reader = struct
      include Queue_reader_raw

      let create spool queue =
        Queue_reader_raw.create_internal spool queue ~test_mode:true
      ;;
    end
  end
end
