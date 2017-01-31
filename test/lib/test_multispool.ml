open Core.Std
open Async.Std
open Async_smtp.Std
open Expect_test_helpers

module Test_name_generator = struct
  type t = string
  let next (prefix : t) ~attempt =
    let attempt_str = sprintf "%06d" attempt in
    prefix ^ attempt_str
  ;;
end

module Widgetspool = Multispool.Make(struct
    include Widget
    module Name_generator = Test_name_generator
  end)

let chdir_or_error dir =
  Deferred.Or_error.try_with (fun () -> Sys.chdir dir)
;;

let system_or_error cmd =
  Deferred.Or_error.try_with (fun () -> system cmd)
;;

let string_filter_tmp_dir ~tmp_dir string =
  String.substr_replace_all string ~pattern:tmp_dir ~with_:"${TMPDIR}"
;;

let sexp_filter_tmp_dir ~tmp_dir sexp =
  let rec loop = function
    | Sexp.Atom atom -> Sexp.Atom (string_filter_tmp_dir atom ~tmp_dir)
    | Sexp.List list -> Sexp.List (List.map list ~f:loop)
  in
  loop sexp
;;

let print_s ~tmp_dir sexp =
  print_s (sexp_filter_tmp_dir sexp ~tmp_dir)
;;

let chdir_create_and_open_spool tmp_dir spoolname =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = chdir_or_error tmp_dir in
  let spool_dir = tmp_dir ^/ spoolname in
  Widgetspool.create spool_dir
;;

let%expect_test "Spool Creation Fails" =
  with_temp_dir (fun tmp_dir ->
    let spoolname = "spool" in
    Unix.mkdir (tmp_dir ^/ spoolname)
    >>= fun () ->
    Writer.save (tmp_dir ^/ spoolname ^/ "some_file") ~contents:""
    >>= fun () ->
    chdir_create_and_open_spool tmp_dir spoolname
    >>= fun result ->
    print_s ~tmp_dir [%sexp (result : Widgetspool.t Or_error.t)];
    [%expect {|
      (Error ("dir is not empty while creating new spool" (dir ${TMPDIR}/spool)))
    |}])
;;

let nn_with_prefix = fun prefix attempt ->
  let attempt_str = sprintf "%06d" attempt in
  prefix ^ attempt_str
;;

let%expect_test "File Behavior" =
  let open Deferred.Or_error.Let_syntax in
  with_temp_dir (fun tmp_dir ->

    let%bind spool = chdir_create_and_open_spool tmp_dir "spool" in
    let%bind () = system_or_error "find spool | /usr/bin/env -i sort" in
    let%bind () = [%expect {|
        spool
        spool/.checkout
        spool/.registry
        spool/.tmp
        spool/queue1
        spool/queue2
        spool/queue3
      |}] |> Deferred.ok
    in

    (* Enqueue a file, check its path(s) at various steps, and then remove it *)
    let widget1 = Widget.Cog 1 in
    let%bind entry =
      Widgetspool.enqueue spool Queue1 widget1 (`Reserve "q1_co_test_")
    in
    let%bind () = system_or_error "find spool | /usr/bin/env -i sort" in
    (* File exists in registry and queue *)
    let%bind () = [%expect {|
        spool
        spool/.checkout
        spool/.registry
        spool/.registry/q1_co_test_000000
        spool/.tmp
        spool/queue1
        spool/queue1/q1_co_test_000000
        spool/queue2
        spool/queue3
      |}] |> Deferred.ok
    in
    let%bind file_co = Widgetspool.Expert.checkout entry in
    let%bind () = system_or_error "find spool | /usr/bin/env -i sort" in
    (* File exists in registry and checkout area *)
    let%bind () = [%expect {|
        spool
        spool/.checkout
        spool/.checkout/q1_co_test_000000
        spool/.registry
        spool/.registry/q1_co_test_000000
        spool/.tmp
        spool/queue1
        spool/queue2
        spool/queue3
      |}] |> Deferred.ok
    in
    let%bind () = Widgetspool.Expert.Checked_out_entry.remove file_co in
    let%bind () = system_or_error "find spool | /usr/bin/env -i sort" in
    (* File no longer exists *)
    let%bind () = [%expect {|
        spool
        spool/.checkout
        spool/.registry
        spool/.tmp
        spool/queue1
        spool/queue2
        spool/queue3
      |}] |> Deferred.ok
    in

    (* Enqueue some files and test nextname functionality *)
    let enqueue (queue : Widget.Queue.t) prefix text =
      let w = Widget.Sprocket text in
      let%map (_ : Widgetspool.Entry.t) =
        Widgetspool.enqueue spool queue w (`Reserve prefix)
      in
      ()
    in
    let%bind () = enqueue Queue1 "q1prefix1_" "Hello, world 1! q1prefix1_000000" in
    let%bind () = enqueue Queue1 "q1prefix1_" "Hello, world 2! q1prefix1_000001" in
    let%bind () = enqueue Queue1 "q1prefix2_" "Hello, world 3! q1prefix2_000000" in
    let%bind () = enqueue Queue2 "q2prefix1_" "Hello, world 4! q2prefix1_000000" in
    let%bind () = enqueue Queue2 "q2prefix2_" "Hello, world 5! q2prefix2_000000" in
    let%bind () = enqueue Queue2 "q2prefix1_" "Hello, world 6! q2prefix1_000001" in
    let%bind () = system_or_error "find spool | /usr/bin/env -i sort" in
    let%bind () = [%expect {|
        spool
        spool/.checkout
        spool/.registry
        spool/.registry/q1prefix1_000000
        spool/.registry/q1prefix1_000001
        spool/.registry/q1prefix2_000000
        spool/.registry/q2prefix1_000000
        spool/.registry/q2prefix1_000001
        spool/.registry/q2prefix2_000000
        spool/.tmp
        spool/queue1
        spool/queue1/q1prefix1_000000
        spool/queue1/q1prefix1_000001
        spool/queue1/q1prefix2_000000
        spool/queue2
        spool/queue2/q2prefix1_000000
        spool/queue2/q2prefix1_000001
        spool/queue2/q2prefix2_000000
        spool/queue3
      |}] |> Deferred.ok
    in

    (* List files *)
    let%bind fentries1 = Widgetspool.list spool Queue1 in
    let%bind fentries2 = Widgetspool.list spool Queue2 in
    let entries1 = List.map fentries1 ~f:(fun entry -> Widgetspool.Entry.name entry) in
    let entries2 = List.map fentries2 ~f:(fun entry -> Widgetspool.Entry.name entry) in
    print_s ~tmp_dir [%message "queue1 entries" (entries1 : string list)];
    print_s ~tmp_dir [%message "queue2 entries" (entries2 : string list)];
    let%bind () = [%expect {|
        ("queue1 entries" (
          entries1 (q1prefix1_000000 q1prefix1_000001 q1prefix2_000000)))
        ("queue2 entries" (
          entries2 (q2prefix1_000000 q2prefix1_000001 q2prefix2_000000)))
      |}] |> Deferred.ok
    in

    (* Checkout a file and move to a different queue *)
    let entry = Widgetspool.Entry.create spool Queue1 ~name:"q1prefix1_000000" in
    let%bind file_co = Widgetspool.Expert.checkout entry in
    let%bind () = Widgetspool.Expert.Checked_out_entry.save file_co Queue2 in
    let%bind () = system_or_error "find spool | /usr/bin/env -i sort" in
    let%bind () = [%expect {|
        spool
        spool/.checkout
        spool/.registry
        spool/.registry/q1prefix1_000000
        spool/.registry/q1prefix1_000001
        spool/.registry/q1prefix2_000000
        spool/.registry/q2prefix1_000000
        spool/.registry/q2prefix1_000001
        spool/.registry/q2prefix2_000000
        spool/.tmp
        spool/queue1
        spool/queue1/q1prefix1_000001
        spool/queue1/q1prefix2_000000
        spool/queue2
        spool/queue2/q1prefix1_000000
        spool/queue2/q2prefix1_000000
        spool/queue2/q2prefix1_000001
        spool/queue2/q2prefix2_000000
        spool/queue3
      |}] |> Deferred.ok
    in

    (* Use iter_exn to visit each file in queue2 in turn *)
    let%bind reader2 = Widgetspool.Queue_reader.create spool Queue2 in
    let%bind () = Widgetspool.Queue_reader.iter_available reader2 ~f:(fun widget ->
      print_s ~tmp_dir ([%sexp_of: Widget.t] widget);
      Deferred.return (`Save (widget, Widget.Queue.Queue2))
    ) in
    let%bind () = [%expect {|
        (Sprocket "Hello, world 1! q1prefix1_000000")
        (Sprocket "Hello, world 4! q2prefix1_000000")
        (Sprocket "Hello, world 6! q2prefix1_000001")
        (Sprocket "Hello, world 5! q2prefix2_000000")
      |}] |> Deferred.ok
    in

    (* Update a Sprocket *)
    let entry = Widgetspool.Entry.create spool Queue2 ~name:"q2prefix1_000000" in
    let%bind () =
      Widgetspool.with_entry entry
        ~f:(fun widget ->
          let new_widget = Widget.Cog 42 in
          printf !"Replacing '%{sexp:Widget.t}' with '%{sexp:Widget.t}' ...\n"
            widget new_widget;
          Deferred.return (`Save (new_widget, Widget.Queue.Queue2), ()))
    in
    (* Print the list again *)
    let%bind () = Widgetspool.Queue_reader.iter_available reader2 ~f:(fun widget ->
      print_s ~tmp_dir ([%sexp_of: Widget.t] widget);
      Deferred.return (`Save (widget, Widget.Queue.Queue2))
    ) in
    let%bind () = [%expect {|
        Replacing '(Sprocket "Hello, world 4! q2prefix1_000000")' with '(Cog 42)' ...
        (Sprocket "Hello, world 1! q1prefix1_000000")
        (Cog 42)
        (Sprocket "Hello, world 6! q2prefix1_000001")
        (Sprocket "Hello, world 5! q2prefix2_000000")
      |}] |> Deferred.ok
    in

    return ()
  ) |> Deferred.Or_error.ok_exn
;;
