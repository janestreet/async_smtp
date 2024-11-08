open Core
open Async
open Expect_test_helpers_core
open Expect_test_helpers_async
open Async_smtp
open Test_async_smtp

module Widgetspool = Multispool.For_testing.Make (struct
    include Widget
    module Name_generator = Multispool.For_testing.Lexicographic_time_order_name_generator
  end)

module%test _ = struct
  open Widgetspool

  let dequeue_and_move ?(timeout_span = sec 5.) ?stop spool ~iterations ~print_detail =
    let rec loop ~timeout reader = function
      | i when i = iterations ->
        print_string "Dequeued and moved all expected widgets.\n";
        Deferred.unit
      | i ->
        let dequeue = Expert.Queue_reader.dequeue reader ?stop >>| ok_exn in
        let choices =
          [ choice dequeue (fun x -> `Dequeued x); choice timeout (fun () -> `Timeout) ]
        in
        (match%bind choose choices with
         | `Dequeued dequeued_or_error ->
           (match dequeued_or_error with
            | `Stopped ->
              print_string "Stopped.\n";
              Deferred.unit
            | `Checked_out (widget_co, new_reader) ->
              let widget = Expert.Checked_out_entry.contents widget_co in
              if print_detail
              then printf !"Dequeued: %{Widget.Metadata} in Queue1\n" widget;
              (match widget with
               | Sprocket _ ->
                 failwith "[dequeue_and_move] only supports Cogs, sorry Cosmo!"
               | Cog num when num <> i ->
                 printf !"OUT-OF-ORDER Widget: expected %i, got %i\n" i num;
                 ()
               | Cog _ -> ());
              let%bind () =
                Expert.Checked_out_entry.save widget_co Widget.Queue.Queue2 >>| ok_exn
              in
              if print_detail
              then printf !"   Moved: %{Widget.Metadata} to Queue2\n" widget;
              loop new_reader ~timeout (i + 1))
         | `Timeout ->
           print_string "[dequeue] timed out, something is wrong.\n";
           Deferred.unit)
    in
    let%bind reader = Queue_reader.create spool Queue1 >>| ok_exn in
    let timeout = after timeout_span in
    loop reader ~timeout 0
  ;;

  let enqueue spool ~iterations =
    let rec enqueue_loop = function
      | x when x = iterations -> Deferred.unit
      | i ->
        let data =
          Widget.Data.Fields.create ~customer:"Amalgamated Consolidated" ~serial_number:i
        in
        let%bind (_ : Entry.t) =
          enqueue spool Widget.Queue.Queue1 (Cog i) data (`Reserve i) >>| ok_exn
        in
        enqueue_loop (i + 1)
    in
    enqueue_loop 0
  ;;

  let print_queue spool queue =
    let%bind entries = list spool queue >>| ok_exn in
    printf "Contents of %s:\n" (Widget.Queue.to_dirname queue);
    Deferred.List.iter ~how:`Sequential entries ~f:(fun entry ->
      let%bind widget = Entry.Direct.contents entry >>| ok_exn in
      printf !"    %{Widget.Metadata}\n" widget;
      Deferred.unit)
  ;;

  let chdir_create_and_open_spool tmp_dir spoolname =
    let%bind () = Sys.chdir tmp_dir in
    let spool_dir = tmp_dir ^/ spoolname in
    create spool_dir >>| ok_exn
  ;;

  let%expect_test "Dequeue Interrupt" =
    with_temp_dir (fun tmp_dir ->
      let%bind spool = chdir_create_and_open_spool tmp_dir "spool" in
      let ivar = Ivar.create () in
      let stop = Ivar.read ivar in
      let iterations = 5 in
      let pipeline = dequeue_and_move spool ~iterations ~stop ~print_detail:false in
      let%bind () = enqueue spool ~iterations:(iterations - 1) in
      print_string "Stopping [dequeue]...\n";
      Ivar.fill_exn ivar ();
      let%bind () = pipeline in
      [%expect
        {|
        Stopping [dequeue]...
        Stopped.
        |}];
      return ())
  ;;

  let%expect_test "Queue Iteration" =
    with_temp_dir (fun tmp_dir ->
      let%bind spool = chdir_create_and_open_spool tmp_dir "spool" in
      let iterations = 10 in
      let pipeline =
        dequeue_and_move spool ~iterations ~timeout_span:(sec 60.) ~print_detail:false
      in
      let%bind () = enqueue spool ~iterations in
      let%bind () = pipeline in
      (* This calls [iter] *)
      let%bind () = print_queue spool Widget.Queue.Queue2 in
      [%expect
        {|
        Dequeued and moved all expected widgets.
        Contents of queue2:
            (Cog 0)
            (Cog 1)
            (Cog 2)
            (Cog 3)
            (Cog 4)
            (Cog 5)
            (Cog 6)
            (Cog 7)
            (Cog 8)
            (Cog 9)
        |}];
      return ())
  ;;

  let%expect_test "Queue Ordering" =
    with_temp_dir (fun tmp_dir ->
      let%bind spool = chdir_create_and_open_spool tmp_dir "spool" in
      let iterations = 1_000 in
      let pipeline =
        dequeue_and_move spool ~iterations ~timeout_span:(sec 50.) ~print_detail:false
      in
      let%bind () = enqueue spool ~iterations in
      let%bind () = pipeline in
      [%expect {| Dequeued and moved all expected widgets. |}];
      return ())
  ;;
end
