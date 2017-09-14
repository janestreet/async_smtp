open Core
open Async
open Expect_test_helpers
open Async_smtp
open Test_async_smtp

module Widgetspool = Multispool.For_testing.Make(struct
    include Widget
    module Name_generator = Multispool.For_testing.Lexicographic_time_order_name_generator
  end)

module Test_active_and_passive_queues = struct
  open Widgetspool

  let dequeue_and_move ?(timeout_span = (sec 5.)) ?stop spool ~iterations ~print_detail =
    let rec loop ~timeout reader = function
      | i when i = iterations ->
        print_string "Dequeued and moved all expected widgets.\n";
        Deferred.unit
      | i ->
        let dequeue = Expert.Queue_reader.dequeue reader ?stop >>| ok_exn in
        let choices =
          [ choice dequeue (fun x  -> `Dequeued x)
          ; choice timeout (fun () -> `Timeout)
          ]
        in
        match%bind choose choices with
        | `Dequeued dequeued_or_error -> begin
            match dequeued_or_error with
            | `Stopped ->
              print_string "Stopped.\n";
              Deferred.unit
            | `Checked_out (widget_co, new_reader) ->
              let widget = Expert.Checked_out_entry.contents widget_co in
              if print_detail then printf !"Dequeued: %{Widget.Metadata} in Queue1\n" widget;
              begin match widget with
              | Sprocket _ ->
                failwith "[dequeue_and_move] only supports Cogs, sorry Cosmo!"
              | Cog num when num <> i ->
                printf !"OUT-OF-ORDER Widget: expected %i, got %i\n" i num;
                ()
              | Cog _ ->
                ()
              end;
              Expert.Checked_out_entry.save widget_co Widget.Queue.Queue2 >>| ok_exn
              >>= fun () ->
              if print_detail then printf !"   Moved: %{Widget.Metadata} to Queue2\n" widget;
              loop new_reader ~timeout (i + 1)
          end

        | `Timeout ->
          print_string "[dequeue] timed out, something is wrong.\n";
          Deferred.unit
    in
    Queue_reader.create spool Queue1 >>| ok_exn
    >>= fun reader ->
    let timeout = after timeout_span in
    loop reader ~timeout 0
  ;;

  let enqueue spool ~iterations =
    let rec enqueue_loop = function
      | x when x = iterations -> Deferred.unit
      | i ->
        let data =
          Widget.Data.Fields.create
            ~customer:"Amalgamated Consolidated"
            ~serial_number:i
        in
        enqueue spool Widget.Queue.Queue1 (Cog i) data (`Reserve i)
        >>| ok_exn
        >>= fun (_ : Entry.t) ->
        enqueue_loop (i + 1)
    in
    enqueue_loop 0
  ;;

  let print_queue spool queue =
    list spool queue >>| ok_exn
    >>= fun entries ->
    printf "Contents of %s:\n" (Widget.Queue.to_dirname queue);
    Deferred.List.iter entries ~f:(fun entry ->
      Entry.Direct.contents entry >>| ok_exn
      >>= fun widget ->
      printf !"    %{Widget.Metadata}\n" widget;
      Deferred.unit
    )
  ;;

  let chdir_create_and_open_spool tmp_dir spoolname =
    Sys.chdir tmp_dir
    >>= fun () ->
    let spool_dir = tmp_dir ^/ spoolname in
    create spool_dir >>| ok_exn
  ;;

  let%expect_test "Dequeue Interrupt" =
    with_temp_dir (fun tmp_dir ->
      chdir_create_and_open_spool tmp_dir "spool"
      >>= fun spool ->
      let ivar = Ivar.create () in
      let stop = Ivar.read ivar in
      let iterations = 5 in
      let pipeline = dequeue_and_move spool ~iterations ~stop ~print_detail:false in
      enqueue spool ~iterations:(iterations - 1)
      >>= fun () ->
      print_string "Stopping [dequeue]...\n";
      Ivar.fill ivar ();
      pipeline
      >>= fun () ->
      [%expect {|
        Stopping [dequeue]...
        Stopped.
      |}]
    )
  ;;

  let%expect_test "Queue Iteration" =
    with_temp_dir (fun tmp_dir ->
      chdir_create_and_open_spool tmp_dir "spool"
      >>= fun spool ->
      let iterations = 10 in
      let pipeline =
        dequeue_and_move spool
          ~iterations
          ~timeout_span:(sec 60.)
          ~print_detail:false
      in
      enqueue spool ~iterations
      >>= fun () ->
      pipeline
      >>= fun () ->
      (* This calls [iter] *)
      print_queue spool Widget.Queue.Queue2
      >>= fun () ->
      [%expect {|
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
      |}]
    )
  ;;

  let%expect_test "Queue Ordering" =
    with_temp_dir (fun tmp_dir ->
      chdir_create_and_open_spool tmp_dir "spool"
      >>= fun spool ->
      let iterations = 1_000 in
      let pipeline =
        dequeue_and_move spool
          ~iterations
          ~timeout_span:(sec 50.)
          ~print_detail:false
      in
      enqueue spool ~iterations
      >>= fun () ->
      pipeline
      >>= fun () ->
      [%expect {|
        Dequeued and moved all expected widgets.
      |}]
    )
  ;;
end
