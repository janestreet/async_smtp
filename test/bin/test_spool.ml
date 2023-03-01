open Core
open Async
open Async_smtp
open Test_async_smtp

module Widget_monitor = Multispool.Monitor.Make (struct
    include Widget
    module Name_generator = Common.Test_name_generator
  end)

module Fsck = struct
  module Immediate = struct
    let command =
      let open Command.Let_syntax in
      Command.async_or_error
        ~summary:"Run a consistency check on a Widget spool."
        [%map_open
          let spec = Widget_monitor.Spec.param in
          fun () ->
            Widget_monitor.create spec
            >>=? fun spool ->
            Widget_monitor.run_once spool
            >>=? fun problems ->
            List.iter problems ~f:(fun p ->
              printf !"%{sexp:Widget_monitor.Problem.t}\n" p);
            Deferred.Or_error.ok_unit]
        ~behave_nicely_in_pipeline:false
    ;;
  end

  module Monitor = struct
    let command =
      let open Command.Let_syntax in
      Command.async_or_error
        ~summary:"Run a monitor that prints events to stdout."
        [%map_open
          let spec = Widget_monitor.Spec.param
          and daemon = Widget_monitor.Daemon.param in
          fun () ->
            Widget_monitor.create spec
            >>=? fun monitor ->
            Widget_monitor.Daemon.start daemon ~monitor ~f:(fun event ->
              printf !"%{sexp:Widget_monitor.Event.t}\n" event;
              Deferred.unit);
            Deferred.never ()]
        ~behave_nicely_in_pipeline:false
    ;;
  end

  let command =
    Command.group
      ~summary:"Widget spool fsck consistency checker."
      [ "immediate", Immediate.command; "monitor", Monitor.command ]
  ;;
end

module Create = struct
  (* We need a Widget spool to create the on-disk spool structure *)
  module Widget_spool = Multispool.Make (struct
      include Widget
      module Name_generator = Common.Test_name_generator
    end)

  let command =
    let open Command.Let_syntax in
    Command.async_or_error
      ~summary:"Create an empty spool."
      [%map_open
        let spool_dir = anon ("SPOOL_DIR" %: string) in
        fun () ->
          Widget_spool.create spool_dir
          >>=? fun (_ : Widget_spool.t) -> Deferred.Or_error.ok_unit]
      ~behave_nicely_in_pipeline:false
  ;;
end

let command =
  Command.group
    ~summary:"TESTING Spool utilities."
    [ "create", Create.command; "fsck", Fsck.command ]
;;

let () = Command_unix.run command
