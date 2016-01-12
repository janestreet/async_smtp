open Core.Std
open Async.Std

module Monitor = struct
  let errors () =
    let seqnum = ref 0 in
    let error_stream =
      Bus.create [%here] Arity1 ~allow_subscription_after_first_write:true
        ~on_callback_raise:Error.raise
    in
    (* Hearbeats *)
    Clock.every (sec 10.) (fun () ->
      Bus.write error_stream (!seqnum, None));
    (* Actual errors *)
    let send_errors = Log.Output.create (fun messages ->
      Queue.iter messages ~f:(fun message ->
        match Log.Message.level message with
        | Some `Error ->
          let error =
            Log.Message.message message
            |> Error.of_string
          in
          incr seqnum;
          Bus.write error_stream (!seqnum, Some error)
        | _ -> ());
      Deferred.unit)
    in
    Log.Global.set_output (send_errors :: Log.Global.get_output ());
    Rpc.Pipe_rpc.implement Rpc_intf.Monitor.errors
      (fun _ () ~aborted ->
         Log.Global.debug "received error stream subscription";
         let pipe = Bus.pipe1_exn (Bus.read_only error_stream) in
         (aborted >>> fun () -> Pipe.close_read pipe);
         return (Ok pipe))
  ;;
end

module Spool = struct
  let status () =
    Rpc.Rpc.implement Rpc_intf.Spool.status
      (fun (_config, spool) () -> return (Spool.status spool))
  ;;

  let freeze () =
    Rpc.Rpc.implement Rpc_intf.Spool.freeze
      (fun (_config, spool) msgids -> Spool.freeze spool msgids)
  ;;

  let send () =
    Rpc.Rpc.implement Rpc_intf.Spool.send
      (fun (_config, spool) (retry_intervals, send_info) ->
         Spool.send ~retry_intervals spool send_info)

  let remove () =
    Rpc.Rpc.implement Rpc_intf.Spool.remove
      (fun (_config, spool) msgids -> Spool.remove spool msgids)
  ;;

  let recover () =
    Rpc.Rpc.implement Rpc_intf.Spool.recover
      (fun (_config, spool) msgids -> Spool.recover spool msgids)
  ;;

  let events () =
    Rpc.Pipe_rpc.implement Rpc_intf.Spool.events
      (fun (_config, spool) () ~aborted ->
         let pipe = Spool.event_stream spool in
         (aborted >>> fun () -> Pipe.close_read pipe);
         return (Ok pipe))
  ;;

  let set_max_concurrent_send_jobs () =
    Rpc.Rpc.implement Rpc_intf.Spool.set_max_concurrent_send_jobs
      (fun (_config, spool) n ->
        Spool.set_max_concurrent_jobs spool n |> return)
  ;;
end

module Gc = struct
  let stat () =
    Rpc.Rpc.implement Rpc_intf.Gc.stat
      (fun _ () -> Gc.stat () |> return)
  ;;

  let quick_stat () =
    Rpc.Rpc.implement Rpc_intf.Gc.quick_stat
      (fun _ () -> Gc.quick_stat () |> return)
  ;;

  let full_major () =
    Rpc.Rpc.implement Rpc_intf.Gc.full_major
      (fun _ () -> Gc.full_major () |> return)
  ;;

  let major () =
    Rpc.Rpc.implement Rpc_intf.Gc.major
      (fun _ () -> Gc.major () |> return)
  ;;

  let minor () =
    Rpc.Rpc.implement Rpc_intf.Gc.minor
      (fun _ () -> Gc.minor () |> return)
  ;;

  let compact () =
    Rpc.Rpc.implement Rpc_intf.Gc.compact
      (fun _ () -> Gc.compact () |> return)
  ;;

  let stat_pipe () =
    Rpc.Pipe_rpc.implement Rpc_intf.Gc.stat_pipe
      (fun _ () ~aborted ->
         let r, w = Pipe.create () in
         Clock.every' ~stop:aborted (Time.Span.of_sec 15.)
           (fun () -> Pipe.write w (Gc.quick_stat ()));
         return (Ok r))
  ;;
end

module Process = struct
  let pid () =
    Rpc.Rpc.implement Rpc_intf.Process.pid
      (fun _ () -> Unix.getpid () |> return)
  ;;
end
