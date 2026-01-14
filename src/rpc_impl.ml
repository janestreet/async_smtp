open! Core
open! Async

module Monitor = struct
  let errors () =
    let seqnum = ref 0 in
    let error_stream =
      Bus.create_exn
        ~on_subscription_after_first_write:Allow
        ~on_callback_raise:Error.raise
        ()
    in
    (* Hearbeats *)
    Clock.every (sec 10.) (fun () -> Bus.write error_stream (!seqnum, None));
    (* Actual errors *)
    let send_errors =
      Log.Output.create
        ~flush:(fun () -> return ())
        (fun messages ->
          Queue.iter messages ~f:(fun message ->
            match Mail_log.Message.level message with
            | `Error ->
              let error = Log.Message.message message |> Error.of_string in
              incr seqnum;
              Bus.write error_stream (!seqnum, Some error)
            | _ -> ());
          Deferred.unit)
    in
    Log.Global.set_output (send_errors :: Log.Global.get_output ());
    Rpc.Pipe_rpc.implement
      Rpc_intf.Monitor.errors
      (fun () () ->
        [%log.debug_string "received error stream subscription"];
        let pipe = Async_bus.pipe1_exn error_stream in
        return (Ok pipe))
      ~leave_open_on_exception:true
  ;;

  let rpcs () = [ errors () ]
end

module Spool = struct
  module Cache = struct
    let status =
      Rpc.Pipe_rpc.implement
        Rpc_intf.Spool.Cache.status
        (fun spool update_interval ->
          let cache = Spool.client_cache spool in
          let r, w = Pipe.create () in
          Clock.every' ~stop:(Pipe.closed w) update_interval (fun () ->
            Pipe.write w (Client_cache.status cache));
          return (Ok r))
        ~leave_open_on_exception:true
    ;;

    let config =
      Rpc.Rpc.implement Rpc_intf.Spool.Cache.config (fun spool () ->
        return
          (Client_cache.config (Spool.client_cache spool)
           |> Resource_cache.Address_config.Stable.V2.of_v3
           |> Resource_cache.Address_config.Stable.V1.of_v2))
    ;;

    let rpcs = [ status; config ]
  end

  module Status = struct
    let rpcs =
      Babel.Callee.implement_multi_exn Rpc_intf.Spool.Status.callee ~f:(fun spool _ () ->
        return (Spool.status spool |> Ok))
    ;;
  end

  let freeze =
    Rpc.Rpc.implement Rpc_intf.Spool.freeze (fun spool msgids ->
      Spool.freeze spool msgids)
  ;;

  let send =
    Rpc.Rpc.implement Rpc_intf.Spool.send (fun spool (retry_intervals, send_info) ->
      Spool.send ~retry_intervals spool send_info)
  ;;

  let remove =
    Rpc.Rpc.implement Rpc_intf.Spool.remove (fun spool msgids ->
      Spool.remove spool msgids)
  ;;

  let recover =
    Rpc.Rpc.implement Rpc_intf.Spool.recover (fun spool info -> Spool.recover spool info)
  ;;

  let events =
    Rpc.Pipe_rpc.implement Rpc_intf.Spool.events (fun spool () ->
      let pipe = Spool.event_stream spool in
      return (Ok pipe))
  ;;

  let rpcs = [ freeze; send; remove; recover; events ] @ Status.rpcs @ Cache.rpcs
end

module Smtp_events = struct
  let events =
    Rpc.Pipe_rpc.implement
      Rpc_intf.Smtp_events.events
      (fun server_events () ->
        let pipe = Smtp_events.event_stream server_events in
        return (Ok pipe))
      ~leave_open_on_exception:true
  ;;

  let rpcs = [ events ]
end

module Gc = struct
  let stat = Rpc.Rpc.implement Rpc_intf.Gc.stat (fun () () -> Gc.stat () |> return)

  let quick_stat =
    Rpc.Rpc.implement Rpc_intf.Gc.quick_stat (fun () () -> Gc.quick_stat () |> return)
  ;;

  let full_major =
    Rpc.Rpc.implement Rpc_intf.Gc.full_major (fun () () -> Gc.full_major () |> return)
  ;;

  let major = Rpc.Rpc.implement Rpc_intf.Gc.major (fun () () -> Gc.major () |> return)
  let minor = Rpc.Rpc.implement Rpc_intf.Gc.minor (fun () () -> Gc.minor () |> return)

  let compact =
    Rpc.Rpc.implement Rpc_intf.Gc.compact (fun () () -> Gc.compact () |> return)
  ;;

  let stat_pipe =
    Rpc.Pipe_rpc.implement
      Rpc_intf.Gc.stat_pipe
      (fun () () ->
        let r, w = Pipe.create () in
        Clock.every' ~stop:(Pipe.closed w) (Time_float.Span.of_sec 15.) (fun () ->
          Pipe.write w (Gc.quick_stat ()));
        return (Ok r))
      ~leave_open_on_exception:true
  ;;

  let rpcs = [ stat; quick_stat; full_major; major; minor; compact; stat_pipe ]
end

module Process = struct
  let pid = Rpc.Rpc.implement Rpc_intf.Process.pid (fun () () -> Unix.getpid () |> return)
  let rpcs = [ pid ]
end
