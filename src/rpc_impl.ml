open Core
open Async

module Monitor = struct
  let errors () =
    let seqnum = ref 0 in
    let error_stream =
      Bus.create [%here] Arity1 ~on_subscription_after_first_write:Allow
        ~on_callback_raise:Error.raise
    in
    (* Hearbeats *)
    Clock.every (sec 10.) (fun () ->
      Bus.write error_stream (!seqnum, None));
    (* Actual errors *)
    let send_errors = Log.Output.create ~flush:(fun () -> return ()) (fun messages ->
      Queue.iter messages ~f:(fun message ->
        match Mail_log.Message.level message with
        | `Error ->
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
      (fun _ () ->
         Log.Global.debug "received error stream subscription";
         let pipe = Bus.pipe1_exn (Bus.read_only error_stream) [%here] in
         return (Ok pipe))
  ;;
end

module Smtp_events = struct
  let events () =
    Rpc.Pipe_rpc.implement Rpc_intf.Smtp_events.events
      (fun (_config, _spool, server_events) () ->
         let pipe = Smtp_events.event_stream server_events in
         return (Ok pipe))
  ;;
end

module Cache = struct
  let status () =
    Rpc.Pipe_rpc.implement Rpc_intf.Cache.status
      (fun (_config, spool, _server_events) update_interval ->
         let cache = Spool.client_cache spool in
         let r, w = Pipe.create () in
         Clock.every' ~stop:(Pipe.closed w) update_interval
           (fun () -> Pipe.write w (Client_cache.status cache));
         return (Ok r))

  let config () =
    Rpc.Rpc.implement Rpc_intf.Cache.config
      (fun (_config, spool, _server_events) () ->
         return (Client_cache.config (Spool.client_cache spool)))
end

module Spool = struct
  let status () =
    Rpc.Rpc.implement Rpc_intf.Spool.status
      (fun (_config, spool, _server_events) () -> return (Spool.status spool))
  ;;

  let freeze () =
    Rpc.Rpc.implement Rpc_intf.Spool.freeze
      (fun (_config, spool, _server_events) msgids -> Spool.freeze spool msgids)
  ;;

  let send () =
    Rpc.Rpc.implement Rpc_intf.Spool.send
      (fun (_config, spool, _server_events) (retry_intervals, send_info) ->
         Spool.send ~retry_intervals spool send_info)

  let remove () =
    Rpc.Rpc.implement Rpc_intf.Spool.remove
      (fun (_config, spool, _server_events) msgids -> Spool.remove spool msgids)
  ;;

  let recover () =
    Rpc.Rpc.implement Rpc_intf.Spool.recover
      (fun (_config, spool, _server_events) info -> Spool.recover spool info)
  ;;

  let events () =
    Rpc.Pipe_rpc.implement Rpc_intf.Spool.events
      (fun (_config, spool, _server_events) () ->
         let pipe = Spool.event_stream spool in
         return (Ok pipe))
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
      (fun _ () ->
         let r, w = Pipe.create () in
         Clock.every' ~stop:(Pipe.closed w) (Time.Span.of_sec 15.)
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
