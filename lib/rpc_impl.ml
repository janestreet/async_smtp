open Core.Std
open Async.Std

module Spool = struct
  let status () =
    Rpc.Rpc.implement Rpc_intf.Spool.status
      (fun (_config, spool) () -> return (Spool.status spool))
  ;;

  let freeze () =
    Rpc.Rpc.implement Rpc_intf.Spool.freeze
      (fun (_config, spool) msgid -> Spool.freeze spool msgid)
  ;;

  let send_now () =
    Rpc.Rpc.implement Rpc_intf.Spool.send_now
      (fun (_config, spool) (msgid, new_retry_intervals) ->
         Spool.send_now ~new_retry_intervals spool msgid)
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
