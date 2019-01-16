open! Core
open Async
open Async_smtp
open Common

let gc_command summary rpc =
  let open Command.Let_syntax in
  Command.rpc
    ~summary
    [%map_open
      let () = return () in
      fun client ->
        let open Deferred.Let_syntax in
        let%map () = Rpc.Rpc.dispatch_exn rpc client () in
        printf "OK\n"]
;;

let gc_stat_command summary rpc =
  let open Command.Let_syntax in
  Command.rpc
    ~summary
    [%map_open
      let () = return () in
      fun client ->
        let open Deferred.Let_syntax in
        let%map stat = Rpc.Rpc.dispatch_exn rpc client () in
        printf !"%{sexp:Gc.Stat.t}\n" stat]
;;

let stat = gc_stat_command "Gc.stat" Smtp_rpc_intf.Gc.stat
let quick_stat = gc_stat_command "Gc.quick_stat" Smtp_rpc_intf.Gc.quick_stat
let full_major = gc_command "Gc.full_major" Smtp_rpc_intf.Gc.full_major
let major = gc_command "Gc.major" Smtp_rpc_intf.Gc.major
let minor = gc_command "Gc.minor" Smtp_rpc_intf.Gc.minor
let compact = gc_command "Gc.compact" Smtp_rpc_intf.Gc.compact

let command =
  Command.group
    ~summary:"GC management"
    [ "stat", stat
    ; "quick-stat", quick_stat
    ; "full-major", full_major
    ; "major", major
    ; "minor", minor
    ; "compact", compact
    ]
;;
