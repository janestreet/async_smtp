open Core
open! Async
open Async_smtp.Smtp_server.Config

let v1 =
  {|
((where_to_listen ((Port 2525)))
 (max_concurrent_receive_jobs_per_port 0)
 (timeouts (
   (receive             5m)
   (receive_after_close 10s)))
 (rpc_port         0)
 (malformed_emails Reject)
 (max_message_size (Megabytes 24)))
 |}
;;

let v2 =
  {|
((where_to_listen (
   (All_interfaces_on_port 2525)
   (Localhost_on_port      2526)
   Localhost_on_port_chosen_by_os
   (Ip_on_port 1.2.3.4 2527)))
 (max_concurrent_receive_jobs_per_port 0)
 (timeouts (
   (receive             5m)
   (receive_after_close 10s)))
 (rpc_port         0)
 (malformed_emails Reject)
 (max_message_size (Megabytes 24)))
 |}
;;

let ensure_parses config =
  Expect_test_helpers_async.with_temp_dir (fun dir ->
    let path = dir ^/ "config" in
    let%bind () = Writer.save path ~contents:config in
    let%bind (_ : t) = load_exn path in
    print_endline "Ok";
    Deferred.unit)
;;

let%expect_test _ =
  let%bind () = ensure_parses v1 in
  [%expect {| Ok |}];
  let%bind () = ensure_parses v2 in
  [%expect {| Ok |}];
  Deferred.unit
;;
