open Core
open! Async
open Async_smtp.Smtp_spool.Config

let v0 =
  {|
((spool_dir .)
 (tmp_dir ())
 (max_concurrent_send_jobs 0)
 (client (
   (tls ((
     "" (
       (version ())
       (options ())
       (name    ())
       (allowed_ciphers Secure)
       (ca_file ())
       (ca_path ())
       (mode             If_available)
       (certificate_mode Ignore)))))
   (send_receive_timeout Default)
   (final_ok_timeout     Default))))
 |}
;;

let v1 =
  {|
((spool_dir .)
 (tmp_dir ())
 (max_concurrent_send_jobs 0)
 (connection_cache (
   (max_open_connections          500)
   (cleanup_idle_connection_after 5s)
   (max_connections_per_address   10)
   (max_connection_reuse          10)))
 (client (
   (tls ((
     "" (
       (version ())
       (options ())
       (name    ())
       (allowed_ciphers Secure)
       (ca_file ())
       (ca_path ())
       (mode             If_available)
       (certificate_mode Ignore)))))
   (send_receive_timeout Default)
   (final_ok_timeout     Default))))
 |}
;;

let v2 =
  {|
((spool_dir .)
 (tmp_dir ())
 (connection_cache (
   (max_open_connections          500)
   (cleanup_idle_connection_after 5s)
   (max_connections_per_address   10)
   (max_connection_reuse          10)))
 (client (
   (tls ((
     "" (
       (version ())
       (options ())
       (name    ())
       (allowed_ciphers Secure)
       (ca_file ())
       (ca_path ())
       (mode             If_available)
       (certificate_mode Ignore)))))
   (send_receive_timeout Default)
   (final_ok_timeout     Default))))
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
  let%bind () = ensure_parses v0 in
  [%expect {| Ok |}];
  let%bind () = ensure_parses v1 in
  [%expect {| Ok |}];
  let%bind () = ensure_parses v2 in
  [%expect {| Ok |}];
  Deferred.unit
;;
