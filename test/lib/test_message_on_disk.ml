open! Core
open Async
open Async_smtp
open Async_smtp.Private
open Expect_test_helpers_core
open Expect_test_helpers_async

let envelope_batch_and_envelope email_str =
  let envelope =
    Smtp_envelope.create
      ~sender:`Null
      ~recipients:[ Email_address.of_string "foo@bar.com" |> ok_exn ]
      ~email:(Email.of_string email_str)
      ()
  in
  let envelope_routed =
    Smtp_envelope.Routed.create ~envelope ~next_hop_choices:[] ~retry_intervals:[]
  in
  Smtp_envelope.Routed.Batch.single_envelope envelope_routed, envelope
;;

let get_entry spool queue =
  Message.On_disk_spool.list spool queue >>| ok_exn >>| List.hd_exn
;;

let escape str =
  String.concat_map str ~f:(fun c ->
    if Char.equal c '\r'
    then "\\r"
    else if Char.equal c '\n'
    then "\\n\n"
    else String.of_char c)
;;

let print_on_disk_contents data_file =
  let path = Message.On_disk_spool.Data_file.path data_file in
  let%bind contents = Reader.file_contents path in
  print_endline "On disk contents:\n-----------------";
  printf "%s" (escape contents);
  Deferred.unit
;;

let print_loaded_contents data_file =
  let%bind email =
    Message.On_disk_spool.Data_file.load data_file >>| ok_exn >>| Message.Data.to_email
  in
  print_endline "\n\nLoaded contents:\n----------------";
  printf "%s" (escape (Email.to_string email));
  Deferred.unit
;;

let test email_str =
  let envelope_batch, original_msg = envelope_batch_and_envelope email_str in
  with_temp_dir (fun tmpdir ->
    let%bind spool = Message_spool.create (tmpdir ^/ "spool") >>| ok_exn in
    let%bind (_ : (Message.t * Smtp_envelope.Routed.t) list) =
      Message_spool.enqueue
        spool
        ~log:(Lazy.force Log.Global.log)
        ~initial_status:`Frozen
        envelope_batch
        ~set_related_ids:true
        ~flows:Smtp_mail_log.Flows.none
        ~original_msg
      >>| ok_exn
    in
    let%bind entry = get_entry spool Frozen in
    let data_file = Message.On_disk_spool.Entry.Direct.data_file entry in
    let%bind () = print_on_disk_contents data_file in
    let%bind () = print_loaded_contents data_file in
    Deferred.unit)
;;

let%expect_test "Dot encoded headers and body" =
  let%bind () = test "A: B\n.C: D\n\nLine1\n.Line2" in
  [%expect
    {|
    On disk contents:
    -----------------
    A: B\r\n
    ..C: D\r\n
    \r\n
    Line1\r\n
    ..Line2

    Loaded contents:
    ----------------
    A: B\n
    .C: D\n
    \n
    Line1\n
    .Line2
    |}];
  return ()
;;

let%expect_test "Last body line ends in a new line" =
  let%bind () = test "A: B\n\nLine1\n" in
  [%expect
    {|
    On disk contents:
    -----------------
    A: B\r\n
    \r\n
    Line1\r\n


    Loaded contents:
    ----------------
    A: B\n
    \n
    Line1\n
    |}];
  return ()
;;

let%expect_test "Empty body" =
  let%bind () = test "A: B\n" in
  [%expect
    {|
    On disk contents:
    -----------------
    A: B\r\n


    Loaded contents:
    ----------------
    A: B\n
    |}];
  return ()
;;

let%expect_test "No body" =
  let%bind () = test "A: B" in
  [%expect
    {|
    On disk contents:
    -----------------
    A: B\r\n


    Loaded contents:
    ----------------
    A: B\n
    |}];
  return ()
;;
