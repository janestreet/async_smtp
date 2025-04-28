open! Core
open! Async
open Async_smtp
open Async_smtp.Private
open Email_message
open Expect_test_helpers_core
open Expect_test_helpers_async

let main ~tmp_dir ~iterations ~msg_size =
  let%bind spool_dir = Message_spool.create tmp_dir >>| ok_exn in
  let log = Lazy.force Log.Global.log in
  let email =
    Email.Simple.create
      ~to_:[]
      ~subject:""
      (Email.Simple.Content.text_utf8 (String.init msg_size ~f:(fun _ -> '0')))
  in
  let envelope = Smtp_envelope.create ~sender:`Null ~recipients:[] ~email () in
  let envelope_batch =
    Smtp_envelope.Routed.create ~envelope ~next_hop_choices:[] ~retry_intervals:[]
    |> Smtp_envelope.Routed.Batch.single_envelope
  in
  let spool () =
    Message_spool.enqueue
      spool_dir
      ~log
      ~initial_status:`Frozen
      envelope_batch
      ~set_related_ids:true
      ~flows:Smtp_mail_log.Flows.none
      ~original_msg:envelope
    >>| ok_exn
    >>| ignore
  in
  Deferred.for_ 0 ~to_:iterations ~do_:(fun (_ : int) -> spool ())
;;

let command =
  let open Command.Let_syntax in
  Command.async
    ~summary:""
    [%map_open
      let iterations =
        flag
          "-iterations"
          (optional_with_default 100 int)
          ~doc:"NUM number of emails to send (default 100)"
      and msg_size =
        flag
          "-msg-size"
          (optional_with_default 1_048_576 int)
          ~doc:"SIZE message body size in bytes (default 1_048_576)"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let gc_stat = Gc.stat () in
        let%map result =
          with_temp_dir (fun tmp_dir ->
            Monitor.try_with_or_error ~rest:`Log (fun () ->
              main ~tmp_dir ~iterations ~msg_size))
        in
        let () = ok_exn result in
        let gc_stat' = Gc.stat () in
        let get f = f gc_stat' -. f gc_stat in
        print_s
          [%message
            ""
              ~minor_words:(get Gc.Stat.minor_words : float)
              ~major_words:(get Gc.Stat.major_words : float)]]
    ~behave_nicely_in_pipeline:false
;;
