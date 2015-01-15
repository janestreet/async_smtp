open Core.Std
open Async.Std
open Async_smtp.Std
open Common

type format = [ `Ascii_table | `Ascii_table_with_max_width of int | `Exim | `Sexp ] with sexp

let format =
  Command.Spec.Arg_type.create
    (function
      | "ascii" -> `Ascii_table
      | "exim" -> `Exim
      | "sexp" -> `Sexp
      | str ->
        str |> String.strip |> Sexp.of_string |> format_of_sexp
    )

let msgid =
  Command.Spec.Arg_type.create
    Smtp_spool.Spooled_message_id.of_string

let status ~format client =
  Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.status client ()
  >>| fun status ->
  printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)

let status_command =
  Command.config_or_rpc ~summary:"list current contents of the spool"
    Command.Spec.(
      step (fun m v -> m ~format:v)
      +> flag "format" (optional_with_default `Ascii_table format)
        ~doc:" Output format for the spool, valid values include 'ascii', 'exim', 'sexp'"
    )
    (fun ~format -> function
       | `Config config ->
           Smtp_spool.status_from_disk config |> Deferred.Or_error.ok_exn
           >>| fun status ->
           printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)
       | `Rpc client ->
         status ~format client
    )

let count client =
  Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.status client ()
  >>| List.length
  >>| printf "%d\n"

let count_command =
  Command.config_or_rpc ~summary:"print total number of messages in the spool"
    Command.Spec.empty
    (function
      | `Config config ->
         Smtp_spool.count_from_disk config
         >>| Or_error.ok_exn
         >>| printf "%d\n"
      | `Rpc client ->
        count client
    )

let set_max_send_jobs ~num client =
  Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.set_max_concurrent_send_jobs client num

let set_max_send_jobs_command =
  Command.rpc ~summary:""
    Command.Spec.(step (fun m v -> m ~num:v) +> anon ("n" %: int))
    set_max_send_jobs

let freeze ~msgid client =
  Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.freeze client msgid
  >>| Or_error.ok_exn

let freeze_command =
  Command.rpc ~summary:"freeze a message in the spool"
    Command.Spec.(
      step (fun m v -> m ~msgid:v)
      +> anon ("msgid" %: msgid)
    )
    freeze
;;

let send_now ?(retry_intervals=[]) ~msgid client =
  Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.send_now client (msgid, retry_intervals)
  >>| Or_error.ok_exn

let send_now_command =
  Command.rpc ~summary:"unfreeze and enqueue the message"
    Command.Spec.(
      step (fun m v -> m ?retry_intervals:(Some v))
      +> flag "retry-interval" (listed time_span) ~doc:"SPAN additional retry intervals (order matters)"
      ++ step (fun m v -> m ~msgid:v)
      +> anon ("msgid" %: msgid)
    )
    send_now
;;

let events client =
  Rpc.Pipe_rpc.dispatch_exn Smtp_rpc_intf.Spool.events client ()
  >>= fun (pipe, _) ->
  Pipe.iter_without_pushback pipe ~f:fun event ->
    printf !"%{sexp:Smtp_spool.Event.t}\n" event

let events_command =
  Command.rpc ~summary:"view the stream of spool events"
    Command.Spec.empty
    events
;;

let command =
  Command.group ~summary:"spool management"
    [ "status"           , status_command
    ; "count"            , count_command
    ; "freeze"           , freeze_command
    ; "send-now"         , send_now_command
    ; "events"           , events_command
    ; "set-max-send-jobs", set_max_send_jobs_command
    ]
;;
