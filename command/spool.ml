open Core.Std
open Async.Std
open Async_smtp.Std
open Common

type format = [ `Ascii_table | `Ascii_table_with_max_width of int | `Exim | `Sexp ] [@@deriving sexp]

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

let freeze ~msgids client =
  Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.freeze client msgids
  >>| Or_error.ok_exn

let freeze_command =
  Command.rpc ~summary:"Freeze messages in the spool"
    Command.Spec.(
      step (fun m v -> m ~msgids:v)
      +> anon (sequence ("msgid" %: msgid))
    )
    freeze
;;

let send ?(retry_intervals=[]) send_info client =
  Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.send client
    (retry_intervals, send_info)
  >>| Or_error.ok_exn

let remove ~msgids client =
  Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.remove client msgids
  >>| Or_error.ok_exn

let remove_command =
  Command.rpc ~summary:"Remove messages in the spool"
    Command.Spec.(
      step (fun m v -> m ~msgids:v)
      +> anon (sequence ("msgid" %: msgid))
    )
    remove
;;

let recover recover_info client =
  Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.recover client recover_info
  >>| Or_error.ok_exn

let recover_command =
  Command.rpc ~summary:"recover a removed message back into a frozen state"
    Command.Spec.(
      step (fun m ~msgids ~quarantine ~remove  ->
        match quarantine, remove with
        | true, false -> m (`Quarantined msgids)
        | false, true -> m (`Removed msgids)
        | _ -> failwith "Must specify exactly one of -from-quarantine or -from-remove")
      ++ step (fun m v -> m ~msgids:v)
      +> anon (sequence ("msg" %: msgid))
      ++ step (fun m v -> m ~quarantine:v)
      +> flag "from-quarantine" no_arg ~doc:" recover quarantined messages"
      ++ step (fun m v -> m ~remove:v)
      +> flag "from-remove" no_arg ~doc:" recover removed messages"
    )
    recover
;;

let send_command =
  Command.rpc ~summary:"Force immediate sending of messages"
    Command.Spec.(
      step (fun m v -> m ?retry_intervals:(Some v))
      +> flag "retry-interval" (listed time_span) ~doc:"SPAN additional retry \
                                                        intervals (order matters)"
      ++ step (fun m ~all ~frozen -> function
        | [] when frozen -> m `Frozen_only
        | [] when all -> m `All_messages
        | [] -> failwith "Must specify either msgids or -all or -frozen"
        | _ when all || frozen ->
          failwith "Can't specify msgids and -all or -frozen"
        | msgids -> m (`Some_messages msgids))
      ++ step (fun m v -> m ~all:v)
      +> flag "all" no_arg ~doc:" force immediate sending of all messages"
      ++ step (fun m v -> m ~frozen:v)
      +> flag "frozen" no_arg ~doc:" force immidiate resending of frozen messages only"
      +> anon (sequence ("msgid" %: msgid))
    )
    send
;;

let events client =
  Rpc.Pipe_rpc.dispatch_exn Smtp_rpc_intf.Spool.events client ()
  >>= fun (pipe, _) ->
  Pipe.iter_without_pushback pipe ~f:(fun event ->
    printf !"%{sexp:Smtp_spool.Event.t}\n" event)

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
    ; "send"             , send_command
    ; "remove"           , remove_command
    ; "recover"          , recover_command
    ; "events"           , events_command
    ; "set-max-send-jobs", set_max_send_jobs_command
    ]
;;
