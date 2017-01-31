open Core
open Async.Std
open Async_smtp.Std
open Common


let msgid =
  Command.Spec.Arg_type.create
    Smtp_spool.Message_id.of_string

module Status = struct
  type format = [ `Ascii_table | `Ascii_table_with_max_width of int | `Exim | `Sexp ] [@@deriving sexp]

  let format =
    Command.Spec.Arg_type.create
      (function
        | "ascii" -> `Ascii_table
        | "exim" -> `Exim
        | "sexp" -> `Sexp
        | str ->
          str |> Sexp.of_string |> format_of_sexp
      )

  let spec () =
    Command.Spec.(
      step (fun m v -> m ~format:v)
      +> flag "format" (optional_with_default `Ascii_table format)
           ~doc:" Output format for the spool, valid values include 'ascii', 'exim', 'sexp'"
    )

  let dispatch ~format client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.status client ()
    >>| fun status ->
    printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)

  let command =
    Command.config_or_rpc ~summary:"list current contents of the spool"
      (spec ())
      (fun ~format -> function
         | `Config config ->
           Smtp_spool.status_from_disk config |> Deferred.Or_error.ok_exn
           >>| fun status ->
           printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)
         | `Rpc client ->
           dispatch ~format client
      )
end

module Count = struct
  let spec () = Command.Spec.empty

  let dispatch client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.status client ()
    >>| List.length
    >>| printf "%d\n"

  let command =
    Command.config_or_rpc ~summary:"print total number of messages in the spool"
      (spec ())
      (function
        | `Config config ->
          Smtp_spool.count_from_disk config
          >>| Or_error.ok_exn
          >>| printf "%d\n"
        | `Rpc client ->
          dispatch client
      )
end

module Set_max_send_jobs = struct
  let spec () = Command.Spec.(step (fun m v -> m ~num:v) +> anon ("n" %: int))

  let dispatch ~num client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.set_max_concurrent_send_jobs client num

  let command = Command.rpc ~summary:"" (spec ()) dispatch
end

module Freeze = struct
  let spec () =
    Command.Spec.(
      step (fun m v -> m ~msgids:v)
      +> anon (sequence ("msgid" %: msgid))
    )

  let dispatch ~msgids client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.freeze client msgids
    >>| Or_error.ok_exn

  let command = Command.rpc ~summary:"Freeze messages in the spool" (spec ()) dispatch
end

module Send = struct
  let spec () =
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

  let dispatch ?(retry_intervals=[]) send_info client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.send client
      (retry_intervals, send_info)
    >>| Or_error.ok_exn

  let command = Command.rpc ~summary:"Force immediate sending of messages" (spec ()) dispatch
end

module Remove = struct
  let spec () =
    Command.Spec.(
      step (fun m v -> m ~msgids:v)
      +> anon (sequence ("msgid" %: msgid))
    )

  let dispatch ~msgids client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.remove client msgids
    >>| Or_error.ok_exn

  let command = Command.rpc ~summary:"Remove messages in the spool" (spec ()) dispatch
end


module Recover = struct
  let spec () =
    Command.Spec.(
      step (fun m ~msgids ~quarantine ~remove ~security_warning ~custom_warning_file ->
        let wrapper =
          let wrapper_of_file file =
            let header = In_channel.read_all file |> Email.of_string in
            Email_message.Wrapper.create_from_email header
          in
          match security_warning, custom_warning_file with
          | true, None ->
            wrapper_of_file "/j/office/app/mailcore/prod/var/security_warning"
            |> Option.some
          | false, Some custom_warning_file ->
            wrapper_of_file custom_warning_file |> Option.some
          | true, Some _ ->
            failwith "Can't specify -add-security-warning and -add-custom-warning"
          | false, None -> None
        in
        match quarantine, remove with
        | true, false -> m { Smtp_spool.Recover_info.wrapper; msgs = `Quarantined msgids }
        | false, true -> m { Smtp_spool.Recover_info.wrapper; msgs = `Removed msgids }
        | _ -> failwith "Must specify exactly one of -from-quarantine or -from-remove")
      ++ step (fun m v -> m ~msgids:v)
      +> anon (sequence ("msg" %: msgid))
      ++ step (fun m v -> m ~quarantine:v)
      +> flag "from-quarantine" no_arg ~doc:" recover quarantined messages"
      ++ step (fun m v -> m ~remove:v)
      +> flag "from-remove" no_arg ~doc:" recover removed messages"
      ++ step (fun m v -> m ~security_warning:v)
      +> flag "add-security-warning" no_arg
           ~doc: " wrap the original email in a JS security warning"
      ++ step (fun m v -> m ~custom_warning_file:v)
      +> flag "add-custom-warning-from-file" (optional string)
           ~doc: "FILE wrap the original email in a custom email"
    )

  let dispatch recover_info client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.recover client recover_info
    >>| Or_error.ok_exn

  let command =
    Command.rpc ~summary:"recover a removed message back into a frozen state"
      (spec ())
      dispatch
end

module Events = struct
  let spec () = Command.Spec.empty

  let dispatch client =
    Rpc.Pipe_rpc.dispatch_exn Smtp_rpc_intf.Spool.events client ()
    >>= fun (pipe, _) ->
    Pipe.iter_without_pushback pipe ~f:(fun event ->
      printf !"%{sexp:Smtp_spool.Event.t}\n" event)

  let command =
    Command.rpc ~summary:"view the stream of spool events" (spec ()) dispatch
end

let command =
  Command.group ~summary:"spool management"
    [ "status"           , Status.command
    ; "count"            , Count.command
    ; "freeze"           , Freeze.command
    ; "send"             , Send.command
    ; "remove"           , Remove.command
    ; "recover"          , Recover.command
    ; "events"           , Events.command
    ; "set-max-send-jobs", Set_max_send_jobs.command
    ]
;;
