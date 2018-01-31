open Core
open Async
open Async_smtp
open Email_message
open Common

let msgid =
  Command.Param.Arg_type.create
    Smtp_spool.Stable.Message_id.V1.of_string

module Status = struct
  module Format = struct
    type t =
      [ `Ascii_table
      | `Ascii_table_with_max_width of int
      | `Exim
      | `Sexp
      ] [@@deriving sexp]

    let of_string = function
      | "ascii" -> `Ascii_table
      | "exim" -> `Exim
      | "sexp" -> `Sexp
      | str -> Sexp.of_string_conv_exn str [%of_sexp:t]

    let arg_type =
      Command.Param.Arg_type.create of_string

    let param =
      Command.Param.(
        flag "format" (optional_with_default `Ascii_table arg_type)
          ~doc:" Output format for the spool, valid values include 'ascii', 'exim', 'sexp'")
  end

  let dispatch ~format client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.status client ()
    >>| fun status ->
    printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)

  let command =
    let open Command.Let_syntax in
    Command.config_or_rpc ~summary:"list current contents of the spool"
      [%map_open
        let format = Format.param in
        function
        | `Config config ->
          Smtp_spool.status_from_disk config |> Deferred.Or_error.ok_exn
          >>| fun status ->
          printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)
        | `Rpc client ->
          dispatch ~format client
      ]
end

module Count = struct
  let which =
    Command.Param.(
      choose_one ~if_nothing_chosen:(`Default_to `All)
        [ flag "-frozen-only" no_arg ~doc:" Only count frozen messages"
          |> map ~f:(fun b -> Option.some_if b `Only_frozen)
        ; flag "-active-only" no_arg ~doc:" Only count active messages"
          |> map ~f:(fun b -> Option.some_if b `Only_active)
        ])

  let is_frozen = function
    | `Frozen -> true
    | `Send_now
    | `Send_at _
    | `Sending
    | `Removed
    | `Delivered
    | `Quarantined _ -> false

  let is_active = function
    | `Send_now
    | `Send_at _
    | `Sending -> true
    | `Frozen
    | `Removed
    | `Delivered
    | `Quarantined _ -> false

  let dispatch ~which client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.status client ()
    >>| List.filter ~f:(fun message_info ->
      let status = Smtp_spool.Spooled_message_info.status message_info in
      match which with
      | `All -> true
      | `Only_frozen -> is_frozen status
      | `Only_active -> is_active status)
    >>| List.length

  let command =
    let open Command.Let_syntax in
    Command.config_or_rpc ~summary:"print total number of messages in the spool"
      [%map_open
        let which = which in
        function
        | `Config config ->
          Smtp_spool.count_from_disk config
          >>| Or_error.ok_exn
          >>| printf "%d\n"
        | `Rpc client ->
          dispatch ~which client
          >>| printf "%d\n"
      ]
end

module Set_max_send_jobs = struct
  let num =
    Command.Param.(
      anon ("n" %: int))

  let dispatch ~num client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.set_max_concurrent_send_jobs client num

  let command =
    let open Command.Let_syntax in
    Command.rpc ~summary:""
      [%map_open
        let num = num in
        dispatch ~num
      ]
end

module Freeze = struct
  let msgids =
    Command.Param.(
      anon (sequence ("msgid" %: msgid)))

  let dispatch ~msgids client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.freeze client msgids
    >>| Or_error.ok_exn

  let command =
    let open Command.Let_syntax in
    Command.rpc ~summary:"Freeze messages in the spool"
      [%map_open
        let msgids = msgids in
        dispatch ~msgids
      ]
end

module Send = struct
  let retry_intervals =
    Command.Param.(
      flag "retry-interval" (listed time_span)
        ~doc:"SPAN additional retry intervals (order matters)"
      |> map ~f:(List.map ~f:(Smtp_envelope.Retry_interval.create)))

  let param =
    let open Command.Let_syntax in
    [%map_open
      let all = flag "all" no_arg ~doc:" force immediate sending of all messages"
      and frozen =
        flag "frozen" no_arg ~doc:" force immidiate resending of frozen messages only"
      and msgids = anon (sequence ("msgid" %: msgid))
      in
      match msgids with
      | [] when frozen -> `Frozen_only
      | [] when all -> `All_messages
      | [] -> failwith "Must specify either msgids or -all or -frozen"
      | _ when all || frozen ->
        failwith "Can't specify msgids and -all or -frozen"
      | msgids -> `Some_messages msgids
    ]

  let dispatch ?(retry_intervals=[]) send_info client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.send client
      (retry_intervals, send_info)
    >>| Or_error.ok_exn

  let command =
    let open Command.Let_syntax in
    Command.rpc ~summary:"Force immediate sending of messages"
      [%map_open
        let retry_intervals = retry_intervals
        and send_info = param
        in
        dispatch ~retry_intervals send_info
      ]
end

module Remove = struct
  let msgids =
    Command.Param.(
      anon (sequence ("msgid" %: msgid)))

  let dispatch ~msgids client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.remove client msgids
    >>| Or_error.ok_exn

  let command =
    let open Command.Let_syntax in
    Command.rpc ~summary:"Remove messages in the spool"
      [%map_open
        let msgids = msgids in
        dispatch ~msgids
      ]
end


module Recover = struct
  let wrapper =
    let open Command.Let_syntax in
    [%map_open
      let security_warning =
        flag "add-security-warning" no_arg
          ~doc: " wrap the original email in a JS security warning"
      and custom_warning_file =
        flag "add-custom-warning-from-file" (optional string)
          ~doc: "FILE wrap the original email in a custom email"
      in
      let wrapper_of_file file =
        let header = In_channel.read_all file |> Email.of_string in
        Email_wrapper.create_from_email header
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
    ]

  let param =
    let open Command.Let_syntax in
    [%map_open
      let msgids =
        anon (sequence ("msg" %: msgid))
      and quarantine =
        flag "from-quarantine" no_arg ~doc:" recover quarantined messages"
      and remove =
        flag "from-remove" no_arg ~doc:" recover removed messages"
      and wrapper = wrapper
      in
      match quarantine, remove with
      | true, false -> { Smtp_spool.Recover_info.wrapper; msgs = `Quarantined msgids }
      | false, true -> { Smtp_spool.Recover_info.wrapper; msgs = `Removed msgids }
      | _ -> failwith "Must specify exactly one of -from-quarantine or -from-remove"
    ]

  let dispatch recover_info client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.recover client recover_info
    >>| Or_error.ok_exn

  let command =
    let open Command.Let_syntax in
    Command.rpc ~summary:"recover a removed message back into a frozen state"
      [%map_open
        let recover_info = param in
        dispatch recover_info
      ]
end

module Events = struct
  let dispatch client =
    Rpc.Pipe_rpc.dispatch_exn Smtp_rpc_intf.Spool.events client ()
    >>= fun (pipe, _) ->
    Pipe.iter_without_pushback pipe ~f:(fun event ->
      printf !"%{sexp:Smtp_spool.Event.t}\n" event)

  let command =
    Command.rpc ~summary:"view the stream of spool events"
      (Command.Param.return dispatch)
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
