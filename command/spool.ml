open Core
open Async
open Async_smtp
open Common

let msgid = Command.Param.Arg_type.create Smtp_spool.Stable.Message_id.V1.of_string

module Status = struct
  module Format = struct
    type t =
      [ `Ascii_table
      | `Ascii_table_with_max_width of int
      | `Exim
      | `Sexp
      ]
    [@@deriving sexp]

    let of_string = function
      | "ascii" -> `Ascii_table
      | "exim" -> `Exim
      | "sexp" -> `Sexp
      | str -> Sexp.of_string_conv_exn str [%of_sexp: t]
    ;;

    let arg_type = Command.Param.Arg_type.create of_string

    let param =
      Command.Param.(
        flag
          "format"
          (optional_with_default `Ascii_table arg_type)
          ~doc:
            " Output format for the spool, valid values include 'ascii', 'exim', 'sexp'")
    ;;
  end

  let dispatch ~format client =
    let%map status = Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.status client () in
    printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)
  ;;

  let command =
    let open Command.Let_syntax in
    Command.configs_or_rpc
      ~summary:"list current contents of the spool"
      [%map_open
        let format = Format.param in
        function
        | `Configs (_, spool_config) ->
          let open Deferred.Let_syntax in
          let%map status =
            Smtp_spool.status_from_disk spool_config |> Deferred.Or_error.ok_exn
          in
          printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)
        | `Rpc client -> dispatch ~format client]
  ;;
end

module Count = struct
  let which =
    Command.Param.(
      choose_one
        ~if_nothing_chosen:(Default_to `All)
        [ flag "-frozen-only" no_arg ~doc:" Only count frozen messages"
          |> map ~f:(fun b -> Option.some_if b `Only_frozen)
        ; flag "-active-only" no_arg ~doc:" Only count active messages"
          |> map ~f:(fun b -> Option.some_if b `Only_active)
        ])
  ;;

  let is_frozen = function
    | `Frozen -> true
    | `Send_now | `Send_at _ | `Sending | `Removed | `Delivered | `Quarantined _ -> false
  ;;

  let is_active = function
    | `Send_now | `Send_at _ | `Sending -> true
    | `Frozen | `Removed | `Delivered | `Quarantined _ -> false
  ;;

  let dispatch ~which client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.status client ()
    >>| List.filter ~f:(fun message_info ->
      let status = Smtp_spool.Spooled_message_info.status message_info in
      match which with
      | `All -> true
      | `Only_frozen -> is_frozen status
      | `Only_active -> is_active status)
    >>| List.length
  ;;

  let command =
    let open Command.Let_syntax in
    Command.configs_or_rpc
      ~summary:"print total number of messages in the spool"
      [%map_open
        let which = which in
        function
        | `Configs (_, spool_config) ->
          let open Deferred.Let_syntax in
          Smtp_spool.count_from_disk spool_config >>| Or_error.ok_exn >>| printf "%d\n"
        | `Rpc client ->
          let open Deferred.Let_syntax in
          dispatch ~which client >>| printf "%d\n"]
  ;;
end

module Freeze = struct
  let msgids = Command.Param.(anon (sequence ("msgid" %: msgid)))

  let dispatch ~msgids client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.freeze client msgids >>| Or_error.ok_exn
  ;;

  let command =
    let open Command.Let_syntax in
    Command.rpc
      ~summary:"Freeze messages in the spool"
      [%map_open
        let msgids = msgids in
        dispatch ~msgids]
  ;;
end

module Send = struct
  let retry_intervals =
    Command.Param.(
      flag
        "retry-interval"
        (listed Time.Span.arg_type)
        ~doc:"SPAN additional retry intervals (order matters)"
      |> map ~f:(List.map ~f:Smtp_envelope.Retry_interval.create))
  ;;

  let param =
    let open Command.Let_syntax in
    [%map_open
      let all = flag "all" no_arg ~doc:" force immediate sending of all messages"
      and frozen =
        flag "frozen" no_arg ~doc:" force immidiate resending of frozen messages only"
      and msgids = anon (sequence ("msgid" %: msgid)) in
      match msgids with
      | [] when frozen -> `Frozen_only
      | [] when all -> `All_messages
      | [] -> failwith "Must specify either msgids or -all or -frozen"
      | _ when all || frozen -> failwith "Can't specify msgids and -all or -frozen"
      | msgids -> `Some_messages msgids]
  ;;

  let dispatch ?(retry_intervals = []) send_info client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.send client (retry_intervals, send_info)
    >>| Or_error.ok_exn
  ;;

  let command =
    let open Command.Let_syntax in
    Command.rpc
      ~summary:"Force immediate sending of messages"
      [%map_open
        let retry_intervals = retry_intervals
        and send_info = param in
        dispatch ~retry_intervals send_info]
  ;;
end

module Remove = struct
  let msgids = Command.Param.(anon (sequence ("msgid" %: msgid)))

  let dispatch ~msgids client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.remove client msgids >>| Or_error.ok_exn
  ;;

  let command =
    let open Command.Let_syntax in
    Command.rpc
      ~summary:"Remove messages in the spool"
      [%map_open
        let msgids = msgids in
        dispatch ~msgids]
  ;;
end

module Recover = struct
  let param =
    let open Command.Let_syntax in
    [%map_open
      let msgs = anon (sequence ("msg" %: msgid))
      and quarantine = flag "from-quarantine" no_arg ~doc:" recover quarantined messages"
      and remove = flag "from-remove" no_arg ~doc:" recover removed messages" in
      match quarantine, remove with
      | true, false -> { Smtp_spool.Recover_info.msgs; from = `Quarantined }
      | false, true -> { Smtp_spool.Recover_info.msgs; from = `Removed }
      | _ -> failwith "Must specify exactly one of -from-quarantine or -from-remove"]
  ;;

  let dispatch recover_info client =
    Rpc.Rpc.dispatch_exn Smtp_rpc_intf.Spool.recover client recover_info
    >>| Or_error.ok_exn
  ;;

  let command =
    let open Command.Let_syntax in
    Command.rpc
      ~summary:"recover a removed message back into a frozen state"
      [%map_open
        let recover_info = param in
        dispatch recover_info]
  ;;
end

module Events = struct
  let dispatch client =
    let%bind pipe, _ = Rpc.Pipe_rpc.dispatch_exn Smtp_rpc_intf.Spool.events client () in
    Pipe.iter_without_pushback pipe ~f:(fun event ->
      printf !"%{sexp:Smtp_spool.Event.t}\n" event)
  ;;

  let command =
    Command.rpc
      ~summary:"view the stream of spool events"
      (Command.Param.return dispatch)
  ;;
end

let command =
  Command.group
    ~summary:"spool management"
    [ "status", Status.command
    ; "count", Count.command
    ; "freeze", Freeze.command
    ; "send", Send.command
    ; "remove", Remove.command
    ; "recover", Recover.command
    ; "events", Events.command
    ]
;;
