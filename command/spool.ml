open Core
open Async
open! Async_smtp
open Common
module Time = Time_float_unix

let msgid = Command.Param.Arg_type.create Smtp_spool.Stable.Message_id.V1.of_string

module Client_side_filter = struct
  type t =
    { next_hop : Re2.t option
    ; sender : Re2.t option
    ; recipient : Re2.t option
    ; queue : Smtp_spool_message.Queue.t option
    ; younger_than : Time_float_unix.Span.t option
    ; older_than : Time_float_unix.Span.t option
    }
  [@@deriving fields ~iterators:fold]

  let has_some t =
    let or_is_some acc field = acc || Option.is_some (Field.get field t) in
    Fields.fold
      ~init:false
      ~next_hop:or_is_some
      ~sender:or_is_some
      ~recipient:or_is_some
      ~queue:or_is_some
      ~younger_than:or_is_some
      ~older_than:or_is_some
  ;;

  let param_opt =
    let open Command.Let_syntax in
    let open Command.Param in
    let re_opt_param ~name =
      flag
        ~doc:[%string "REGEX filter %{name} by regex"]
        name
        (Arg_type.create Re2.of_string |> optional)
    in
    let than_opt_param ~name =
      flag ~doc:[%string "TIME %{name} than"] name (optional Time.Span.arg_type)
    in
    let%map_open next_hop = re_opt_param ~name:"next-hop"
    and sender = re_opt_param ~name:"sender"
    and recipient = re_opt_param ~name:"recipient"
    and queue =
      flag
        ~doc:"QUEUE the status/queue of the message"
        ~aliases:[ "status" ]
        "queue"
        (Arg_type.enumerated (module Smtp_spool_message.Queue) |> optional)
    and younger_than = than_opt_param ~name:"younger"
    and older_than = than_opt_param ~name:"older" in
    (* If all fields are [None], return [None]. Only return [Some t] if [Some] field has a
       value. *)
    let t = { next_hop; sender; recipient; queue; younger_than; older_than } in
    Option.some_if (has_some t) t
  ;;

  let matches ~now t msg =
    let module S = Smtp_spool.Spooled_message_info in
    let and_matches m field ~f =
      match m with
      | false -> false
      | true -> Option.value_map ~default:true (Field.get field t) ~f
    in
    let and_matches_re m field ~get_values ~value_to_string =
      and_matches m field ~f:(fun re ->
        get_values msg |> List.map ~f:value_to_string |> List.exists ~f:(Re2.matches re))
    in
    Fields.fold
      ~init:true
      ~next_hop:
        (and_matches_re
           ~get_values:S.next_hop_choices
           ~value_to_string:Host_and_port.to_string)
      ~sender:
        (and_matches_re
           ~get_values:(fun msg -> [ S.envelope_info msg |> Smtp_envelope.Info.sender ])
           ~value_to_string:Smtp_envelope.Sender.to_string)
      ~recipient:
        (and_matches_re
           ~get_values:(fun msg -> S.envelope_info msg |> Smtp_envelope.Info.recipients)
           ~value_to_string:Email_address.to_string)
      ~queue:
        (and_matches ~f:(fun queue ->
           S.status msg
           |> Smtp_spool_message.Queue.of_status
           |> Option.value_map
                ~default:false
                ~f:([%compare.equal: Smtp_spool_message.Queue.t] queue)))
      ~younger_than:
        (and_matches ~f:(fun younger_than ->
           Time.( >= ) (S.spool_date msg) (Time.sub now younger_than)))
      ~older_than:
        (and_matches ~f:(fun older_than ->
           Time.( <= ) (S.spool_date msg) (Time.sub now older_than)))
  ;;

  let filter ?now t msgs =
    let now = Option.value_or_thunk now ~default:Time.now in
    List.filter msgs ~f:(matches ~now t)
  ;;

  let filter_opt ?now t msgs =
    match t with
    | None -> msgs
    | Some t -> filter ?now t msgs
  ;;
end

module Status = struct
  module Format = struct
    type t =
      [ `Ascii_table
      | `Ascii_table_with_max_width of int
      | `Exim
      | `Sexp
      | `Id
      ]
    [@@deriving sexp]

    let of_string = function
      | "ascii" -> `Ascii_table
      | "exim" -> `Exim
      | "sexp" -> `Sexp
      | "id" -> `Id
      | str -> Sexp.of_string_conv_exn str [%of_sexp: t]
    ;;

    let arg_type = Command.Param.Arg_type.create of_string

    let param =
      Command.Param.(
        flag
          "format"
          (optional_with_default `Ascii_table arg_type)
          ~doc:
            " Output format for the spool, valid values include 'ascii', 'exim', 'sexp', \
             'id'")
    ;;
  end

  let dispatch ~format ?client_side_filter client =
    let%map status =
      Smtp_rpc_intf.Spool.Status.dispatch client
      >>| ok_exn
      >>| Client_side_filter.filter_opt client_side_filter
    in
    printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)
  ;;

  let on_disk ~format ?client_side_filter config =
    let%map status =
      Smtp_spool.status_from_disk config
      |> Deferred.Or_error.ok_exn
      >>| Client_side_filter.filter_opt client_side_filter
    in
    printf "%s\n" (Smtp_spool.Status.to_formatted_string ~format status)
  ;;

  let command =
    let open Command.Let_syntax in
    Command.configs_or_rpc
      ~summary:"list current contents of the spool"
      [%map_open
        let format = Format.param
        and client_side_filter = Client_side_filter.param_opt in
        function
        | `Configs (_, spool_config) -> on_disk ~format ?client_side_filter spool_config
        | `Rpc client -> dispatch ~format ?client_side_filter client]
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

  let dispatch ~which ?client_side_filter client =
    Smtp_rpc_intf.Spool.Status.dispatch client
    >>| ok_exn
    >>| Client_side_filter.filter_opt client_side_filter
    >>| List.filter ~f:(fun message_info ->
      let status = Smtp_spool.Spooled_message_info.status message_info in
      match which with
      | `All -> true
      | `Only_frozen -> is_frozen status
      | `Only_active -> is_active status)
    >>| (List.length :> _ -> _)
  ;;

  let on_disk ?client_side_filter config =
    let%map count =
      match client_side_filter with
      | None -> Smtp_spool.count_from_disk config >>| Or_error.ok_exn
      | Some client_side_filter ->
        let%map l =
          Smtp_spool.status_from_disk config
          |> Deferred.Or_error.ok_exn
          >>| Client_side_filter.filter client_side_filter
        in
        List.length l
    in
    printf "%d\n" count
  ;;

  let command =
    let open Command.Let_syntax in
    Command.configs_or_rpc
      ~summary:"print total number of messages in the spool"
      [%map_open
        let which = which
        and client_side_filter = Client_side_filter.param_opt in
        function
        | `Configs (_, spool_config) -> on_disk ?client_side_filter spool_config
        | `Rpc client ->
          let open Deferred.Let_syntax in
          dispatch ~which ?client_side_filter client >>| printf "%d\n"]
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
    Command.rpc ~summary:"view the stream of spool events" (Command.Param.return dispatch)
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
