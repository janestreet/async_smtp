module Stable = struct
  open Core.Core_stable
  module Flows = struct
    module V1 = struct
      type t = string list [@@deriving sexp, bin_io]
    end
  end
end

open Core
open Async
open Async_smtp_types

module Level = struct
  type t =
    [ Log.Level.t
    | `Error_no_monitor
    ] [@@deriving sexp]

  let severity = function
    | `Debug            -> 0
    | `Info             -> 1
    | `Error_no_monitor -> 2
    | `Error            -> 3

  let of_async_log_level = function
    | `Debug -> `Debug
    | `Info  -> `Info
    | `Error -> `Error

  let to_async_log_level = function
    | `Debug            -> `Debug
    | `Info             -> `Info
    | `Error_no_monitor -> `Error
    | `Error            -> `Error

  include Comparable.Make(struct
      type nonrec t = t [@@deriving sexp]
      let compare a b = Int.compare (severity a) (severity b)
    end)
end

module Mail_fingerprint = struct
  type t =
    { headers : (string * string) list [@default []]
    ; md5     : string sexp_option
    ; parts   : t list [@default []]
    } [@@deriving sexp, fields]

  let rec of_email email =
    let headers = Email_headers.to_list (Email.headers email) in
    match Email.Content.parse email with
    | Ok (Email.Content.Data _)
    | Error _ ->
      { headers
      ; md5 = Some (Email.raw_content email
                    |> Email.Raw_content.to_bigstring_shared
                    |> Bigstring_shared.to_string
                    |> Md5.digest_string
                    |> Md5.to_hex)
      ; parts = []
      }
    | Ok (Email.Content.Message email) -> of_email email
    | Ok (Email.Content.Multipart { Email.Content.Multipart.parts; _ }) ->
      { headers
      ; md5 = None
      ; parts = List.map parts ~f:(of_email)
      }
end

module Flows = struct
  module Kind = struct
    type t =
      [ `Server_session
      | `Client_session
      | `Inbound_envelope
      | `Outbound_envelope
      | `Cached_connection
      ] [@@deriving sexp_of]
  end
  module Id = struct
    module T = struct
      type t = string [@@deriving sexp_of, compare, hash]
      let tag = function
        | `Server_session -> "srv#"
        | `Client_session -> "cli#"
        | `Inbound_envelope -> "in#"
        | `Outbound_envelope -> "out#"
        | `Cached_connection -> "conn#"
      let create kind =
        sprintf !"%s#%{Uuid}" (tag kind) (Uuid.create ())
      let is t kind =
        String.is_prefix t ~prefix:(tag kind)
    end
    include T
    include Hashable.Make_plain(T)
    include Comparable.Make_plain(T)
    let equal = String.equal
  end
  type t = Id.t list [@@deriving sexp_of]
  let none = []
  let of_list = Fn.id
  let create kind = [ Id.create kind ]
  let union = (@)
  let extend t kind = Id.create kind :: t
  let are_related a =
    List.exists ~f:(List.mem a ~equal:Id.equal)
end

module Component = struct
  module T = struct
    let module_name = "Async_smtp.Smtp_mail_log.Component"
    type t = string list [@@deriving sexp, bin_io, compare, hash]
    let to_string = String.concat ~sep:"/"
    let of_string = String.split ~on:'/'
  end
  include T
  include Identifiable.Make(T)

  let parts t = t

  let join a b =
    a @ b

  let is_parent ~parent t =
    let rec loop = function
      | ([], _) -> true
      | ((ph::pt),(th::tt)) when String.equal ph th ->
        loop (pt,tt)
      | _ -> false
    in
    loop (parent, t)

  let unknown = []

  let is_unknown t =
    t = unknown
end

module Session_marker = struct
  type t =
    [ `Connected
    | `Mail_from
    | `Rcpt_to
    | `Data
    | `Sending
    ] [@@deriving sexp]

  let of_string s = t_of_sexp (Sexp.of_string s)
  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Message = struct
  module Action = (String : Identifiable.S with type t = string)
  module Sender = struct
    type t = [ `Sender of Smtp_envelope.Sender.t | `String of string ]
    let to_string : t -> string = function
      | `String str -> str
      | `Sender sender -> Smtp_envelope.Sender.to_string sender
    let of_string str : t =
      match Smtp_envelope.Sender.of_string str with
      | Ok sender -> `Sender sender
      | Error _ -> `String str
  end

  module Recipient = struct
    type t = [ `Email of Email_address.t | `String of string ]
    let to_string : t -> string = function
      | `String str -> str
      | `Email email -> Email_address.to_string email
    let of_string str : t =
      match Email_address.of_string str with
      | Ok email -> `Email email
      | Error _ -> `String str
  end

  module Tag = Mail_log_tags

  type 'a with_info
    =  flows:Flows.t
    -> component:Component.t
    -> here:Source_code_position.t
    -> ?local_address:Smtp_socket_address.t
    -> ?remote_address:Smtp_socket_address.t
    -> ?email:[ `Fingerprint of Mail_fingerprint.t
              | `Email of Email.t
              | `Envelope of Smtp_envelope.t
              ]
    -> ?message_size:int
    -> ?rfc822_id:string
    -> ?local_id:Smtp_envelope.Id.t
    -> ?sender:Sender.t
    -> ?recipients:Recipient.t list
    -> ?spool_id:string
    -> ?dest:Smtp_socket_address.t
    -> ?command:Smtp_command.t
    -> ?reply:Smtp_reply.t
    -> ?session_marker:Session_marker.t
    -> ?tags:(string * string) list
    -> 'a
  ;;

  let with_info ~f ~flows ~component ~here ?local_address ?remote_address
        ?email ?message_size ?rfc822_id ?local_id ?sender ?recipients ?spool_id
        ?dest ?command ?reply ?session_marker ?(tags=[]) =
    let tags = match reply with
      | Some reply -> (Tag.reply, Smtp_reply.to_string reply) :: tags
      | None -> tags
    in
    let tags = match command with
      | Some command -> (Tag.command, Smtp_command.to_string command) :: tags
      | None -> tags
    in
    let tags = match dest with
      | Some dest -> (Tag.dest, Smtp_socket_address.Stable.V1.to_string dest) :: tags
      | None -> tags
    in
    let tags = match spool_id with
      | Some spool_id -> (Tag.spool_id, spool_id) :: tags
      | None -> tags
    in
    let recipients = match recipients with
      | Some recipients -> Some (recipients)
      | None -> match email with
        | Some (`Envelope envelope) ->
          Some (List.map (Smtp_envelope.recipients envelope) ~f:(fun e -> `Email e))
        | _ -> None
    in
    let recipients = match recipients with
      | None -> []
      (* Ensure that the empty list of recipients is specially encoded *)
      | Some [] -> [`String ""]
      | Some recipients -> recipients
    in
    let tags =
      List.map recipients ~f:(function t -> Tag.recipient, Recipient.to_string t) @ tags
    in
    let sender = match sender with
      | Some _ -> sender
      | None -> match email with
        | Some (`Envelope envelope) -> Some (`Sender (Smtp_envelope.sender envelope))
        | _ -> None
    in
    let tags = match sender with
      | Some sender -> (Tag.sender, Sender.to_string sender) :: tags
      | None -> tags
    in
    let email_fingerprint = match email with
      | Some (`Envelope envelope) -> Some (Mail_fingerprint.of_email (Smtp_envelope.email envelope))
      | Some (`Email email) -> Some (Mail_fingerprint.of_email email)
      | Some (`Fingerprint fingerprint) -> Some fingerprint
      | None -> None
    in
    let tags = match email_fingerprint with
      | Some fingerprint ->
        (Tag.email_fingerprint, Sexp.to_string (Mail_fingerprint.sexp_of_t fingerprint)) :: tags
      | None -> tags
    in
    let message_size =
      match message_size with
      | Some _ -> message_size
      | None ->
        match email with
        | Some (`Envelope envelope) ->
          Some (Smtp_envelope.email envelope
                |> Email.raw_content
                |> Email.Raw_content.length)
        | Some (`Email email) ->
          Some (Email.raw_content email
                |> Email.Raw_content.length)
        | Some (`Fingerprint _) -> None
        | None -> None
    in
    let tags = match message_size with
      | None -> tags
      | Some message_size ->
        (Tag.message_size, Int.to_string message_size) :: tags
    in
    let local_id = match local_id with
      | Some _ -> local_id
      | None -> match email with
        | Some (`Envelope envelope) -> Some (Smtp_envelope.id envelope)
        | _ -> None
    in
    let tags = match local_id with
      | Some local_id -> (Tag.local_id, Smtp_envelope.Id.to_string local_id) :: tags
      | None -> tags
    in
    let rfc822_id = match rfc822_id with
      | Some _ -> rfc822_id
      | None -> match email with
        | Some (`Envelope envelope) ->
          Email_headers.last (Smtp_envelope.email envelope |> Email.headers) "Message-Id"
        | Some (`Email email) -> Email_headers.last (Email.headers email) "Message-Id"
        | Some (`Fingerprint { Mail_fingerprint.headers; _ }) ->
          Email_headers.last (Email_headers.of_list ~whitespace:`Raw headers) "Message-Id"
        | None -> None
    in
    let tags = match rfc822_id with
      | Some rfc822_id -> (Tag.rfc822_id, rfc822_id) :: tags
      | None -> tags
    in
    let tags = match remote_address with
      | Some remote_address -> (Tag.remote_address, Smtp_socket_address.Stable.V1.to_string remote_address) :: tags
      | None -> tags
    in
    let tags = match local_address with
      | Some local_address -> (Tag.local_address, Smtp_socket_address.Stable.V1.to_string local_address) :: tags
      | None -> tags
    in
    let tags = match session_marker with
      | Some event -> (Tag.session_marker, Session_marker.to_string event) :: tags
      | None -> tags
    in
    let tags = List.map flows ~f:(fun f -> Tag.flow, f) @ tags in
    let tags = (Tag.location, Source_code_position.to_string here) :: tags in
    let tags = (Tag.component, Component.to_string component) :: tags in
    f tags

  type t = Log.Message.t [@@deriving sexp_of]

  let create =
    with_info ~f:(fun tags action ->
      Log.Message.create ~tags (`String action))

  let debugf ~flows =
    with_info ~flows ~f:(fun tags fmt ->
      ksprintf (fun msg ->
        Log.Message.create ~tags (`String msg)) fmt)

  let of_error =
    with_info ~f:(fun tags error ->
      Log.Message.create ~tags (`Sexp (Error.sexp_of_t error)))

  let info =
    with_info ~f:(fun tags () ->
      Log.Message.create ~tags (`String "INFO"))

  let with_flow_and_component ~flows ~component t =
    let tags = Log.Message.tags t in
    let tags =
      if List.is_empty component then
        tags
      else
        match List.find tags ~f:(fun (k,_) -> k = Tag.component) with
        | None -> [ Tag.component, Component.to_string component ] @ tags
        | Some (_,base_component) ->
          let tags = List.filter tags ~f:(fun (k,_) -> k <> Tag.component) in
          [ Tag.component, Component.to_string (base_component :: component) ] @ tags
    in
    let tags =
      List.map flows ~f:(fun f -> Tag.flow, f) @ tags
    in
    Log.Message.create
      ~time:(Log.Message.time t)
      ?level:(Log.Message.level t)
      ~tags
      (Log.Message.raw_message t)

  let find_tag' t ~tag ~f =
    List.find_map (Log.Message.tags t) ~f:(fun (k, v) ->
      if k = tag then
        Option.try_with (fun () ->
          f v)
      else None)

  let find_tag = find_tag' ~f:ident

  let is_error_no_monitor t =
    find_tag t ~tag:Tag.error_no_monitor
    |> Option.is_some

  let remove_error_no_monitor_tag t =
    if is_error_no_monitor t
    then (
      let tags =
        Log.Message.tags t |> List.filter ~f:(fun (tag, _) ->
          not (String.equal tag Tag.error_no_monitor))
      in
      Log.Message.create
        ?level:(Log.Message.level t)
        ~time:(Log.Message.time t)
        ~tags
        (Log.Message.raw_message t))
    else t

  let add_error_no_monitor_tag t =
    match find_tag t ~tag:Tag.error_no_monitor with
    | None ->
      Log.Message.add_tags t [Tag.error_no_monitor, ""]
    | Some (_ : string) -> t

  let set_level t level =
    let t, level =
      match level with
      | `Error_no_monitor ->
        add_error_no_monitor_tag t, `Error
      | _ as level ->
        remove_error_no_monitor_tag t, Level.to_async_log_level level
    in
    Log.Message.set_level t (Some level)

  let level t =
    Log.Message.level t
    |> Option.value_map ~f:Level.of_async_log_level ~default:`Info
    |> function
    | `Error ->
      if is_error_no_monitor t
      then `Error_no_monitor
      else `Error
    | _ as level -> level

  let time = Log.Message.time

  let action = Log.Message.message
  let tags = Log.Message.tags

  let component t =
    List.find_map (Log.Message.tags t) ~f:(fun (k,v) ->
      Option.some_if (k = Tag.component) v)
    |> Option.value_map ~f:Component.of_string ~default:Component.unknown

  let rfc822_id = find_tag ~tag:Tag.rfc822_id

  let local_id = find_tag' ~tag:Tag.local_id ~f:Smtp_envelope.Id.of_string

  let spool_id = find_tag ~tag:Tag.spool_id

  let dest = find_tag' ~tag:Tag.dest ~f:Smtp_socket_address.Stable.V1.of_string

  let of_string str ~of_sexp =
    Sexp.of_string_conv_exn str of_sexp

  let sender = find_tag' ~tag:Tag.sender ~f:Sender.of_string

  let recipients t =
    let recipients =
      List.filter_map (Log.Message.tags t) ~f:(fun (k,v) ->
        if k = Tag.recipient then
          Option.try_with (fun () -> Recipient.of_string v)
        else
          None)
    in
    match recipients with
    | [] -> None
    (* Special case to distinguish the empty list from omitting this *)
    | [`String ""] -> Some []
    | recipients -> Some recipients

  let email = find_tag' ~tag:Tag.email_fingerprint ~f:(of_string ~of_sexp:Mail_fingerprint.t_of_sexp)

  let local_address = find_tag' ~tag:Tag.local_address ~f:Smtp_socket_address.Stable.V1.of_string

  let remote_address = find_tag' ~tag:Tag.remote_address ~f:Smtp_socket_address.Stable.V1.of_string

  let command = find_tag' ~tag:Tag.command ~f:Smtp_command.of_string

  let reply = find_tag' ~tag:Tag.reply ~f:Smtp_reply.of_string

  let session_marker = find_tag' ~tag:Tag.session_marker ~f:Session_marker.of_string

  let flows t = List.filter_map (Log.Message.tags t) ~f:(fun (k,v) ->
    Option.some_if (k=Tag.flow) v)
end

type t = Log.t

let error_no_monitor_tag = Message.Tag.error_no_monitor, ""

let message' t ~level msg =
  let log_level =  Level.of_async_log_level (Log.level t) in
  let message_level = Message.level msg in
  if Level.(<=) log_level level then (
    let msg =
      if message_level = level then
        msg
      else
        Message.set_level msg level
    in
    Log.message t msg)

let message t ~level msg =
  let log_level =  Level.of_async_log_level (Log.level t) in
  if Level.(<=) log_level level then (
    let msg = Lazy.force msg in
    let message_level = Message.level msg in
    let msg =
      if message_level = level then
        msg
      else
        Message.set_level msg level
    in
    Log.message t msg)

let debug = message ~level:`Debug
let info = message ~level:`Info
let error ?dont_send_to_monitor =
  if Option.is_some dont_send_to_monitor
  then message ~level:`Error_no_monitor
  else message ~level:`Error

let null_log =
  Log.create
    ~level:`Error
    ~output:[ Log.Output.create ~flush:(fun () -> return ()) (fun _ -> Deferred.unit) ]
    ~on_error:(`Call ignore)

let with_flow_and_component ~flows ~component t =
  Log.create
    ~level:(Log.level t)
    ~output:[ Log.Output.create ~flush:(fun () -> Log.flushed t) (fun msgs ->
      Queue.iter msgs ~f:(fun msg ->
        let level = Message.level msg in
        let log_level = Level.of_async_log_level (Log.level t) in
        if Level.(<=) log_level level then
          message ~level t
            (lazy (Message.with_flow_and_component ~flows ~component msg)));
      return ()) ]
    ~on_error:(`Call (fun err ->
      Log.Global.sexp
        ~level:`Error
        ~tags:([ Message.Tag.component, sprintf !"%{Component}" (component @ ["_LOG"]) ]
               @ List.map flows ~f:(fun f -> Message.Tag.flow, f))
        (Error.sexp_of_t err)))

let adjust_log_levels
      ?(minimum_level=`Debug)
      ?(remap_info_to=`Info)
      ?(remap_error_no_monitor_to=`Error_no_monitor)
      ?(remap_error_to=`Error)
      t =
  let log_level = Level.of_async_log_level (Log.level t) in
  let minimum_level = Level.max log_level minimum_level in
  if
    Level.(<) remap_info_to minimum_level &&
    Level.(<) remap_error_no_monitor_to minimum_level &&
    Level.(<) remap_error_to minimum_level
  then
    null_log
  else if
    minimum_level = log_level &&
    remap_info_to = `Info &&
    remap_error_no_monitor_to = `Error_no_monitor &&
    remap_error_to = `Error
  then
    t
  else
    Log.create
      ~level:(Level.to_async_log_level minimum_level)
      ~output:[ Log.Output.create ~flush:(fun () -> Log.flushed t) (fun msgs ->
        Queue.iter msgs ~f:(fun msg ->
          let level = match Message.level msg with
            | `Debug -> `Debug
            | `Info  -> remap_info_to
            | `Error_no_monitor -> remap_error_no_monitor_to
            | `Error -> remap_error_to
          in
          if Level.(<=) minimum_level level then
            message' ~level t msg);
        return ()) ]
      ~on_error:(`Call (fun err ->
        Log.Global.sexp
          ~level:`Error
          ~tags:[Message.Tag.component, "_LOG"]
          (Error.sexp_of_t err)))
