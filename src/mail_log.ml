open Core
open Async
open Email_message.Std

open Json_wheel_jane_street_overlay.Std
module J = struct
  include Json_type.Build
  let array_of f v = List.map ~f v |> array
  let val_or_array_of f = function
    | [ v ] -> f v
    | vs -> array_of f vs
end

(* Level with a comparison function that has our desired behaviour *)
module Level : Comparable.S with type t := Log.Level.t = struct
  include Comparable.Make(struct
      type t = Log.Level.t [@@deriving sexp]
      let compare a b = match a, b with
        | `Debug, `Debug -> 0
        | `Debug, (`Info | `Error) -> -1
        | `Info, `Debug -> 1
        | `Info, `Info -> 0
        | `Info, `Error -> -1
        | `Error, (`Debug | `Info) -> 1
        | `Error, `Error -> 0
    end)

  let%test _ = `Debug = `Debug
  let%test _ = `Debug < `Info
  let%test _ = `Debug < `Error
  let%test _ = `Info > `Debug
  let%test _ = `Info = `Info
  let%test _ = `Info < `Error
  let%test _ = `Error > `Debug
  let%test _ = `Error > `Info
  let%test _ = `Error = `Error
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
                    |> Bigstring_shared.to_string
                    |> Digest.string
                    |> Digest.to_hex)
      ; parts = []
      }
    | Ok (Email.Content.Message email) -> of_email email
    | Ok (Email.Content.Multipart { Email.Content.Multipart.parts; _ }) ->
      { headers
      ; md5 = None
      ; parts = List.map parts ~f:(of_email)
      }

  let json_of_headers headers =
    String.Map.of_alist_multi headers
    |> Map.to_alist
    |> List.Assoc.map ~f:(function
      | [v] -> J.string v
      | vs -> J.array (List.rev_map vs ~f:J.string))
    |> J.objekt

  let rec json_of_t t =
    let j = [] in
    let j = match t.parts with
      | [] -> j
      | parts -> ("parts", J.array (List.map ~f:json_of_t parts)) :: j
    in
    let j = match t.md5 with
      | None -> j
      | Some md5 -> ("md5", J.string md5) :: j
    in
    let j = ("headers", json_of_headers t.headers) :: j in
    J.objekt j
end

module Here = struct
  let to_string (here:Lexing.position) =
    sprintf "%s:%d" here.pos_fname here.pos_lnum
end

module Flows = struct
  module Kind = struct
    type t =
      [ `Server_session
      | `Client_session
      | `Inbound_envelope
      | `Outbound_envelope
      | `Cached_connection
      ] [@@deriving sexp, bin_io]
  end
  module Id = struct
    module T = struct
      type t = string [@@deriving bin_io, sexp, compare, hash]
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
    include Hashable.Make(T)
    include Comparable.Make(T)
    let equal = String.equal
  end
  type t = Id.t list [@@deriving bin_io, sexp]
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
    let module_name = "Async_smtp.Mail_log.Component"
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
    type t = [ `Sender of Sender.t | `String of string ]
    let to_string : t -> string = function
      | `String str -> str
      | `Sender sender -> Sender.to_string sender
    let of_string str : t =
      match Sender.of_string str with
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

  module Tag = struct
    let component         = "component"
    let spool_id          = "spool-id"
    let rfc822_id         = "rfc822-id"
    let local_id          = "local-id"
    let sender            = "sender"
    let recipient         = "recipient"
    let email_fingerprint = "email-fingerprint"
    let local_address     = "local-address"
    let remote_address    = "remote-address"
    let command           = "command"
    let dest              = "dest"
    let reply             = "reply"
    let session_marker    = "session_marker"
    let flow              = "flow"
    let location          = "location"
  end

  type 'a with_info
    =  flows:Flows.t
    -> component:Component.t
    -> here:Lexing.position
    -> ?local_address:Address.t
    -> ?remote_address:Address.t
    -> ?email:[ `Fingerprint of Mail_fingerprint.t
              | `Email of Email.t
              | `Envelope of Envelope.t
              ]
    -> ?rfc822_id:string
    -> ?local_id:Envelope.Id.t
    -> ?sender:Sender.t
    -> ?recipients:Recipient.t list
    -> ?spool_id:string
    -> ?dest:Address.t
    -> ?command:Smtp_command.t
    -> ?reply:Smtp_reply.t
    -> ?session_marker:Session_marker.t
    -> ?tags:(string * string) list
    -> 'a
  ;;

  let with_info ~f ~flows ~component ~here ?local_address ?remote_address
        ?email ?rfc822_id ?local_id ?sender ?recipients ?spool_id
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
      | Some dest -> (Tag.dest, Address.to_string dest) :: tags
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
          Some (List.map (Envelope.recipients envelope) ~f:(fun e -> `Email e))
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
        | Some (`Envelope envelope) -> Some (`Sender (Envelope.sender envelope))
        | _ -> None
    in
    let tags = match sender with
      | Some sender -> (Tag.sender, Sender.to_string sender) :: tags
      | None -> tags
    in
    let email_fingerprint = match email with
      | Some (`Envelope envelope) -> Some (Mail_fingerprint.of_email (Envelope.email envelope))
      | Some (`Email email) -> Some (Mail_fingerprint.of_email email)
      | Some (`Fingerprint fingerprint) -> Some fingerprint
      | None -> None
    in
    let tags = match email_fingerprint with
      | Some fingerprint ->
        (Tag.email_fingerprint, sprintf !"%{sexp:Mail_fingerprint.t}" fingerprint) :: tags
      | None -> tags
    in
    let local_id = match local_id with
      | Some _ -> local_id
      | None -> match email with
        | Some (`Envelope envelope) -> Some (Envelope.id envelope)
        | _ -> None
    in
    let tags = match local_id with
      | Some local_id -> (Tag.local_id, Envelope.Id.to_string local_id) :: tags
      | None -> tags
    in
    let rfc822_id = match rfc822_id with
      | Some _ -> rfc822_id
      | None -> match email with
        | Some (`Envelope envelope) ->
          Email_headers.last (Envelope.email envelope |> Email.headers) "Message-Id"
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
      | Some remote_address -> (Tag.remote_address, Address.to_string remote_address) :: tags
      | None -> tags
    in
    let tags = match local_address with
      | Some local_address -> (Tag.local_address, Address.to_string local_address) :: tags
      | None -> tags
    in
    let tags = match session_marker with
      | Some event -> (Tag.session_marker, Session_marker.to_string event) :: tags
      | None -> tags
    in
    let tags = List.map flows ~f:(fun f -> Tag.flow, f) @ tags in
    let tags = (Tag.location, Here.to_string here) :: tags in
    let tags = (Tag.component, Component.to_string component) :: tags in
    f tags

  type t = Log.Message.t [@@deriving sexp_of]

  let json_of_t =
    let tag name to_json = name, { Logstash_conv.Message.field = name; to_json } in
    let tag1 name to_json = tag name (J.val_or_array_of to_json) in
    Logstash_conv.Message.json_of_t' ()
      ~tags:
        [ tag1 Tag.component   J.string
        ; tag1 Tag.spool_id    J.string
        ; tag1 Tag.rfc822_id   J.string
        ; tag1 Tag.local_id    J.string
        ; tag1 Tag.sender      J.string
        ; tag  Tag.recipient
            (fun strs ->
               List.filter_map strs ~f:(function
                 | "" -> None
                 | str -> Some (J.string str))
               |> J.array)
        ; tag1 Tag.email_fingerprint
            (fun str ->
               Sexp.of_string_conv_exn str Mail_fingerprint.t_of_sexp
               |> Mail_fingerprint.json_of_t)
        ; tag1 Tag.local_address J.string
        ; tag1 Tag.remote_address J.string
        ; tag1 Tag.command J.string
        ; tag1 Tag.dest (fun str ->
            match Sexp.of_string_conv_exn str Address.t_of_sexp with
            | `Unix file -> J.(objekt [ "unix", string file ])
            | `Inet hp -> J.(objekt [ "inet", string (Host_and_port.to_string hp) ])
            | exception _ -> J.string str)
        ; tag1 Tag.reply J.string
        ; tag1 Tag.session_marker J.string
        ; tag  Tag.flow J.(array_of string)
        ; tag1 Tag.location J.string
        ]
    |> Staged.unstage

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

  let level t = Log.Message.level t |> Option.value ~default:`Info

  let time = Log.Message.time

  let action = Log.Message.message
  let tags = Log.Message.tags

  let component t =
    List.find_map (Log.Message.tags t) ~f:(fun (k,v) ->
      Option.some_if (k = Tag.component) v)
    |> Option.value_map ~f:Component.of_string ~default:Component.unknown

  let find_tag' t ~tag ~f =
    List.find_map (Log.Message.tags t) ~f:(fun (k, v) ->
      if k = tag then
        Option.try_with (fun () ->
          f v)
      else None)

  let find_tag = find_tag' ~f:ident

  let rfc822_id = find_tag ~tag:Tag.rfc822_id

  let local_id = find_tag' ~tag:Tag.local_id ~f:Envelope.Id.of_string

  let spool_id = find_tag ~tag:Tag.spool_id

  let dest = find_tag' ~tag:Tag.dest ~f:Address.of_string

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

  let local_address = find_tag' ~tag:Tag.local_address ~f:Address.of_string

  let remote_address = find_tag' ~tag:Tag.remote_address ~f:Address.of_string

  let command = find_tag' ~tag:Tag.command ~f:Smtp_command.of_string

  let reply = find_tag' ~tag:Tag.reply ~f:Smtp_reply.of_string

  let session_marker = find_tag' ~tag:Tag.session_marker ~f:Session_marker.of_string

  let flows t = List.filter_map (Log.Message.tags t) ~f:(fun (k,v) ->
    Option.some_if (k=Tag.flow) v)

end

type t = Log.t

let message' t ~level msg =
  if Level.(<=) (Log.level t) level &&
     Level.(<=) level (Option.value (Log.Message.level msg) ~default:level) then
    let msg =
      if Log.Message.level msg = Some level then
        msg
      else
        Log.Message.set_level msg (Some level)
    in
    Log.message t msg

let message t ~level msg =
  if Level.(<=) (Log.level t) level then
    let msg = Lazy.force msg in
    let msg =
      if Log.Message.level msg = Some level then
        msg
      else
        Log.Message.set_level msg (Some level)
    in
    Log.message t msg

let debug = message ~level:`Debug
let info = message ~level:`Info
let error = message ~level:`Error

let null_log =
  Log.create
    ~level:`Error
    ~output:[ Log.Output.create (fun _ -> Deferred.unit) ]
    ~on_error:(`Call ignore)

let with_flow_and_component ~flows ~component t =
  Log.create
    ~level:(Log.level t)
    ~output:[ Log.Output.create (fun msgs ->
      Queue.iter msgs ~f:(fun msg ->
        let level = Message.level msg in
        if Level.(<=) (Log.level t) level then
          message ~level t
            (lazy (Message.with_flow_and_component ~flows ~component msg)));
      Log.flushed t) ]
    ~on_error:(`Call (fun err ->
      Log.Global.sexp
        ~level:`Error
        ~tags:([ Message.Tag.component, sprintf !"%{Component}" (component @ ["_LOG"]) ]
               @ List.map flows ~f:(fun f -> Message.Tag.flow, f))
        (Error.sexp_of_t err)))

let adjust_log_levels ?(minimum_level=`Debug) ?(remap_info_to=`Info) ?(remap_error_to=`Error) t =
  let minimum_level = Level.max (Log.level t) minimum_level in
  if Level.(<) remap_info_to minimum_level && Level.(<) remap_error_to minimum_level then
    null_log
  else if minimum_level = Log.level t && remap_info_to = `Info && remap_error_to = `Error then
    t
  else
    Log.create
      ~level:minimum_level
      ~output:[ Log.Output.create (fun msgs ->
        Queue.iter msgs ~f:(fun msg ->
          let level = match Log.Message.level msg with
            | None | Some `Info -> remap_info_to
            | Some `Error -> remap_error_to
            | Some `Debug -> `Debug
          in
          if Level.(<=) minimum_level level then
            message' ~level t msg);
        Log.flushed t) ]
      ~on_error:(`Call (fun err ->
        Log.Global.sexp
          ~level:`Error
          ~tags:[Message.Tag.component, "_LOG"]
          (Error.sexp_of_t err)))
