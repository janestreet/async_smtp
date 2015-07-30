open Core.Std
open Async.Std
open Async_ssl.Std
open Types

module Config = Client_config

module Peer_info = struct
  type t =
    { dest : Host_and_port.t
    ; greeting : string Set_once.t
    ; hello : [`Simple of string | `Extended of string * (Extension.t list) ] Set_once.t
    } with sexp, fields

  let create ~dest () =
    { dest
    ; greeting = Set_once.create ()
    ; hello = Set_once.create ()
    }

  let set field t value =
    match Set_once.set (Field.get field t) value with
    | Ok () -> Ok ()
    | Error s ->
      Error (Error.tag (Error.of_string s) (Field.name field))

  let set_greeting = set Fields.greeting
  let set_hello = set Fields.hello

  let extensions t =
    match Set_once.get t.hello with
    | None | Some (`Simple _) -> None
    | Some (`Extended (_,extensions)) -> Some extensions

  let supports_extension t extension =
    match extensions t with
    | None -> false
    | Some extensions -> List.mem extensions extension

  let domain t =
    Host_and_port.host t.dest

  let greeting t =
    Set_once.get t.greeting

  let hello t =
    Set_once.get t.hello
end

module Bsmtp = struct
  type t = { writer : Writer.t } with fields

  let create ~writer =
    { writer }
end

module Plain = struct
  type t =
    { reader : Reader.t
    ; writer : Writer.t
    ; info : Peer_info.t
    } with fields

  let create ~reader ~writer ~info =
    { reader; writer; info }
end

module Tls = struct
  type t =
    { reader : Reader.t
    ; writer : Writer.t
    ; info : Peer_info.t
    ; tls : Ssl.Connection.t
    } with fields

  let create ~reader ~writer ~info ~tls =
    { reader; writer; info; tls }
end

(* The reason we don't keep info at the top is that switching from plain to tls requires
   us to forget the info, so we are making it less likely that we forget to forget. *)
type t =
  { config : Config.t
  ; session_id : string
  ; log : Log.t option
  (* The only allowed transition is from Plain to Tls. *)
  ; mutable mode : [ `Bsmtp of Bsmtp.t
                   | `Plain of Plain.t
                   | `Tls of Tls.t
                   ]
  } with fields

(* Debug stuff *)
let has_log t = Option.is_some (log t)
module Log = struct
  let debug t =
    ksprintf (fun msg ->
      let session_id = session_id t in
      match log t with
      | Some log ->
        Log.debug ~tags:["session_id",session_id] log "[%s] %s" session_id msg
      | None ->
        ())

  let error t =
    ksprintf (fun msg ->
      let session_id = session_id t in
      match log t with
      | Some log ->
        Log.error ~tags:["session_id",session_id] log "[%s] %s" session_id msg
      | None ->
        ())
end

let create
      ?log
      ~session_id
      ~dest
      reader
      writer
      config =
  let info = Peer_info.create ~dest () in
  let mode = `Plain (Plain.create ~reader ~writer ~info) in
  { mode; session_id; log; config }

let create_bsmtp
      ?log
      ~session_id
      writer
      config =
  let mode = `Bsmtp (Bsmtp.create ~writer) in
  { mode; session_id; log; config }

let reader t =
  match t.mode with
  | `Bsmtp _ -> None
  | `Plain plain -> Some (Plain.reader plain)
  | `Tls tls -> Some (Tls.reader tls)

let writer t =
  match t.mode with
  | `Bsmtp bsmtp -> Bsmtp.writer bsmtp
  | `Plain plain -> Plain.writer plain
  | `Tls tls -> Tls.writer tls

let info t =
  match t.mode with
  | `Bsmtp _ -> None
  | `Plain plain -> Some (Plain.info plain)
  | `Tls tls -> Some (Tls.info tls)

let supports_extension t extension =
  match info t with
  | None -> false
  | Some info -> Peer_info.supports_extension info extension

let info_exn t =
  Option.value_exn (info t)

let tls t =
  match t.mode with
  | `Bsmtp _ | `Plain _ -> None
  | `Tls tls -> Some (Tls.tls tls)

let is_using_tls t =
  Option.is_some (tls t)

let read_reply reader =
  let rec loop partial =
    Reader.read_line reader
    >>| (function
      | `Ok line -> Ok line
      | `Eof -> Or_error.error_string "Unexpected EOF")
    >>=? fun line ->
    match Reply.parse ?partial line with
    | `Done reply -> Deferred.Or_error.return reply
    | `Partial partial -> loop (Some partial)
  in
  Deferred.Or_error.try_with_join
    (fun () -> loop None)

let receive ?(timeout=Time.Span.of_sec 30.) t =
  match reader t with
  | None -> Deferred.Or_error.return `Bsmtp
  | Some reader ->
    Clock.with_timeout timeout (read_reply reader)
    >>| function
    | `Result (Ok v) ->
      if has_log t then Log.debug t !"<- %{Reply}" v;
      Ok (`Received v)
    | `Result (Error e) -> Error e
    | `Timeout ->
      Or_error.error_string
        (sprintf !"Timeout %{Time.Span} waiting for reply" timeout)

let send t cmd =
  Deferred.Or_error.try_with (fun () ->
    if has_log t then Log.debug t !"-> %{Command}" cmd;
    Writer.write_line (writer t) (Command.to_string cmd);
    Writer.flushed (writer t))

let send_receive ?timeout t cmd =
  send t cmd >>=? fun () -> receive ?timeout t

let do_quit t =
  if Writer.is_closed (writer t) then return (Ok ())
  else begin
    send_receive t Command.Quit
    >>= function
    | Error e ->
      return (Error (Error.tag e "Error sending QUIT"))
    | Ok result ->
      match result with
      | `Bsmtp -> return (Ok ())
      | `Received Reply.Closing_connection_221 -> return (Ok ())
      | `Received reply ->
        return (Or_error.error_string (sprintf !"Bad reply to QUIT: %{Reply}" reply))
  end

let cleanup t =
  Writer.close (writer t)
  >>= fun () ->
  Option.value_map (reader t) ~f:Reader.close ~default:Deferred.unit
  >>= fun () ->
  match t.mode with
  | `Bsmtp _ | `Plain _ -> return (Ok ())
  | `Tls tls ->
    Ssl.Connection.close (Tls.tls tls);
    Ssl.Connection.closed (Tls.tls tls)

let quit_and_cleanup t =
  do_quit t
  >>= fun quit_result ->
  cleanup t
  >>= fun cleanup_result ->
  return (Or_error.combine_errors_unit [quit_result; cleanup_result])

let do_greeting t =
  receive t
  >>=? function
  | `Bsmtp -> return (Ok ())
  | `Received (Service_ready_220 greeting) ->
    return (Peer_info.set_greeting (info_exn t) greeting)
  | `Received reply ->
    return (Or_error.errorf !"Unexpected greeting: %{Reply}" reply)

let greeting t =
  let config = config t in
  Option.value config.greeting ~default:(Unix.gethostname ())

let do_helo t =
  send_receive t (Command.Hello (greeting t))
  >>=? function
  | `Bsmtp -> return (Ok ())
  | `Received (Reply.Ok_completed_250 helo) ->
    return (Peer_info.set_hello (info_exn t) (`Simple helo))
  | `Received reply ->
    return (Or_error.errorf !"Unexpected response to HELO: %{Reply}" reply)

let do_ehlo t =
  send_receive t (Command.Extended_hello (greeting t))
  >>=? function
  | `Bsmtp -> return (Ok ())
  | `Received reply ->
    match reply with
    | Reply.Ok_completed_250 ehlo_greeting ->
      begin match String.split_lines ehlo_greeting with
      | ehlo_greeting :: extensions ->
        let extensions = List.map ~f:Extension.of_string extensions in
        Peer_info.set_hello (info_exn t) (`Extended (ehlo_greeting, extensions))
        |> return
      | [] -> failwith "IMPOSSIBLE: EHLO greeting expected, got empty response"
      end
    | Reply.Command_not_recognized_500 _
    | Reply.Command_not_implemented_502 _ ->
      do_helo t
    | reply ->
      return (Or_error.errorf !"Unexpected response to EHLO: %{Reply}" reply)

let do_start_tls t tls_options =
  match t.mode with
  | `Bsmtp _ ->
    failwithf
      "[%s] do_start_tls: Cannot switch from bsmtp to TLS"
      t.session_id ()
  | `Tls _ ->
    failwithf
      "[%s] do_start_tls: TLS is already negotiated"
      t.session_id ()
  | `Plain plain ->
    if has_log t then Log.debug t "Starting TLS negotiation";
    let old_reader = Plain.reader plain in
    let old_writer = Plain.writer plain in
    let reader_pipe_r,reader_pipe_w = Pipe.create () in
    let writer_pipe_r,writer_pipe_w = Pipe.create () in
    Ssl.client
      ?version:tls_options.Config.Tls.version
      ?name:tls_options.Config.Tls.name
      ?ca_file:tls_options.Config.Tls.ca_file
      ?ca_path:tls_options.Config.Tls.ca_path
      (* Closing ssl connection will close the pipes which will in turn close
         the readers. *)
      ~net_to_ssl:(Reader.pipe old_reader)
      ~ssl_to_net:(Writer.pipe old_writer)
      ~ssl_to_app:reader_pipe_w
      ~app_to_ssl:writer_pipe_r
      ()
    >>=? fun tls ->
    Reader.of_pipe (Info.of_string "SMTP/TLS") reader_pipe_r
    >>= fun new_reader ->
    Writer.of_pipe (Info.of_string "SMTP/TLS") writer_pipe_w
    >>= fun (new_writer, _) ->
    if has_log t then Log.debug t "Finished TLS negotiation";
    (* Make sure we forget all of the peer info except the host and port we talk
       to. *)
    let dest = Peer_info.dest (Plain.info plain) in
    let info = Peer_info.create ~dest () in
    t.mode <- `Tls (Tls.create ~reader:new_reader ~writer:new_writer ~tls ~info);
    do_ehlo t

(* The correctness of our security relies on the correctness of this
   function. The rest of the code in this module does not need to be trusted.
*)
let check_tls_security t =
  let config = config t in
  match t.mode with
  | `Bsmtp _ ->
    if not (Config.has_tls config) then Ok ()
    else Or_error.errorf "No TLS allowed in Bsmtp mode."
  | `Plain plain ->
    let domain = Peer_info.domain (Plain.info plain) in
    begin match Config.match_tls_domain config domain with
    | None -> Ok ()
    | Some tls ->
      match Config.Tls.mode tls with
      | `Always_try | `If_available -> Ok ()
      | `Required ->
        Or_error.errorf "TLS Required for %s but not negotiated" domain
    end
  | `Tls tls ->
    let domain = Peer_info.domain (Tls.info tls) in
    match Config.match_tls_domain config domain with
    | None ->
      Or_error.errorf "TLS forbidden for %s but still negotiated" domain
    | Some tls_config ->
      let certificate_mode = Config.Tls.certificate_mode tls_config in
      let certificate = Ssl.Connection.peer_certificate (Tls.tls tls) in
      let check_domain cert =
        Ssl.Certificate.subject cert
        |> List.find ~f:(fun (sn, _) -> sn = "CN")
        |> function
        | None -> Or_error.errorf "No CN in certificate for %s" domain
        | Some (_, cn) ->
          if cn = domain then Ok ()
          else Or_error.errorf "Certificate for '%s' has CN = '%s'" domain cn
      in
      let no_cert_error () =
        Or_error.errorf "Certificate required, but not sent by peer: %s" domain
      in
      match certificate_mode, certificate with
      | `Ignore, _                -> Ok ()
      | `Verify, None             -> no_cert_error ()
      | `Verify, (Some (Error e)) -> Error e
      | `Verify, (Some (Ok cert)) -> check_domain cert

let should_try_tls t : Config.Tls.t option =
  match t.mode with
  | `Bsmtp _ | `Tls _ -> None
  | `Plain plain ->
    let domain = Peer_info.domain (Plain.info plain) in
    match Config.match_tls_domain (config t) domain with
    | None -> None
    | Some tls ->
      match Config.Tls.mode tls with
      | `Always_try | `Required -> Some tls
      | `If_available ->
        if supports_extension t Extension.Start_tls then Some tls else None

(* Will fail if negotiated security level is lower than that required by the
   config. *)
let maybe_start_tls t =
  begin match should_try_tls t with
  | None -> return (Ok ())
  | Some tls_options ->
    send_receive t Command.Start_tls
    >>=? function
    | `Bsmtp -> return (Ok ())
    | `Received reply ->
      match reply with
      | Reply.Service_ready_220 _ ->
        do_start_tls t tls_options
      | Reply.Command_not_recognized_500 _
      | Reply.Command_not_implemented_502 _
      | Reply.Parameter_not_implemented_504 _ ->
        return (Ok ())
      | reply ->
        return (Or_error.errorf !"Unexpected respose to STARTTLS: %{Reply}" reply)
  end
  >>=? fun () ->
  return (check_tls_security t)

let with_quit t ~f =
  let quit_and_cleanup_with_log t =
    quit_and_cleanup t
    >>| Result.iter_error ~f:(fun err ->
      Log.error t "%s" (Error.to_string_hum err))
  in
  Monitor.protect f ~finally:(fun () -> quit_and_cleanup_with_log t)

let with_session t ~f =
  (* The RFC prescribes that we send QUIT if we are not happy with the reached
     level of TLS security. *)
  with_quit t ~f:(fun () ->
    do_greeting t
    >>=? fun () ->
    do_ehlo t
    >>=? fun () ->
    maybe_start_tls t
    >>=? fun () ->
    f t)
