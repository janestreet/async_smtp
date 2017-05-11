open Core
open Core_extended.Std
open Async
open Async_ssl.Std

module Log = Mail_log

module Config = Client_config

module Peer_info = struct
  type t =
    { dest : Address.t
    ; greeting : string Set_once.Stable.V1.t
    ; hello : [ `Simple of string
              | `Extended of string * (Smtp_extension.t list) ] Set_once.Stable.V1.t
    } [@@deriving sexp, fields]

  let create ~dest () =
    { dest
    ; greeting = Set_once.create ()
    ; hello = Set_once.create ()
    }

  let set field t value =
    match Set_once.set (Field.get field t) [%here] value with
    | Ok () -> Ok ()
    | Error error ->
      Error (Error.tag error ~tag:(Field.name field))

  let set_greeting = set Fields.greeting
  let set_hello = set Fields.hello

  let extensions t =
    match Set_once.get t.hello with
    | None | Some (`Simple _) -> None
    | Some (`Extended (_,extensions)) -> Some extensions

  let supports_extension t extension =
    match extensions t with
    | None -> false
    | Some extensions -> List.mem extensions extension ~equal:Smtp_extension.equal

  let greeting t =
    Set_once.get t.greeting

  let hello t =
    Set_once.get t.hello
end

module Bsmtp = struct
  type t = { writer : Writer.t } [@@deriving fields]

  let create ~writer =
    { writer }
end

module Plain = struct
  type t =
    { reader : Reader.t
    ; writer : Writer.t
    ; info : Peer_info.t
    } [@@deriving fields]

  let create ~reader ~writer ~info =
    { reader; writer; info }
end

module Tls = struct
  type t =
    { reader : Reader.t
    ; writer : Writer.t
    ; info : Peer_info.t
    ; tls : Ssl.Connection.t
    } [@@deriving fields]

  let create ~reader ~writer ~info ~tls =
    { reader; writer; info; tls }
end

(* The reason we don't keep info at the top is that switching from plain to tls requires
   us to forget the info, so we are making it less likely that we forget to forget. *)
type t =
  { config : Config.t
  ; flows : Log.Flows.t
  (* The only allowed transition is from Plain to Tls. *)
  ; mutable mode : [ `Bsmtp of Bsmtp.t
                   | `Plain of Plain.t
                   | `Tls of Tls.t
                   ]
  } [@@deriving fields]

let remote_address t =
  match t.mode with
  | `Bsmtp _ -> None
  | `Plain { Plain.info; _ }
  | `Tls { Tls.info; _ } -> Some info.Peer_info.dest
;;

let create
      ?flows
      ~dest
      reader
      writer
      config =
  let info = Peer_info.create ~dest () in
  let flows = match flows with
    | Some flows -> flows
    | None -> Log.Flows.create `Client_session
  in
  let mode = `Plain (Plain.create ~reader ~writer ~info) in
  { mode; flows; config }

let create_bsmtp
      ?flows
      writer
      config =
  let flows = match flows with
    | Some flows -> flows
    | None -> Log.Flows.create `Client_session
  in
  let mode = `Bsmtp (Bsmtp.create ~writer) in
  { mode; flows; config }

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

let is_using_auth_login t =
  supports_extension t Smtp_extension.Auth_login

let read_reply ?on_eof reader =
  let rec loop partial =
    Reader.read_line reader
    >>= function
    | `Ok line -> begin match Smtp_reply.parse ?partial line with
      | `Done reply -> Deferred.Or_error.return reply
      | `Partial partial -> loop (Some partial)
    end
    | `Eof ->  begin match on_eof with
      | Some on_eof -> on_eof ?partial ()
      | None -> Deferred.Or_error.error_string "Unexpected EOF"
    end
  in
  Deferred.Or_error.try_with_join
    (fun () -> loop None)

(* entry point *)
let receive ?on_eof ?timeout ?flows t ~log ~component ~here =
  let flows = match flows with
    | None -> t.flows
    | Some flows -> Log.Flows.union t.flows flows
  in
  let component = component @ [ "receive" ] in
  let timeout =
    Option.value timeout ~default:(Config.send_receive_timeout t.config)
  in
  match reader t with
  | None -> Deferred.Or_error.return `Bsmtp
  | Some reader ->
    Clock.with_timeout timeout (read_reply ?on_eof reader)
    >>| function
    | `Result (Ok v) ->
      Log.debug log (lazy (Log.Message.create ~here ~flows ~component ~reply:v "<-"));
      Ok (`Received v)
    | `Result (Error e) ->
      Log.error log (lazy (Log.Message.of_error ~here ~flows ~component e));
      Error e
    | `Timeout ->
      let e = Error.createf !"Timeout %{Time.Span} waiting for reply" timeout in
      Log.error log (lazy (Log.Message.of_error ~here ~flows ~component e));
      Error e


(* entry point *)
let send_gen ?command t ~log ?flows ~component ~here str =
  let flows = match flows with
    | None -> t.flows
    | Some flows -> Log.Flows.union t.flows flows
  in
  Deferred.Or_error.try_with (fun () ->
    Log.debug log (lazy (Log.Message.create
                           ~here
                           ~flows
                           ~component
                           ?command
                           "->"));
    Writer.write_line (writer t) str;
    Writer.flushed (writer t))

let send_string t ~log ?flows ~component ~here str =
  send_gen t ~log ?flows ~component ~here str

let send t ~log ?flows ~component ~here cmd =
  send_gen t ~command:cmd ~log ?flows ~component ~here (Smtp_command.to_string cmd)

(* entry point *)
let send_receive ?on_eof ?timeout t ~log ?flows ~component ~here cmd =
  send t ~log ?flows ~component ~here cmd
  >>=? fun () ->
  receive ?on_eof ?timeout t ~log ?flows ~component ~here


(* entry point *)
let send_receive_string ?on_eof ?timeout t ~log ?flows ~component ~here raw_string =
  send_string t ~log ?flows ~component ~here raw_string
  >>=? fun () ->
  receive ?on_eof ?timeout t ~log ?flows ~component ~here

let do_quit t ~log ~component =
  let component = component @ ["quit"] in
  Log.debug log (lazy (Log.Message.create
                         ~here:[%here]
                         ~flows:t.flows
                         ~component
                         ?remote_address:(remote_address t)
                         "INFO"));
  if Writer.is_closed (writer t) then return (Ok ())
  else begin
    (* Errors when we send a QUIT command are tolerable. Don't raise unnecessary noise to
       our monitor. *)
    let on_eof ?partial:_ () =
      Log.info log (lazy (Log.Message.of_error ~here:[%here] ~flows:t.flows ~component (Error.of_string "Unexpected EOF during QUIT")));
      Deferred.Or_error.return Smtp_reply.closing_connection_221
    in
    send_receive ~on_eof t ~log ~component ~here:[%here] Smtp_command.Quit
    >>= function
    | Error e ->
      let error = Error.tag e ~tag:"Error sending QUIT" in
      Log.info log (lazy (Log.Message.of_error ~here:[%here] ~flows:t.flows ~component error));
      return (Ok ())
    | Ok result ->
      match result with
      | `Bsmtp -> return (Ok ())
      | `Received { Smtp_reply.code=`Closing_connection_221; _ } ->
        return (Ok ())
      | `Received reply->
        return (Or_error.error_string (sprintf !"Bad reply to QUIT: %{Smtp_reply}" reply))
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

let quit_and_cleanup t ~log ~component =
  do_quit t ~log ~component
  >>= fun quit_result ->
  cleanup t
  >>= fun cleanup_result ->
  return (Or_error.combine_errors_unit [quit_result; cleanup_result])

let do_greeting t ~log ~component =
  let component = component @ ["greeting"] in
  Log.debug log (lazy (Log.Message.create
                         ~here:[%here]
                         ~flows:t.flows
                         ~component
                         ?remote_address:(remote_address t)
                         "INFO"));
  receive t ~log ~component ~here:[%here]
  >>=? function
  | `Bsmtp -> return (Ok ())
  | `Received { Smtp_reply.code=`Service_ready_220; raw_message } ->
    return (Peer_info.set_greeting (info_exn t) (String.concat ~sep:"\n" raw_message))
  | `Received reply ->
    return (Or_error.errorf !"Unexpected greeting: %{Smtp_reply}" reply)

let greeting t =
  let config = config t in
  Option.value config.greeting ~default:(Unix.gethostname ())

let do_helo t  ~log ~component =
  send_receive t ~log ~component ~here:[%here] (Smtp_command.Hello (greeting t))
  >>=? function
  | `Bsmtp -> return (Ok ())
  | `Received { Smtp_reply.code = `Ok_completed_250; raw_message } ->
    return (Peer_info.set_hello (info_exn t) (`Simple (String.concat ~sep:"\n" raw_message)))
  | `Received reply ->
    return (Or_error.errorf !"Unexpected response to HELO: %{Smtp_reply}" reply)

let do_ehlo ~log ~component t =
  send_receive t ~log ~component ~here:[%here] (Smtp_command.Extended_hello (greeting t))
  >>=? function
  | `Bsmtp -> return (Ok ())
  | `Received { Smtp_reply.code =`Ok_completed_250; raw_message } ->
    begin match raw_message with
    | ehlo_greeting :: extensions ->
      let extensions = List.map ~f:Smtp_extension.of_string extensions in
      Peer_info.set_hello (info_exn t) (`Extended (ehlo_greeting, extensions))
      |> return
    | [] -> failwith "IMPOSSIBLE: EHLO greeting expected, got empty response"
    end
  | `Received { Smtp_reply.code = (`Command_not_recognized_500 | `Command_not_implemented_502); _ } ->
    do_helo t ~log ~component
  | `Received reply ->
    return (Or_error.errorf !"Unexpected response to EHLO: %{Smtp_reply}" reply)

let do_start_tls t ~log ~component tls_options =
  let component = component @ ["starttls"] in
  match t.mode with
  | `Bsmtp _ ->
    failwith "do_start_tls: Cannot switch from bsmtp to TLS"
  | `Tls _ ->
    failwith "do_start_tls: TLS is already negotiated"
  | `Plain plain ->
    Log.debug log (lazy (Log.Message.create
                           ~here:[%here]
                           ~flows:t.flows
                           ~component
                           "starting tls negotiation"));
    let old_reader = Plain.reader plain in
    let old_writer = Plain.writer plain in
    let reader_pipe_r,reader_pipe_w = Pipe.create () in
    let writer_pipe_r,writer_pipe_w = Pipe.create () in
    Ssl.client
      ?version:tls_options.Config.Tls.version
      ?options:tls_options.Config.Tls.options
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
    Log.debug log (lazy (Log.Message.create
                           ~here:[%here]
                           ~flows:t.flows
                           ~component
                           "finished tls negotiation"));
    (* Make sure we forget all of the peer info except the host and port we talk
       to. *)
    let dest = Peer_info.dest (Plain.info plain) in
    let info = Peer_info.create ~dest () in
    t.mode <- `Tls (Tls.create ~reader:new_reader ~writer:new_writer ~tls ~info);
    do_ehlo t ~log ~component

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
    begin match Peer_info.dest (Plain.info plain) with
    | `Unix _file -> Ok ()
    | `Inet hp ->
      let host, port = Host_and_port.tuple hp in
      match Config.match_tls_domain config host with
      | None -> Ok ()
      | Some tls ->
        match Config.Tls.mode tls with
        | `Always_try | `If_available -> Ok ()
        | `Required ->
          Or_error.errorf "TLS Required for %s:%d but not negotiated" host port
    end
  | `Tls tls ->
    match Peer_info.dest (Tls.info tls) with
    | `Unix file ->
      Or_error.errorf "TLS forbidden for unix socket %s but still negotiated" file
    | `Inet hp ->
      let host, port = Host_and_port.tuple hp in
      match Config.match_tls_domain config host with
      | None ->
        Or_error.errorf "TLS forbidden for %s:%d but still negotiated" host port
      | Some tls_config ->
        let certificate_mode = Config.Tls.certificate_mode tls_config in
        let certificate = Ssl.Connection.peer_certificate (Tls.tls tls) in
        let check_domain cert =
          Ssl.Certificate.subject cert
          |> List.find ~f:(fun (sn, _) -> sn = "CN")
          |> function
          | None -> Or_error.errorf "No CN in certificate for %s:%d" host port
          | Some (_, cn) ->
            if cn = host then Ok ()
            else Or_error.errorf "Certificate for '%s:%d' has CN = '%s'" host port cn
        in
        let no_cert_error () =
          Or_error.errorf "Certificate required, but not sent by peer: %s:%d" host port
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
    match Peer_info.dest (Plain.info plain) with
    | `Unix _file -> None
    | `Inet hp ->
      match Config.match_tls_domain (config t) (Host_and_port.host hp) with
      | None -> None
      | Some tls ->
        match Config.Tls.mode tls with
        | `Always_try | `Required -> Some tls
        | `If_available ->
          if supports_extension t Smtp_extension.Start_tls then Some tls else None

(* Will fail if negotiated security level is lower than that required by the
   config. *)
let maybe_start_tls t ~log ~component =
  begin match should_try_tls t with
  | None -> return (Ok ())
  | Some tls_options ->
    send_receive t ~log ~component ~here:[%here] Smtp_command.Start_tls
    >>=? function
    | `Bsmtp -> return (Ok ())
    | `Received { Smtp_reply.code = `Service_ready_220; _ } ->
      do_start_tls t ~log  ~component tls_options
    | `Received { Smtp_reply.code = ( `Command_not_recognized_500
                                    | `Command_not_implemented_502
                                    | `Parameter_not_implemented_504
                                    | `Tls_temporarily_unavailable_454
                                    ); _ } ->
      return (Ok ())
    | `Received reply ->
      return (Or_error.errorf !"Unexpected response to STARTTLS: %{Smtp_reply}" reply)
  end
  >>=? fun () ->
  return (check_tls_security t)

let do_auth_login t ~log ~component ~username ~password =
  let username = Base64.encode username in
  let password = Base64.encode password in
  send_receive_string t ~log ~component ~here:[%here] username
  >>=? function
  | `Bsmtp | `Received { Smtp_reply.code=`Start_authentication_input_334; _ } -> begin
      send_receive_string t ~log ~component ~here:[%here] password
      >>=? function
      | `Bsmtp | `Received { Smtp_reply.code=`Authentication_successful_235; _ } -> return (Ok ())
      | `Received reply -> return (Or_error.errorf !"Unable to authenticate: %{Smtp_reply}" reply)
    end
  | `Received reply -> return (Or_error.errorf !"Unable to authenticate: %{Smtp_reply}" reply)

let maybe_auth_login t ~log ~component ~credentials =
  match is_using_auth_login t with
  | false -> return (Ok ())
  | true ->
    match credentials with
    | None ->
      return (Ok ())
    | Some credentials ->
      send_receive t ~log ~component ~here:[%here] (Smtp_command.Auth_login None)
      >>=? function
      | `Bsmtp -> return (Ok ())
      | `Received reply ->
        match reply with
        | { Smtp_reply.code=`Start_authentication_input_334; _ } ->
          let username = Credentials.username credentials in
          let password = Credentials.password credentials in
          do_auth_login t ~log ~component ~username ~password
        | { Smtp_reply.code=
              ( `Command_not_recognized_500
              | `Command_not_implemented_502
              | `Parameter_not_implemented_504 ); _ } ->
          return (Ok ())
        | reply ->
          return (Or_error.errorf !"Unexpected response to AUTH LOGIN: %{Smtp_reply}" reply)

let with_quit t ~log ~component ~f =
  let component = component @ ["quit"] in
  let quit_and_cleanup_with_log t =
    quit_and_cleanup t ~log ~component
    >>| function
    | Ok () -> ()
    | Error err ->
      Log.error log (lazy (Log.Message.of_error ~flows:t.flows ~here:[%here] ~component err))
  in
  Monitor.protect f ~finally:(fun () -> quit_and_cleanup_with_log t)

(* Entry point *)
let with_session t ~log ~component ~credentials ~f =
  let component = component @ [ "session" ] in
  Log.debug log (lazy (Log.Message.info ~component ~here:[%here]  ~flows:t.flows
                         ?remote_address:(remote_address t) ()));
  (* The RFC prescribes that we send QUIT if we are not happy with the reached
     level of TLS security. *)
  with_quit t ~log ~component ~f:(fun () ->
    do_greeting t ~log ~component
    >>=? fun () ->
    do_ehlo t ~log ~component:(component @ ["helo"])
    >>=? fun () ->
    maybe_start_tls t ~log ~component:(component @ ["starttls"])
    >>=? fun () ->
    maybe_auth_login t ~log ~component:(component @ ["auth_login"]) ~credentials
    >>=? fun () ->
    f t)
