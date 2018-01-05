open Core
open Async
open Async_smtp_types

module Config = Client_config

include Client_raw
module Log = Mail_log

let with_reset t ~log ~flows ~component ~f =
  let component = component @ ["reset"] in
  let reset t =
    send_receive t ~log ~flows ~component ~here:[%here] Smtp_command.Reset
    >>=? function
    | `Bsmtp -> return (Ok ())
    | `Received { Smtp_reply.code = `Ok_completed_250; _ } -> return (Ok ())
    | `Received reject ->
      return (Or_error.errorf !"Unexpected response to RSET: %{Smtp_reply}" reject)
  in
  Deferred.Or_error.try_with_join (fun () -> f t)
  >>= fun result ->
  reset t
  >>| fun (_ : unit Or_error.t) ->
  result

module Envelope_status = struct
  type envelope_id = string [@@deriving sexp]
  type rejected_recipients = (Email_address.Stable.V1.t * Smtp_reply.t) list [@@deriving sexp]
  type ok = envelope_id * rejected_recipients [@@deriving sexp]
  type err =
    [ `Rejected_sender of Smtp_reply.t
    | `No_recipients of rejected_recipients
    | `Rejected_sender_and_recipients of Smtp_reply.t * rejected_recipients
    | `Rejected_body of Smtp_reply.t * rejected_recipients
    ] [@@deriving sexp]
  type t = (ok, err) Result.t [@@deriving sexp]

  let rejected_recipients_to_string ~ok ~err rejected_recipients =
    if not (List.is_empty rejected_recipients) then
      rejected_recipients
      |> List.map ~f:(fun (email, reject) ->
        sprintf !"%{Email_address} (%{Smtp_reply})" email reject)
      |> String.concat ~sep:", "
      |> sprintf "%s %s" err
    else
      ok

  let to_string = function
    | Ok (envelope_id, rejected_recipients) ->
      sprintf "Envelope accepted (%s)%s"
        envelope_id
        (rejected_recipients_to_string
           ~ok:""
           ~err:" but rejected recipients: "
           rejected_recipients)
    | Error (`Rejected_sender reject) ->
      sprintf !"Rejected sender (%{Smtp_reply})" reject
    | Error (`No_recipients rejected_recipients) ->
      rejected_recipients_to_string
        ~ok:"No Recipients"
        ~err:"All recipients rejected: "
        rejected_recipients
    | Error (`Rejected_sender_and_recipients (reject,rejected_recipients)) ->
      sprintf !"Rejected combination of Sender and Recipients (%{Smtp_reply})%s"
        reject
        (rejected_recipients_to_string
           ~ok:""
           ~err:" and rejected recipients: "
           rejected_recipients)
    | Error (`Rejected_body (reject,rejected_recipients)) ->
      sprintf !"Rejected envelope (%{Smtp_reply})%s"
        reject
        (rejected_recipients_to_string
           ~ok:""
           ~err:" and rejected recipients: "
           rejected_recipients)

  let ok_or_error ~allow_rejected_recipients = function
    | Ok (envelope_id, rejected_recipients)
      when allow_rejected_recipients || List.is_empty rejected_recipients ->
      Ok envelope_id
    | error ->
      Error (Error.of_thunk (fun () -> to_string error))

  let ok_exn ~allow_rejected_recipients = function
    | Ok (envelope_id, rejected_recipients)
      when allow_rejected_recipients || List.is_empty rejected_recipients ->
      envelope_id
    | error ->
      failwith (to_string error)
end

(* Better names welcome. *)
module Smtp_monad = struct
  type 'a t = ('a, Envelope_status.err) Result.t Or_error.t Deferred.t

  let (>>=) (a : 'a t) f =
    a >>=? function
    | Error err -> Deferred.Or_error.return (Error err)
    | Ok a -> f a
end

let (>>=??) = Smtp_monad.(>>=)

let flush_writer_with_timeout ~timeout ~writer =
  Clock.with_timeout timeout (Writer.flushed writer)
  >>| function
  | `Timeout ->
    Or_error.errorf !"Timeout %{Time.Span} waiting for data to flush" timeout
  | `Result () -> Ok ()
;;

module Expert = struct
  let send_envelope t ~log ?flows ?(component=[]) ~send_data envelope_info =
    let flows = match flows with
      | None -> Log.Flows.create `Outbound_envelope
      | Some flows -> flows
    in
    let component = component @ ["send-envelope"] in
    with_reset t ~log ~flows ~component ~f:(fun t ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component
                            ?dest:(remote_address t)
                            ~session_marker:`Sending
                            "sending"));
      send_receive t ~log ~flows ~component:(component @ ["sender"]) ~here:[%here]
        (Smtp_command.Sender
           (Smtp_envelope.Sender.to_string_with_arguments
              (Smtp_envelope.Info.sender envelope_info,
               Smtp_envelope.Info.sender_args envelope_info)))
      >>=? begin function
      | `Bsmtp -> return (Ok (Ok ()))
      | `Received { Smtp_reply.code = `Ok_completed_250; _ } -> return (Ok (Ok ()))
      | `Received reply ->
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component:(component @ ["sender"])
                              ~reply "send rejected"));
        return (Ok (Error (`Rejected_sender reply)))
      end
      >>=?? fun () ->
      begin
        Deferred.Or_error.List.map ~how:`Sequential
          (Smtp_envelope.Info.recipients envelope_info)
          ~f:(fun recipient ->
            send_receive t ~log ~flows ~component:(component @ ["recipient"]) ~here:[%here]
              (Smtp_command.Recipient (recipient |> Email_address.to_string))
            >>|? function
            | `Bsmtp -> `Fst recipient
            | `Received { Smtp_reply.code = `Ok_completed_250; _ } -> `Fst recipient
            | `Received reply ->
              Log.info log (lazy (Log.Message.create
                                    ~here:[%here]
                                    ~flows
                                    ~component:(component @ ["recipient"])
                                    ~reply "send rejected"));
              `Snd (recipient, reply))
        >>|? List.partition_map ~f:ident
        >>|? function
        | ([], rejected_recipients) ->
          Error (`No_recipients rejected_recipients)
        | (accepted_recipients, rejected_recipients) ->
          Ok (accepted_recipients, rejected_recipients)
      end
      >>=?? fun (accepted_recipients,rejected_recipients) ->
      send_receive t ~log ~flows ~component:(component @ ["data"]) ~here:[%here] Smtp_command.Data
      >>=? begin function
      | `Bsmtp -> return (Ok (Ok ()))
      | `Received { Smtp_reply.code = `Start_mail_input_354; _ } -> return (Ok (Ok ()))
      | `Received reply ->
        Log.info log (lazy (Log.Message.create
                              ~here:[%here]
                              ~flows
                              ~component:(component @ ["data"])
                              ~reply "send rejected"));
        return (Ok (Error (`Rejected_sender_and_recipients (reply,
                                                            rejected_recipients))))
      end
      >>=?? fun () ->
      Deferred.Or_error.try_with_join (fun () ->
        Log.debug log (lazy (Log.Message.create
                               ~here:[%here]
                               ~flows
                               ~component:(component @ ["data"])
                               "starting transmitting body"));
        send_data t
        >>=? fun () ->
        let writer = writer t in
        Writer.write writer "\r\n";
        Writer.write writer ".";
        Writer.write writer "\r\n";
        flush_writer_with_timeout
          ~timeout:(Config.send_receive_timeout (config t))
          ~writer
        >>=? fun () ->
        Log.debug log (lazy (Log.Message.create
                               ~here:[%here]
                               ~flows
                               ~component:(component @ ["data"])
                               "finishing transmitting body"));
        receive t ~timeout:(Config.final_ok_timeout (config t))
          ~log ~flows ~component ~here:[%here]
        >>|? function
        | `Bsmtp ->
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                ~recipients:(List.map accepted_recipients ~f:(fun e -> `Email e))
                                "delivered"));
          Ok ("bsmtp", rejected_recipients)
        | `Received { Smtp_reply.code = `Ok_completed_250; raw_message } ->
          let remote_id = String.concat ~sep:"\n" raw_message in
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                ~sender:(`Sender (Smtp_envelope.Info.sender envelope_info))
                                ~recipients:(List.map accepted_recipients ~f:(fun e -> `Email e))
                                ?dest:(remote_address t)
                                ~tags:[ "remote-id",remote_id ]
                                "sent"));
          Ok (remote_id, rejected_recipients)
        | `Received reply ->
          Log.info log (lazy (Log.Message.create
                                ~here:[%here]
                                ~flows
                                ~component
                                ~recipients:(List.map accepted_recipients ~f:(fun e -> `Email e))
                                ~reply
                                "send rejected"));
          Error (`Rejected_body (reply, rejected_recipients))))
end

let send_data_via_reader_writer t ~email =
  let block_length = ref 0 in
  (* We will send at most [max_block_length + <max line length> + 1]
     bytes per block. *)
  let max_block_length = 16 * 1024 in
  let timeout = Config.send_receive_timeout (config t) in
  let writer = writer t in
  Email.to_string email
  |> String.split ~on:'\n'
  |> (fun lines ->
    let num_lines = List.length lines in
    Deferred.Or_error.List.iteri lines ~how:`Sequential ~f:(fun i line ->
      begin
        if !block_length >= max_block_length
        then begin
          block_length := 0;
          flush_writer_with_timeout ~timeout ~writer
        end else Deferred.Or_error.ok_unit
      end
      >>|? fun () ->
      let encoded = Dot_escaping.encode_line_string line in
      String_monoid.output_unix encoded writer;
      block_length := !block_length + String_monoid.length encoded;
      if not (i = num_lines - 1)
      then Writer.write writer "\r\n"))
;;

let send_envelope t ~log ?flows ?component envelope =
  Expert.send_envelope t ~log ?flows ?component
    ~send_data:(send_data_via_reader_writer ~email:(Smtp_envelope.email envelope))
    (Smtp_envelope.info envelope)
;;

module For_test = struct
  let with_
        ?(config = Config.default)
        ?(credentials = Credentials.anon)
        ~log
        ?(flows=Log.Flows.none)
        ?(component=[])
        ?(emulate_tls=false)
        ~dest
        reader writer
        ~f =
    create ~dest ~flows ~emulate_tls_for_test:emulate_tls reader writer config
    (* Flow already attatched to the session *)
    |> with_session ~log ~component ~credentials ~f
end

module Tcp = struct
  let with_ ?buffer_age_limit ?interrupt ?reader_buffer_size ?writer_buffer_size ?timeout
        ?(config = Config.default)
        ?(credentials = Credentials.anon)
        ~log
        ?(flows=Log.Flows.none)
        ?(component=[])
        dest
        ~f =
    let flows = Log.Flows.extend flows `Client_session in
    let component = component @ ["smtp-client"] in
    let with_connection address f =
      Deferred.Or_error.try_with_join (fun () ->
        Tcp.with_connection
          ?buffer_age_limit
          ?interrupt
          ?reader_buffer_size
          ?writer_buffer_size
          ?timeout
          address
          f)
    in
    let f _socket reader writer =
      Log.debug log
        (lazy (Log.Message.create
                 ~here:[%here]
                 ~flows
                 ~component:(component @ ["tcp"])
                 ~remote_address:dest
                 "connection established"));
      create ~dest ~flows ~emulate_tls_for_test:false reader writer config
      (* Flow already attatched to the session *)
      |> with_session ~log ~component ~credentials ~f
    in
    match dest with
    | `Inet hp ->
      let address = Tcp.Where_to_connect.of_host_and_port hp in
      with_connection address f
    | `Unix file ->
      with_connection (Tcp.Where_to_connect.of_file file) f
end

(* BSMTP writing *)
module Bsmtp = struct
  let config =
    { Config.
      tls = []
    ; greeting = Some "bsmtp"
    ; send_receive_timeout = `This (Time.Span.of_sec 5.)
    ; final_ok_timeout = `This (Time.Span.of_sec 5.)
    }

  let bsmtp_log = Lazy.map Async.Log.Global.log ~f:(fun log ->
    Log.adjust_log_levels ~remap_info_to:`Debug log)


  let with_ ?(skip_prelude_and_prologue=false) writer ~log ~component ~f =
    create_bsmtp
      writer
      config
    |> fun t ->
    if skip_prelude_and_prologue then f t
    else do_helo t ~log ~component >>=? fun () -> f t

  let write ?skip_prelude_and_prologue ?(log=Lazy.force bsmtp_log) ?flows ?(component=["bsmtp";"writer"]) writer envelopes =
    with_ ?skip_prelude_and_prologue writer ~log ~component ~f:(fun client ->
      Deferred.Or_error.try_with (fun () ->
        Pipe.iter envelopes ~f:(fun envelope ->
          (* Flow already attatched to the session *)
          send_envelope client ~log ?flows ~component envelope
          >>| Or_error.ok_exn
          >>| Envelope_status.ok_exn ~allow_rejected_recipients:false
          >>| (ignore:string -> unit))))
end
