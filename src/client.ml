open Core.Std
open Async.Std
open Email_message.Std
open Types

module Config = Client_config

include Client_raw

let with_reset t ~f =
  let reset t =
    send_receive t Command.Reset
    >>=? function
    | `Bsmtp -> return (Ok ())
    | `Received (Reply.Ok_completed_250 _) -> return (Ok ())
    | `Received reject ->
      return (Or_error.errorf !"Unexpected response to RESET: %{Reply}" reject)
  in
  Deferred.Or_error.try_with_join (fun () -> f t)
  >>= function
  | Error err ->
    reset t
    >>| fun (_ : unit Or_error.t) ->
    Error err
  | Ok ok -> Deferred.Or_error.return ok

module Envelope_status = struct
  type envelope_id = string [@@deriving sexp]
  type rejected_recipients = (Email_address.t * Reply.t) list [@@deriving sexp]
  type ok = envelope_id * rejected_recipients [@@deriving sexp]
  type err =
    [ `Rejected_sender of Reply.t
    | `No_recipients of rejected_recipients
    | `Rejected_sender_and_recipients of Reply.t * rejected_recipients
    | `Rejected_body of Reply.t * rejected_recipients
    ] [@@deriving sexp]
  type t = (ok, err) Result.t [@@deriving sexp]

  let rejected_recipients_to_string ~ok ~err rejected_recipients =
    if not (List.is_empty rejected_recipients) then
      rejected_recipients
      |> List.map ~f:(fun (email, reject) ->
        sprintf !"%{Email_address} (%{Reply})" email reject)
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
      sprintf !"Rejected sender (%{Reply})" reject
    | Error (`No_recipients rejected_recipients) ->
      rejected_recipients_to_string
        ~ok:"No Recipients"
        ~err:"All recipients rejected: "
        rejected_recipients
    | Error (`Rejected_sender_and_recipients (reject,rejected_recipients)) ->
      sprintf !"Rejected combination of Sender and Recipients (%{Reply})%s"
        reject
        (rejected_recipients_to_string
           ~ok:""
           ~err:" and rejected recipients: "
           rejected_recipients)
    | Error (`Rejected_body (reject,rejected_recipients)) ->
      sprintf !"Rejected envelope (%{Reply})%s"
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

let send_envelope t envelope : Envelope_status.t Deferred.Or_error.t =
  with_reset t ~f:(fun t ->
    send_receive t
      (Command.Sender (Envelope.sender envelope |> Sender.to_string))
    >>=? begin function
    | `Bsmtp -> return (Ok (Ok ()))
    | `Received (Reply.Ok_completed_250 _) -> return (Ok (Ok ()))
    | `Received reply -> return (Ok (Error (`Rejected_sender reply)))
    end
    >>=?? fun () ->
    begin
      Deferred.Or_error.List.filter_map ~how:`Sequential
        (Envelope.recipients envelope)
        ~f:(fun recipient ->
          send_receive t
            (Command.Recipient (recipient |> Email_address.to_string))
          >>|? function
          | `Bsmtp -> None
          | `Received (Reply.Ok_completed_250 _) -> None
          | `Received reply -> Some (recipient, reply))
      >>=? fun rejected_recipients ->
      if List.length (rejected_recipients) = List.length (Envelope.recipients envelope)
      then return (Ok (Error (`No_recipients rejected_recipients)))
      else return (Ok (Ok rejected_recipients))
    end
    >>=?? fun rejected_recipients ->
    send_receive t Command.Data
    >>=? begin function
    | `Bsmtp -> return (Ok (Ok ()))
    | `Received (Reply.Start_mail_input_354) -> return (Ok (Ok ()))
    | `Received reply ->
      return (Ok (Error (`Rejected_sender_and_recipients (reply,
                                                          rejected_recipients))))
    end
    >>=?? fun () ->
    begin
      Deferred.Or_error.try_with_join (fun () ->
          if has_log t
          then Log.debug t "## STARTING TRANSMITTING MESSAGE BODY";
          let writer = writer t in
          let block_length = ref 0 in
          (* We will send at most [max_block_length + <max line length> + 1]
             bytes per block. *)
          let max_block_length = 16 * 1024 in
          let timeout = Config.send_receive_timeout (config t) in
          let flush () =
            Clock.with_timeout timeout (Writer.flushed writer)
            >>| function
            | `Timeout ->
              Or_error.error_string
                (sprintf !"Timeout %{Time.Span} waiting for data to flush" timeout)
            | `Result () -> Ok ()
          in
          Envelope.email envelope
          |> Email.to_string
          |> String.split ~on:'\n'
          |> Deferred.Or_error.List.iter ~how:`Sequential ~f:(fun line ->
              begin
                if !block_length >= max_block_length
                then begin
                  block_length := 0;
                  flush ()
                end else Deferred.Or_error.ok_unit
              end
              >>|? fun () ->
              (* dot escaping... *)
              if String.is_prefix ~prefix:"." line then begin
                block_length := !block_length + 1;
                Writer.write writer "."
              end;
              block_length := !block_length + String.length line;
              Writer.write_line writer line
            )
          >>=? fun () ->
          Writer.write_line writer ".";
          flush ()
          >>=? fun () ->
          if has_log t
          then Log.debug t "## FINISHED TRANSMITTING MESSAGE BODY";
          receive ~timeout:(Config.final_ok_timeout (config t)) t
          >>|? function
          | `Bsmtp ->
            Ok ("written", rejected_recipients)
          | `Received (Reply.Ok_completed_250 msg) ->
            Ok (msg, rejected_recipients)
          | `Received reply ->
            Error (`Rejected_body (reply, rejected_recipients)))
    end)

module Tcp = struct
  let with_ ?buffer_age_limit ?interrupt ?reader_buffer_size ?timeout
      ?log
      ?session_id
      ?(config = Config.default)
      dest
      ~f =
    let session_id = match session_id with
      | Some session_id -> session_id
      | None -> sprintf !"dst=%{Host_and_port}|%{Uuid}" dest (Uuid.create ())
    in
    let address =
      Tcp.to_host_and_port
        (Host_and_port.host dest)
        (Host_and_port.port dest)
    in
    let has_log = Option.is_some log in
    let debug fmt =
      ksprintf
        (fun msg -> Option.iter log ~f:(fun log -> Async.Std.Log.debug log "%s" msg))
        fmt
    in
    if has_log then debug "[%s] ## Attempting to connect" session_id;
    Deferred.Or_error.try_with_join (fun () ->
        Tcp.with_connection
          ?buffer_age_limit
          ?interrupt
          ?reader_buffer_size
          ?timeout
          address
          (fun _socket reader writer ->
             if has_log then debug "[%s] ## Connection established" session_id;
             create
               ?log
               ~session_id
               ~dest
               reader
               writer
               config
             |> with_session ~f))
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

  let with_ ?(skip_prelude_and_prologue=false) writer ~f =
    create_bsmtp
      ~session_id:(Uuid.to_string (Uuid.create ()))
      writer
      config
    |> fun t ->
    if skip_prelude_and_prologue then f t
    else do_helo t >>=? fun () -> f t

  let write ?skip_prelude_and_prologue writer envelopes =
    with_ ?skip_prelude_and_prologue writer ~f:(fun client ->
      Deferred.Or_error.try_with (fun () ->
        Pipe.iter envelopes ~f:(fun envelope ->
          send_envelope client envelope
          >>| Or_error.ok_exn
          >>| Envelope_status.ok_exn ~allow_rejected_recipients:false
          >>| (ignore:string -> unit))))
end
