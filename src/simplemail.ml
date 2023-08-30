open Core
open Async
open Async_smtp_types
module Envelope_status = Client.Envelope_status

include (
  Email.Simple : module type of Email.Simple with module Expert := Email.Simple.Expert)

let system_default_server = Host_and_port.create ~host:"localhost" ~port:25
let default_server = ref system_default_server

module For_testing = struct
  let system_default_server = system_default_server
  let set_default_server server = default_server := server
end

let client_log =
  Lazy.map Async.Log.Global.log ~f:(fun log ->
    Mail_log.adjust_log_levels ~remap_info_to:`Debug log)
;;

module Expert = struct
  include Email.Simple.Expert

  let send_envelope ?(log = Lazy.force client_log) ?credentials ?server envelope =
    let server = Option.value server ~default:!default_server in
    Client.Tcp.with_ ?credentials server ~log ~f:(fun client ->
      Client.send_envelope client ~log envelope)
  ;;

  let send' ?log ?credentials ?server ~sender ?sender_args ~recipients email =
    let message =
      Smtp_envelope.create
        ~sender
        ?sender_args
        ~recipients
        ~rejected_recipients:[]
        ~email
        ()
    in
    send_envelope ?log ?credentials ?server message
  ;;

  let send ?log ?credentials ?server ~sender ?sender_args ~recipients email =
    send' ?log ?credentials ?server ~sender ?sender_args ~recipients email
    >>|? Envelope_status.ok_or_error ~allow_rejected_recipients:false
    >>| Or_error.join
    >>| Or_error.ignore_m
  ;;
end

let send'
  ?log
  ?credentials
  ?server
  ?(from = Email.Simple.local_address ())
  ?sender_args
  ~to_
  ?(cc = [])
  ?(bcc = [])
  ?reply_to
  ?bounce_to
  ~subject
  ?id
  ?in_reply_to
  ?date
  ?auto_generated
  ?extra_headers
  ?attachments
  ?no_tracing_headers
  content
  =
  let email =
    create
      ~from
      ~to_
      ~cc
      ?reply_to
      ~subject
      ?id
      ?in_reply_to
      ?date
      ?auto_generated
      ?extra_headers
      ?attachments
      ?no_tracing_headers
      content
  in
  let recipients = to_ @ cc @ bcc in
  let sender = `Email (Option.value ~default:from bounce_to) in
  Expert.send' ?log ?credentials ?server ~sender ?sender_args ~recipients email
;;

let send
  ?log
  ?credentials
  ?server
  ?from
  ?sender_args
  ~to_
  ?cc
  ?bcc
  ?reply_to
  ?bounce_to
  ~subject
  ?id
  ?in_reply_to
  ?date
  ?auto_generated
  ?extra_headers
  ?attachments
  ?no_tracing_headers
  content
  =
  send'
    ?log
    ?credentials
    ?server
    ?from
    ?sender_args
    ~to_
    ?cc
    ?bcc
    ?reply_to
    ?bounce_to
    ~subject
    ?id
    ?in_reply_to
    ?date
    ?auto_generated
    ?extra_headers
    ?attachments
    ?no_tracing_headers
    content
  >>|? Envelope_status.ok_or_error ~allow_rejected_recipients:false
  >>| Or_error.join
  >>| Or_error.ignore_m
;;
