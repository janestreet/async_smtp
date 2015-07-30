open Core.Std
open Async.Std
open Types
open Email_message.Std


module Envelope_status = Client.Envelope_status

include (Email.Simple : module type of Email.Simple with module Expert := Email.Simple.Expert)

let default_server = Host_and_port.create ~host:"qsmtp" ~port:25

module Expert = struct
  include Email.Simple.Expert

  let send' ?(server=default_server) ~sender ~recipients email =
    let message = Envelope.create ~sender ~recipients ~rejected_recipients:[] ~email () in
    Client.Tcp.with_ server ~f:(fun client ->
        Client.send_envelope client message)

  let send ?server ~sender ~recipients email =
    send' ?server ~sender ~recipients email
    >>|? Envelope_status.ok_or_error ~allow_rejected_recipients:false
    >>| Or_error.join
    >>| Or_error.ignore
end

let send' ?server ?(from=Email_address.local_address ()) ~to_ ?(cc=[]) ?(bcc=[]) ~subject ?extra_headers ?attachments content =
  let email = create ~from ~to_ ~cc ~subject ?extra_headers ?attachments content in
  let recipients = to_ @ cc @ bcc in
  let sender = `Email from in
  Expert.send' ?server ~sender ~recipients email

let send ?server ?from ~to_ ?cc ?bcc ~subject ?extra_headers ?attachments content =
  send' ?server ?from ~to_ ?cc ?bcc ~subject ?extra_headers ?attachments content
    >>|? Envelope_status.ok_or_error ~allow_rejected_recipients:false
    >>| Or_error.join
    >>| Or_error.ignore
