open Types
open Email_message.Std

let send destination ~sender ~recipients message =
  let email = Email.of_string message in
  let message =
    Envelope.create ~sender ~recipients ~rejected_recipients:[] ~email ()
  in
  Client.send destination message
