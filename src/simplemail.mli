open! Core
open! Async
open Async_smtp_types

module Envelope_status = Client.Envelope_status


module Expert : sig
  val send'
    :  ?log:Mail_log.t
    -> ?credentials:Credentials.t
    -> ?server:Smtp_socket_address.t
    -> sender:Smtp_envelope.Sender.t
    -> ?sender_args:Smtp_envelope.Sender_argument.t list
    -> recipients:Email_address.t list
    -> Email.t
    -> Envelope_status.t Deferred.Or_error.t

  val send
    :  ?log:Mail_log.t
    -> ?credentials:Credentials.t
    -> ?server:Smtp_socket_address.t
    -> sender:Smtp_envelope.Sender.t
    -> ?sender_args:Smtp_envelope.Sender_argument.t list
    -> recipients:Email_address.t list
    -> Email.t
    -> unit Deferred.Or_error.t

  include (module type of Email.Simple.Expert)
end

include (module type of Email.Simple with module Expert := Email.Simple.Expert)

val send'
  :  ?log:Mail_log.t
  -> ?credentials:Credentials.t
  -> ?server:Host_and_port.t
  -> ?from:Email_address.t (* defaults to <user@host> *)
  -> ?sender_args:Smtp_envelope.Sender_argument.t list
  -> to_:Email_address.t list
  -> ?cc:Email_address.t list
  -> ?bcc:Email_address.t list
  -> ?reply_to:Email_address.t list
  -> ?bounce_to:Email_address.t (* defaults to [from] *)
  -> subject:string
  -> ?id:string
  -> ?in_reply_to:string
  -> ?date:Time.t
  -> ?auto_generated:unit
  -> ?extra_headers:(Email_headers.Name.t * Email_headers.Value.t) list
  -> ?attachments:(attachment_name * Email.Simple.Content.t) list
  -> Email.Simple.Content.t
  -> Envelope_status.t Deferred.Or_error.t

val send
  :  ?log:Mail_log.t
  -> ?credentials:Credentials.t
  -> ?server:Host_and_port.t
  -> ?from:Email_address.t (* defaults to <user@host> *)
  -> ?sender_args:Smtp_envelope.Sender_argument.t list
  -> to_:Email_address.t list
  -> ?cc:Email_address.t list
  -> ?bcc:Email_address.t list
  -> ?reply_to:Email_address.t list
  -> ?bounce_to:Email_address.t (* defaults to [from] *)
  -> subject:string
  -> ?id:string
  -> ?in_reply_to:string
  -> ?date:Time.t
  -> ?auto_generated:unit
  -> ?extra_headers:(Email_headers.Name.t * Email_headers.Value.t) list
  -> ?attachments:(attachment_name * Email.Simple.Content.t) list
  -> Email.Simple.Content.t
  -> unit Deferred.Or_error.t
