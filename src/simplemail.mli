open! Core
open! Async
open Async_smtp_types
module Envelope_status = Client.Envelope_status

module Expert : sig
  val send_envelope
    :  ?log:Log.t
    -> ?credentials:Credentials.t
    -> ?server:Host_and_port.t
    -> Smtp_envelope.t
    -> Envelope_status.t Deferred.Or_error.t

  val send'
    :  ?log:Mail_log.t
    -> ?credentials:Credentials.t
    -> ?server:Host_and_port.t
    -> sender:Smtp_envelope.Sender.t
    -> ?sender_args:Smtp_envelope.Sender_argument.t list
    -> recipients:Email_address.t list
    -> Email.t
    -> Envelope_status.t Deferred.Or_error.t

  val send
    :  ?log:Mail_log.t
    -> ?credentials:Credentials.t
    -> ?server:Host_and_port.t
    -> sender:Smtp_envelope.Sender.t
    -> ?sender_args:Smtp_envelope.Sender_argument.t list
    -> recipients:Email_address.t list
    -> Email.t
    -> unit Deferred.Or_error.t

  include module type of Email.Simple.Expert
end

include module type of Email.Simple with module Expert := Email.Simple.Expert

val send'
  :  ?log:Mail_log.t
  -> ?credentials:Credentials.t
  -> ?server:Host_and_port.t
  -> ?from:Email_address.t (* defaults to <user@host> *)
  -> ?sender_args:Smtp_envelope.Sender_argument.t list
  -> to_:Email_address.t list
  -> ?cc:Email_address.t list
  -> ?bcc:Email_address.t list
  -> ?reply_to:Email_address.t
  -> ?bounce_to:Email_address.t (* defaults to [from] *)
  -> subject:string
  -> ?id:string
  -> ?in_reply_to:string
  -> ?date:Time_float.t
  -> ?auto_generated:unit
  -> ?extra_headers:(Email_headers.Name.t * Email_headers.Value.t) list
  -> ?attachments:(attachment_name * Email.Simple.Content.t) list
  -> ?no_tracing_headers:[ `Because_not_using_standard_email_infra ]
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
  -> ?reply_to:Email_address.t
  -> ?bounce_to:Email_address.t (* defaults to [from] *)
  -> subject:string
  -> ?id:string
  -> ?in_reply_to:string
  -> ?date:Time_float.t
  -> ?auto_generated:unit
  -> ?extra_headers:(Email_headers.Name.t * Email_headers.Value.t) list
  -> ?attachments:(attachment_name * Email.Simple.Content.t) list
  -> ?no_tracing_headers:[ `Because_not_using_standard_email_infra ]
  -> Email.Simple.Content.t
  -> unit Deferred.Or_error.t

module For_testing : sig
  (** Set the default server to localhost with this port.

      You should probably use lib/email_test_helpers/ instead of this. *)
  val set_default_server : Host_and_port.t -> unit

  val system_default_server : Host_and_port.t
end
