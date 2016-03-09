open! Core.Std
open! Async.Std
open Email_message.Std
open Types

module Envelope_status = Client.Envelope_status

module Expert : sig
  val send'
    :  ?log:Mail_log.t
    -> ?server:Address.t
    -> sender:Sender.t
    -> recipients:Email_address.t list
    -> Email.t
    -> Envelope_status.t Deferred.Or_error.t

  val send
    :  ?log:Mail_log.t
    -> ?server:Address.t
    -> sender:Sender.t
    -> recipients:Email_address.t list
    -> Email.t
    -> unit Deferred.Or_error.t

  include (module type of Email.Simple.Expert)
end

include (module type of Email.Simple with module Expert := Email.Simple.Expert)

val send'
  :  ?log:Mail_log.t
  -> ?server:Host_and_port.t
  -> ?from:Email_address.t (* defaults to <user@host> *)
  -> to_:Email_address.t list
  -> ?cc:Email_address.t list
  -> ?bcc:Email_address.t list
  -> subject:string
  -> ?id:string
  -> ?date:Time.t
  -> ?extra_headers:(Email_headers.Name.t * Email_headers.Value.t) list
  -> ?attachments:(attachment_name * Email.Simple.Content.t) list
  -> Email.Simple.Content.t
  -> Envelope_status.t Deferred.Or_error.t

val send
  :  ?log:Mail_log.t
  -> ?server:Host_and_port.t
  -> ?from:Email_address.t (* defaults to <user@host> *)
  -> to_:Email_address.t list
  -> ?cc:Email_address.t list
  -> ?bcc:Email_address.t list
  -> subject:string
  -> ?id:string
  -> ?date:Time.t
  -> ?extra_headers:(Email_headers.Name.t * Email_headers.Value.t) list
  -> ?attachments:(attachment_name * Email.Simple.Content.t) list
  -> Email.Simple.Content.t
  -> unit Deferred.Or_error.t
