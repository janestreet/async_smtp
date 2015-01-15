open Core.Std
open Async.Std
open Types

val send :
  Host_and_port.t
  -> sender:Sender.t
  -> recipients:Email_address.t list
  -> string
  -> (unit, Client.Smtp_error.t) Client.Smtp_result.t
