open Core.Std
open Async.Std

type email_id
type smtp_email = string * string list * email_id * Email_message.Email.t
type rewriting_rule = smtp_email -> smtp_email list option
type routing_rule = smtp_email -> (string * int) list option


module Commands : sig
  include (module type of Comm)
  val commands : string list
  val to_string : t -> string
  val of_string_opt: string -> t option
end

module Replies : sig
  type ok =
  | System_status
  | Help
  | Service_ready
  | Closing_connection
  | Ok_completed
  | Will_forward
  | Will_attempt
  | Start_mail_input

  type not_ok =
  | Not_available
  | Mailbox_unavailable_400
  | Local_error
  | Insufficient_storage
  | Unable_to_accommodate

  type never_ok =
  | Command_not_recognized
  | Syntax_error
  | Command_not_implemented
  | Bad_sequence_of_commands
  | Parameter_not_implemented
  | Mailbox_unavailable_500
  | User_not_local
  | Exceeded_storage_allocation
  | Mailbox_name_not_allowed
  | Trasaction_failed
  | From_to_parameters_bad

  type reply = Ok of ok | Bad of not_ok | Really_bad of never_ok
  type t = reply * string

  val to_string : t -> string
  val of_string : string -> t
end

module Client : sig
  val send_email :
    Reader.t
    -> Writer.t
    -> from:string
    -> to_:string list
    -> string
    -> bool Deferred.t
end

module Server : sig
  val start_connection :
    string
    -> Reader.t
    -> Writer.t
    -> smtp_email option Deferred.t
end

module Router : sig
  val rules_server :
    rewriting_rule list
    -> routing_rule list
    -> Socket.Address.Inet.t-> Reader.t -> Writer.t
    -> unit Deferred.t
end
