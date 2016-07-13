open Core.Std
open Async.Std
open Types

module type S = sig

  (** [session_connect] is called when a client first connects, before any messages are
      accepted.

      [`Accept greeting] initiates a session state and sends the given greeting.

      [`Disconnect maybe_reply] terminates the connection sending the given
      reply. disconnect is NOT called.  *)
  val session_connect
    :  log:Mail_log.t
    -> session:Session.t
    -> [ `Accept of string
       | `Disconnect of (Reply.t option)
       ] Deferred.t

  (** [session_helo] is called in response to initial handshakes (i.e. HELO or EHLO).

      [`Continue advertise_auth] will update the session state and optionally advertise
      "AUTH LOGIN" as an extension

      [`Deny reply] will send the given reply but leave the session open.

      [`Disconnect maybe_reply] will send the given reply and close the session
      (disconnect is not called) *)
  val session_helo
    :  log:Mail_log.t
    -> session:Session.t
    -> string
    -> [ `Continue of [`Advertise_auth of bool]
       | `Deny of Reply.t
       | `Disconnect of Reply.t option
       ] Deferred.t

  (** [process_sender] is called in the event of a "MAIL FROM" SMTP command.

      [`Continue] creates an envelope that is filled in by subsequent commands.

      [`Reject reply] will abort this message.  *)
  val process_sender
    :  log:Mail_log.t
    -> session:Session.t
    -> Sender.t
    -> sender_args:Argument.t list
    -> [ `Continue
       | `Reject of Reply.t
       ] Deferred.t

  (** [process_recipient] is called in the event of a "RCPT TO" SMTP command.
      [`Continue] augments the envelope in the pipeline and passes it on to the next phase.
      [`Reject reply] pass through an unmodified envelope to the next phase, omitting
      this address from the recipients list.
  *)
  val process_recipient
    :  log:Mail_log.t
    -> session:Session.t
    -> sender:Sender.t
    -> sender_args:Argument.t list
    -> Email_address.t
    -> [ `Continue
       | `Reject of Reply.t
       ] Deferred.t

  (** Called after the message body has been received:

      [`Consume ok_msg] will discard the message and return a successful response to the
      client. It should be used if this plugin has processed the message and does not
      intend to relay the message (or a response).

      [`Reject reply] will discard the message and return an error reply to the client. It
      should be used when the plugin does not wish to process this message.

      [`Send envelopes_to_relay] will spool the given envelopes for further sending and
      give a successful response, once these messages have been safely spooled.

      [`Quarantine (envelopes_to_quarantine, reply, reason)] will save the given messages
      to a directory for manual inspection and pass through the given reply to the
      client. [reason] is used only internally to tell us what check failed. *)
  val process_envelope
    :  log:Mail_log.t
    -> session:Session.t
    -> Envelope.t
    -> [ `Consume of string
       | `Reject of Reply.t
       | `Send of Envelope_with_next_hop.t list
       | `Quarantine of Envelope_with_next_hop.t list * Reply.t * string
       ] Deferred.t

  (** [check_authentication_credentials] is called to check if the credentials that the
      client has provided are valid.

      [`Allow] means that the client's credentials are valid.

      [`Deny] means that the client's credentials are not valid. *)
  val check_authentication_credentials
    :  log:Mail_log.t
    -> username:string
    -> password:string
    -> [ `Allow
       | `Deny
       ] Deferred.t

  val rpcs : unit Rpc.Implementation.t list
end

(** [Simple] provides a basic plugin implementation of [S] to be used as the foundation
    for a plugin that overrides only specific callbacks. *)
module Simple : S = struct
  let session_connect ~log:_ ~session:_ =
    return (`Accept
              (sprintf "%s ocaml/mailcore 0.2 %s"
                 (Unix.gethostname ())
                 (Time.now () |> Time.to_string_abs ~zone:Time.Zone.utc)))
  ;;

  let session_helo ~log:_ ~session:_ _helo = return (`Continue (`Advertise_auth false))
  let process_sender ~log:_ ~session:_ _sender  ~sender_args:_                = return `Continue
  let process_recipient ~log:_ ~session:_ ~sender:_ ~sender_args:_ _recipient = return `Continue

  let process_envelope ~log:_ ~session:_ envelope =
    return (`Send
              [ Envelope_with_next_hop.create
                  ~envelope
                  ~next_hop_choices:[]
                  ~retry_intervals:[]
              ])
  ;;

  let check_authentication_credentials ~log:_ ~username:_ ~password:_ = return `Deny

  let rpcs = []
end
