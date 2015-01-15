open Core.Std
open Async.Std
open Types
;;

module type S = sig

  (* Called when a client first connects, before any messages are accepted.
       Accept initiates a session state and sends the given greeting.
       Disconnect terminates the connection sending the given reply.
       disconnect is NOT called.
  *)
  val session_connect :
    session:Session.t ->
    [ `Accept of string
    | `Disconnect of (Reply.t option)
    ] Deferred.t
  ;;

  (* Called in response to initial handshakes such as HELO.
       Accept will update the session state.
       Deny will send the given reply but leave the session open.
       Disconnect with send the given reply and close the session (disconnect is not called)
  *)
  val session_helo :
    session:Session.t ->
    string ->
    [ `Continue
    | `Deny of Reply.t
    | `Disconnect of Reply.t option
    ] Deferred.t
  ;;

  (* Called in response to a MAIL FROM: ...
       Continue creates a place holder envelope that is completed by subsequent calls.
       Reject will abort this message.
  *)
  val process_sender :
    session:Session.t ->
    Sender.t ->
    [ `Continue
    | `Reject of Reply.t
    ] Deferred.t
  ;;

  (* Called in respose to a RCPT TO: ...
       Continue creates an amended envelope that is further completed by subsequent calls.
       Reject will ommit this recipient, but continue with the unmodified envelope.
       Continue
  *)
  val process_recipient :
    session:Session.t ->
    sender:Sender.t ->
    Email_address.t ->
    [ `Continue
    | `Reject of Reply.t
    ] Deferred.t
  ;;

  (* Called after the message body has been received.
       Consume will discard the message and give a successfull response. It should be used
         if this plugin has processed the message and does not intent to relay the message
         (or a response).
       Reject will discard the message and give an error response. It should be used when
         this plugin does not wish to process this message.
       Send will spool the given messages for further sending and give a successfull
         response, once these messages have been safely stored.
  *)
  val process_envelope :
    session:Session.t ->
    Envelope.t ->
    [ `Consume of string
    | `Reject of Reply.t
    | `Send of Envelope_with_next_hop.t list
    ] Deferred.t
  ;;

  val rpcs : unit Rpc.Implementation.t list
end

module Simple : S = struct
  let session_connect ~session:_ =
    return (`Accept
              (sprintf "%s JSMTP 0.1 %s"
                 (Unix.gethostname ())
                 (Time.now () |> Time.to_string_abs ~zone:Time.Zone.utc)))

  let session_helo ~session:_ _helo =
    return `Continue

  let process_sender ~session:_ _sender =
    return `Continue

  let process_recipient ~session:_ ~sender:_ _recipient =
    return `Continue

  let process_envelope ~session:_ envelope =
    return (`Send
              [ Envelope_with_next_hop.create
                  ~envelope
                  ~next_hop_choices:[]
                  ~retry_intervals:[]
              ])

  let rpcs = []
end
