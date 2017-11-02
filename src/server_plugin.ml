open Core
open Async
open Email_message

module type Start_tls = sig
  type session
  (** [upgrade_to_tls] is called when initiating an upgrade to TLS.
      [Session.greeting] will be called to send a suitable greeting after the upgrade. *)
  val upgrade_to_tls
    :  log:Mail_log.t
    -> session
    -> session Deferred.t
end

module type Auth = Auth.Server

module Extension = struct
  type 'session t =
    | Start_tls of (module Start_tls with type session='session)
    | Auth of (module Auth with type session='session)
end

module type Session = sig
  type t

  (** [connect] is called when a client first connects, before any messages are
      accepted.

      [`Accept t] accepts the connection, creating a session.

      [`Disconnect maybe_reply] terminates the connection, sending the given reply. *)
  val connect
    :  log:Mail_log.t
    -> local:Address.t
    -> remote:Address.t
    -> [ `Accept of t
       | `Disconnect of Smtp_reply.t option
       ] Deferred.t

  (** [greeting] to send to clients after the connection has been accepted. *)
  val greeting : t -> string

  (** [helo] is called in response to initial handshakes (i.e. HELO or EHLO).

      [`Continue session] allows the SMTP session to continue.

      [`Deny reply] sends the given reply but leaves the session open.

      [`Disconnect maybe_reply] sends the given reply and closes the session. *)
  val helo
    :  log:Mail_log.t
    -> t
    -> string
    -> [ `Continue of t
       | `Deny of Smtp_reply.t
       | `Disconnect of Smtp_reply.t option
       ] Deferred.t

  (** [extensions] that are supported including the associated implementations.
      It is assumed that this will only change after [connect], [helo] and
      [Start_tls.upgrade_to_tls]. *)
  val extensions : t -> t Extension.t list

  (** [disconnect] is called when an SMTP connection is closed. It allows the plugin to
      cleanup any resources associated with this session *)
  val disconnect : log:Log.t -> t -> unit Deferred.t
end

module type Envelope = sig
  type session
  type t

  val smtp_envelope_info : t -> Envelope.Info.t

  (** [mail_from] is called in the event of a "MAIL FROM" SMTP command.

      [`Continue] creates an envelope that is updated by [rcpt_to] and finally processed
      by [data].

      [`Reject reply] sends the given reply. *)
  val mail_from
    :  log:Mail_log.t
    -> session
    -> Sender.t
    -> Sender_argument.t list
    -> [ `Continue of t
       | `Reject of Smtp_reply.t
       ] Deferred.t

  (** [rcpt_to] is called in the event of a "RCPT TO" SMTP command.

      [`Continue t] augments the envelope in the pipeline and passes it on to the next
      phase.

      [`Reject reply] sends the given reply. *)
  val rcpt_to
    :  log:Mail_log.t
    -> session
    -> t
    -> Email_address.t
    -> [ `Continue of t
       | `Reject of Smtp_reply.t
       ] Deferred.t

  (** [accept_data] is called when the [DATA] command is received to decide
      whether or not to accept the message data. *)
  val accept_data
    : log:Mail_log.t
    -> session
    -> t
    -> [ `Continue of t
       | `Reject of Smtp_reply.t
       ] Deferred.t

  (** [process] is called when the message body has been received
      (after the DATA command and [accept_data]).

      [`Consume ok_msg] drops the message and returns a successful response to the
      client. It should be used if this plugin has processed the message and does not
      intend to relay the message.

      [`Reject reply] drops the message and returns an error reply to the client. It
      should be used when the plugin does not wish to process this message.

      [`Send envelopes_to_relay] spools the given envelopes for further sending and gives
      a successful response, once these messages have been safely spooled.

      [`Quarantine (envelopes_to_quarantine, reply, reason)] saves the given messages to a
      directory for manual inspection and passes through the given reply to the client.
      [reason] is used only internally to tell us what check failed. *)
  val process
    : log:Mail_log.t
    -> session
    -> t
    -> Email.t
    -> [ `Consume of string
       | `Reject of Smtp_reply.t
       | `Send of Envelope.With_next_hop.t list
       | `Quarantine of Envelope.With_next_hop.t list * Smtp_reply.t * Quarantine_reason.t
       ] Deferred.t

end

module type S = sig
  module Session : Session
  module Envelope : Envelope with type session := Session.t

  val rpcs : unit -> unit Rpc.Implementation.t list
end

(** [Simple] provides a basic plugin implementation of [S] to be used as the foundation
    for a plugin that overrides only specific callbacks. *)
module Simple : sig
  module Session : sig
    type t =
      { local         : Address.t
      ; remote        : Address.t
      ; helo          : string option
      ; tls           : bool
      ; authenticated : string option
      } [@@deriving fields, sexp_of]

    val empty : t

    include Session with type t := t
    (* be polymorphic in [t] when possible *)
    val greeting : 't -> string
    val extensions : 't -> 't Extension.t list
    val disconnect : log:Log.t -> 't -> unit Deferred.t
  end

  module Envelope : sig
    type t =
      { id          : Envelope.Id.t
      ; sender      : Sender.t
      ; sender_args : Sender_argument.t list
      ; recipients  : Email_address.t list
      } [@@deriving fields, sexp_of]

    val smtp_envelope : t -> Email.t -> Envelope.t

    (*_ include Envelope with type session := 'a *)
    val smtp_envelope_info : t -> Envelope.Info.t
    val mail_from
      :  log:Mail_log.t
      -> 'session
      -> Sender.t
      -> Sender_argument.t list
      -> [ `Continue of t
         | `Reject of Smtp_reply.t
         ] Deferred.t
    val rcpt_to
      :  log:Mail_log.t
      -> 'session
      -> t
      -> Email_address.t
      -> [ `Continue of t
         | `Reject of Smtp_reply.t
         ] Deferred.t
    val accept_data
      : log:Mail_log.t
      -> 'session
      -> t
      -> [ `Continue of t
         | `Reject of Smtp_reply.t
         ] Deferred.t
    val process
      : log:Mail_log.t
      -> 'session
      -> t
      -> Email.t
      -> [ `Consume of string
         | `Reject of Smtp_reply.t
         | `Send of Envelope.With_next_hop.t list
         | `Quarantine of Envelope.With_next_hop.t list * Smtp_reply.t * Quarantine_reason.t
         ] Deferred.t
  end

  include S
    with module Session := Session
     and module Envelope := Envelope
end = struct
  module Session = struct
    type t =
      { local         : Address.t
      ; remote        : Address.t
      ; helo          : string option
      ; tls           : bool
      ; authenticated : string option
      } [@@deriving fields, sexp_of]

    let empty =
      { local = `Unix "/dev/null"
      ; remote = `Unix "/dev/null"
      ; helo = None
      ; tls = false
      ; authenticated = None
      }

    let connect ~log:_ ~local ~remote =
      return (`Accept { empty with local; remote })

    let greeting _ =
      sprintf "%s ocaml/mailcore 0.2 %s"
        (Unix.gethostname ())
        (Time.now () |> Time.to_string_abs ~zone:Time.Zone.utc)

    let extensions _ = []

    let helo ~log:_ session helo =
      return (`Continue { session with helo = Some helo })

    let disconnect ~log:_ _ =
      return ()
  end

  module Envelope = struct
    type t =
      { id          : Envelope.Id.t
      ; sender      : Sender.t
      ; sender_args : Sender_argument.t list
      ; recipients  : Email_address.t list
      } [@@deriving sexp_of, fields]

    let smtp_envelope_info { id; sender; sender_args; recipients } =
      Envelope.Info.create ~id ~sender ~sender_args ~recipients ()

    let smtp_envelope t email =
      let info = smtp_envelope_info t in
      Envelope.create' ~info ~email

    let mail_from ~log:_ _session sender sender_args =
      return (`Continue { id = Envelope.Id.create (); sender; sender_args; recipients = [] })

    let rcpt_to ~log:_ _session t recipient =
      return (`Continue { t with recipients = t.recipients @ [recipient] })

    let accept_data ~log:_ _session t =
      if List.is_empty t.recipients then
        return (`Reject (Smtp_reply.bad_sequence_of_commands_503 Smtp_command.Data))
      else
        return (`Continue t)

    let process ~log:_ _session t email =
      let envelope = smtp_envelope t email in
      return
        (`Send
           [ Envelope.With_next_hop.create
               ~envelope
               ~next_hop_choices:[]
               ~retry_intervals:[]
           ])
  end

  let rpcs () = []
end
