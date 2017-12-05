open Core
open Async
open Async_smtp_types

module type Start_tls = sig
  type session
  (** [upgrade_to_tls] is called when initiating an upgrade to TLS.
      [Session.greeting] will be called to send a suitable greeting after the upgrade. *)
  val upgrade_to_tls
    :  log:Mail_log.t
    -> session
    -> session Smtp_monad.t
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

      [Ok session] accepts the connection, creating a session.

      [Error err] terminates the connection, sending the reject
      (or [service_unavailable]).
  *)
  val connect
    :  log:Mail_log.t
    -> local:Smtp_socket_address.t
    -> remote:Smtp_socket_address.t
    -> t Smtp_monad.t

  (** [greeting] to send to clients after the connection has been accepted. *)
  val greeting : t -> string

  (** [helo] is called in response to initial handshakes (i.e. HELO or EHLO).

      [Ok session] allows the SMTP session to continue.

      [Error err] terminates the connection, sending the reject
      (or [service_unavailable]). *)
  val helo
    :  log:Mail_log.t
    -> t
    -> string
    -> t Smtp_monad.t

  (** [extensions] that are supported including the associated implementations.
      It is assumed that this will only change after [connect], [helo] and
      [Start_tls.upgrade_to_tls]. *)
  val extensions : t -> t Extension.t list

  (** [disconnect] is called when an SMTP connection is closed. It allows the plugin to
      cleanup any resources associated with this session *)
  val disconnect : log:Log.t -> t -> unit Smtp_monad.t
end

module type Envelope = sig
  type session
  type t

  val smtp_envelope_info : t -> Smtp_envelope.Info.t

  (** [mail_from] is called in the event of a "MAIL FROM" SMTP command.

      [Ok t] creates an envelope that is updated by [rcpt_to] and finally processed
      by [data].

      [Error err] sends the reject (or [service_unavailable]) *)
  val mail_from
    :  log:Mail_log.t
    -> session
    -> Smtp_envelope.Sender.t
    -> Smtp_envelope.Sender_argument.t list
    -> t Smtp_monad.t

  (** [rcpt_to] is called in the event of a "RCPT TO" SMTP command.

      [Ok t] augments the envelope in the pipeline and passes it on to the next
      phase.

      [Error err] sends the reject (or [service_unavailable]). *)
  val rcpt_to
    :  log:Mail_log.t
    -> session
    -> t
    -> Email_address.t
    -> t Smtp_monad.t

  (** [accept_data] is called when the [DATA] command is received to decide
      whether or not to accept the message data. *)
  val accept_data
    : log:Mail_log.t
    -> session
    -> t
    -> t Smtp_monad.t

  (** [process] is called when the message body has been received
      (after the DATA command and [accept_data]).

      [Ok (`Consume ok_msg] drops the message and returns a successful response to the
      client. It should be used if this plugin has processed the message and does not
      intend to relay the message.

      [Error err] drops the message and returns an error reply to the client. It
      should be used when the plugin does not wish to process this message.

      [Ok (`Send envelopes_to_relay)] spools the given envelopes for further sending and gives
      a successful response, once these messages have been safely spooled.

      [Ok (`Quarantine (envelopes_to_quarantine, reply, reason))] saves the given messages to a
      directory for manual inspection and passes through the given reply to the client.
      [reason] is used only internally to tell us what check failed. *)
  val process
    : log:Mail_log.t
    -> session
    -> t
    -> Email.t
    -> [ `Consume of string
       | `Send of Smtp_envelope.Routed.Batch.t list
       | `Quarantine of Smtp_envelope.Routed.Batch.t list * Smtp_reply.t * Quarantine_reason.t
       ] Smtp_monad.t

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
      { local         : Smtp_socket_address.t
      ; remote        : Smtp_socket_address.t
      ; helo          : string option
      ; tls           : bool
      ; authenticated : string option
      } [@@deriving fields, sexp_of]

    val empty : t

    include Session with type t := t
    (* be polymorphic in [t] when possible *)
    val greeting : 't -> string
    val extensions : 't -> 't Extension.t list
    val disconnect : log:Log.t -> 't -> unit Smtp_monad.t
  end

  module Envelope : sig
    type t =
      { id          : Smtp_envelope.Id.t
      ; sender      : Smtp_envelope.Sender.t
      ; sender_args : Smtp_envelope.Sender_argument.t list
      ; recipients  : Email_address.t list
      } [@@deriving fields, sexp_of]

    val smtp_envelope : t -> Email.t -> Smtp_envelope.t

    (*_ include Envelope with type session := 'a *)
    val smtp_envelope_info : t -> Smtp_envelope.Info.t
    val mail_from
      :  log:Mail_log.t
      -> 'session
      -> Smtp_envelope.Sender.t
      -> Smtp_envelope.Sender_argument.t list
      -> t Smtp_monad.t
    val rcpt_to
      :  log:Mail_log.t
      -> 'session
      -> t
      -> Email_address.t
      -> t Smtp_monad.t
    val accept_data
      : log:Mail_log.t
      -> 'session
      -> t
      -> t Smtp_monad.t
    val process
      : log:Mail_log.t
      -> 'session
      -> t
      -> Email.t
      -> [ `Consume of string
         | `Send of Smtp_envelope.Routed.Batch.t list
         | `Quarantine of Smtp_envelope.Routed.Batch.t list * Smtp_reply.t * Quarantine_reason.t
         ] Smtp_monad.t
  end

  include S
    with module Session := Session
     and module Envelope := Envelope
end = struct
  open Smtp_monad.Let_syntax

  module Session = struct
    type t =
      { local         : Smtp_socket_address.t
      ; remote        : Smtp_socket_address.t
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
      return { empty with local; remote }

    let greeting _ =
      sprintf "%s ocaml/mailcore 0.2 %s"
        (Unix.gethostname ())
        (Time.now () |> Time.to_string_abs ~zone:Time.Zone.utc)

    let extensions _ = []

    let helo ~log:_ session helo =
      return { session with helo = Some helo }

    let disconnect ~log:_ _ =
      return ()
  end

  module Envelope = struct
    type t =
      { id          : Smtp_envelope.Id.t
      ; sender      : Smtp_envelope.Sender.t
      ; sender_args : Smtp_envelope.Sender_argument.t list
      ; recipients  : Email_address.t list
      } [@@deriving sexp_of, fields]

    let smtp_envelope_info { id; sender; sender_args; recipients } =
      Smtp_envelope.Info.create ~id ~sender ~sender_args ~recipients ()

    let smtp_envelope t email =
      let info = smtp_envelope_info t in
      Smtp_envelope.create' ~info ~email

    let mail_from ~log:_ _session sender sender_args =
      return { id = Smtp_envelope.Id.create (); sender; sender_args; recipients = [] }

    let rcpt_to ~log:_ _session t recipient =
      return { t with recipients = t.recipients @ [recipient] }

    let accept_data ~log:_ _session t =
      if List.is_empty t.recipients then
        Smtp_monad.reject ~here:[%here]
          (Smtp_reply.bad_sequence_of_commands_503 Smtp_command.Data)
      else
        return t

    let process ~log:_ _session t email =
      let envelope =
        Smtp_envelope.Routed.create
          ~envelope:(smtp_envelope t email)
          ~next_hop_choices:[]
          ~retry_intervals:[]
      in
      return (`Send [Smtp_envelope.Routed.Batch.single_envelope envelope])
  end

  let rpcs () = []
end
