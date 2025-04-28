open Core
open Async
open Async_smtp_types

module type State = T

module type Start_tls = sig
  type session

  (** [upgrade_to_tls] is called when initiating an upgrade to TLS. [Session.greeting]
      will be called to send a suitable greeting after the upgrade. *)
  val upgrade_to_tls : log:Mail_log.t -> session -> session Smtp_monad.t
end

module type Auth = Auth.Server

module Extension = struct
  type 'session t =
    | Start_tls of (module Start_tls with type session = 'session)
    | Auth of (module Auth with type session = 'session)
end

module type Session = sig
  type state
  type t

  (** [connect] is called when a client first connects, before any messages are accepted.

      [Ok session] accepts the connection, creating a session.

      [Error err] terminates the connection, sending the reject (or
      [service_unavailable]). *)
  val connect
    :  state:state
    -> log:Mail_log.t
    -> local:Socket.Address.Inet.t
    -> remote:Socket.Address.Inet.t
    -> t Smtp_monad.t

  (** [greeting] to send to clients after the connection has been accepted. *)
  val greeting : t -> string

  (** [helo] is called in response to initial handshakes (i.e. HELO or EHLO).

      [Ok session] allows the SMTP session to continue.

      [Error err] terminates the connection, sending the reject (or
      [service_unavailable]). *)
  val helo : state:state -> log:Mail_log.t -> t -> string -> t Smtp_monad.t

  (** [extensions] that are supported including the associated implementations. It is
      assumed that this will only change after [connect], [helo] and
      [Start_tls.upgrade_to_tls]. *)
  val extensions : state:state -> t -> t Extension.t list

  (** [disconnect] is called when an SMTP connection is closed. It allows the plugin to
      cleanup any resources associated with this session *)
  val disconnect : state:state -> log:Log.t -> t -> unit Smtp_monad.t
end

module type Envelope = sig
  type state
  type session
  type t

  val smtp_envelope_info : t -> Smtp_envelope.Info.t

  (** [mail_from] is called in the event of a "MAIL FROM" SMTP command.

      [Ok t] creates an envelope that is updated by [rcpt_to] and finally processed by
      [data].

      [Error err] sends the reject (or [service_unavailable]) *)
  val mail_from
    :  state:state
    -> log:Mail_log.t
    -> session
    -> Smtp_envelope.Sender.t
    -> Smtp_envelope.Sender_argument.t list
    -> t Smtp_monad.t

  (** [rcpt_to] is called in the event of a "RCPT TO" SMTP command.

      [Ok t] augments the envelope in the pipeline and passes it on to the next phase.

      [Error err] sends the reject (or [service_unavailable]). *)
  val rcpt_to
    :  state:state
    -> log:Mail_log.t
    -> session
    -> t
    -> Email_address.t
    -> t Smtp_monad.t

  (** [accept_data] is called when the [DATA] command is received to decide whether or not
      to accept the message data. *)
  val accept_data : state:state -> log:Mail_log.t -> session -> t -> t Smtp_monad.t

  (** [process] is called when the message body has been received (after the DATA command
      and [accept_data]).

      The returned string is used to construct the SMTP reply. [Ok str] results in "250
      Ok: <str>". *)
  val process
    :  state:state
    -> log:Mail_log.t
    -> flows:Mail_log.Flows.t
    -> session
    -> t
    -> Email.t
    -> string Smtp_monad.t
end

module type S = sig
  module State : State
  module Session : Session with type state := State.t
  module Envelope : Envelope with type state := State.t and type session := Session.t

  val rpcs : unit -> State.t Rpc.Implementation.t list
end

(** [Simple] provides a basic plugin implementation of [S] to be used as the foundation
    for a plugin that overrides only specific callbacks.

    NOTE: [Envelope.process] is unimplemented. It will unconditionally return back "554
    Message processing not implemented". *)
module Simple : sig
  module State : State with type t = unit

  module Session : sig
    type t =
      { local : Socket.Address.Inet.t
      ; remote : Socket.Address.Inet.t
      ; helo : string option
      ; tls : bool
      ; authenticated : string option
      }
    [@@deriving sexp_of]

    val empty : t

    (*_ include Session with type state := 'state *)

    val connect
      :  state:'state
      -> log:Mail_log.t
      -> local:Socket.Address.Inet.t
      -> remote:Socket.Address.Inet.t
      -> t Smtp_monad.t

    val greeting : 't -> string
    val helo : state:'state -> log:Mail_log.t -> t -> string -> t Smtp_monad.t
    val extensions : state:'state -> 't -> 't Extension.t list
    val disconnect : state:'state -> log:Log.t -> 't -> unit Smtp_monad.t
  end

  module Envelope : sig
    type t =
      { id : Smtp_envelope.Id.t
      ; sender : Smtp_envelope.Sender.t
      ; sender_args : Smtp_envelope.Sender_argument.t list
      ; recipients : Email_address.t list
      }
    [@@deriving sexp_of]

    val smtp_envelope : t -> Email.t -> Smtp_envelope.t

    (*_ include Envelope with type state := 'state and type session := 'session *)

    val smtp_envelope_info : t -> Smtp_envelope.Info.t

    val mail_from
      :  state:'state
      -> log:Mail_log.t
      -> 'session
      -> Smtp_envelope.Sender.t
      -> Smtp_envelope.Sender_argument.t list
      -> t Smtp_monad.t

    val rcpt_to
      :  state:'state
      -> log:Mail_log.t
      -> 'session
      -> t
      -> Email_address.t
      -> t Smtp_monad.t

    val accept_data : state:'state -> log:Mail_log.t -> 'session -> t -> t Smtp_monad.t

    val process
      :  state:'state
      -> log:Mail_log.t
      -> flows:Mail_log.Flows.t
      -> 'session
      -> t
      -> Email.t
      -> string Smtp_monad.t
  end

  include
    S
    with module State := State
    with module Session := Session
     and module Envelope := Envelope
end = struct
  open Smtp_monad.Let_syntax
  module State = Unit

  module Session = struct
    type t =
      { local : Socket.Address.Inet.t
      ; remote : Socket.Address.Inet.t
      ; helo : string option
      ; tls : bool
      ; authenticated : string option
      }
    [@@deriving sexp_of]

    let empty =
      let null_inet_addr = Socket.Address.Inet.create_bind_any ~port:0 in
      { local = null_inet_addr
      ; remote = null_inet_addr
      ; helo = None
      ; tls = false
      ; authenticated = None
      }
    ;;

    let connect ~state:_ ~log:_ ~local ~remote = return { empty with local; remote }

    let greeting _ =
      sprintf
        "%s ocaml/mailcore 0.2 %s"
        (Unix.gethostname ())
        (Time_float.now () |> Time_float.to_string_abs ~zone:Time_float.Zone.utc)
    ;;

    let extensions ~state:_ _ = []
    let helo ~state:_ ~log:_ session helo = return { session with helo = Some helo }
    let disconnect ~state:_ ~log:_ _ = return ()
  end

  module Envelope = struct
    type t =
      { id : Smtp_envelope.Id.t
      ; sender : Smtp_envelope.Sender.t
      ; sender_args : Smtp_envelope.Sender_argument.t list
      ; recipients : Email_address.t list
      }
    [@@deriving sexp_of]

    let smtp_envelope_info { id; sender; sender_args; recipients } =
      Smtp_envelope.Info.create ~id ~sender ~sender_args ~recipients ()
    ;;

    let smtp_envelope t email =
      let info = smtp_envelope_info t in
      Smtp_envelope.create' ~info ~email
    ;;

    let mail_from ~state:_ ~log:_ _session sender sender_args =
      return { id = Smtp_envelope.Id.create (); sender; sender_args; recipients = [] }
    ;;

    let rcpt_to ~state:_ ~log:_ _session t recipient =
      return { t with recipients = t.recipients @ [ recipient ] }
    ;;

    let accept_data ~state:_ ~log:_ _session t =
      if List.is_empty t.recipients
      then
        Smtp_monad.reject
          ~here:[%here]
          (Smtp_reply.bad_sequence_of_commands_503 Smtp_command.Data)
      else return t
    ;;

    let process ~state:_ ~log:_ ~flows:_ _session _t _email =
      Smtp_monad.reject
        ~here:[%here]
        (Smtp_reply.transaction_failed_554 "Message processing not implemented")
    ;;
  end

  let rpcs () = []
end
