open! Core
open! Async
open! Async_smtp_types

module type Server = sig
  type session

  val mechanism : string

  (** perform the server side authentication negotiation.

      [send_challenge_and_expect_response] should be used to perform a challenge/response
      exchange. If the client sent an initial response then the initial challenge will be
      quietly discarded.

      If the authentication flow has been completed, return [Allow session] or [Deny msg]
      as appropriate to complete the flow.

      If the negotiation fails for any reason (e.g. malformed client message or network
      error) [send_challenge_and_expect_response] will raise, you should allow this to
      bubble up.

      You should use [Monitor.protect] to do any necessary cleanup. *)
  val negotiate
    :  log:Mail_log.t
    -> session
    -> send_challenge_and_expect_response:(string -> string Smtp_monad.t)
    -> session Smtp_monad.t
end

module type Client = sig
  (** A mechanism with [require_tls] will only be used if STARTTLS was negotiated during
      the SMTP session. [Login] and [Plain] below both require tls. If you need to use one
      of these mechanisms on an insecure transport you need to define your own custom
      mechanisms. *)
  val require_tls : bool

  val mechanism : string

  (** perform the client side authentication negotiation.

      [send_response_and_expect_challenge] should be used to perform a challenge/response
      exchange.

      [`Start_auth] will initiate the AUTH exchange without an initial response. Sending a
      [`Response] initiates the AUTH exchange with an initial response if the exchange has
      not been started yet.

      If the negotiation fails for any reason (e.g. bad credentials or network error)
      [send_response_and_expect_challenge] will raise, you should allow this to bubble up.

      You should use [Monitor.protect] to do any necessary cleanup.

      It is an error not to call [send_response_and_expect_challenge] at least once, and
      it is an error to call it after the exchange has been completed. *)
  val negotiate
    :  log:Mail_log.t
    -> remote:Host_and_port.t option
    -> send_response_and_expect_challenge:
         ([ `Start_auth | `Response of string ]
          -> [ `Challenge of string | `Auth_completed ] Deferred.t)
    -> unit Deferred.t
end

module Login : sig
  module Server (Session : sig
      type t

      val authenticate
        :  log:Mail_log.t
        -> t
        -> username:string
        -> password:string
        -> t Smtp_monad.t
    end) : Server with type session = Session.t

  module Client (C : sig
      val username : string
      val password : string
    end) : Client
end

module Plain : sig
  module Server (Session : sig
      type t

      val authenticate
        :  log:Mail_log.t
        -> ?on_behalf_of:string
        -> t
        -> username:string
        -> password:string
        -> t Smtp_monad.t
    end) : Server with type session = Session.t

  module Client (C : sig
      val on_behalf_of : string option
      val username : string
      val password : string
    end) : Client
end
