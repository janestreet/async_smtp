open! Core
open! Async

module Server : sig
  module type S = sig
    type session
    val mechanism : string
    (** perform the server side authentication negotiation.

        [send_challenge_and_expect_response] should be used to perform a challenge/response
        exchange. If the client sent an initial response then the initial challenge will
        be quietly discarded.

        If the authentication flow has been completed, return [Allow session] or [Deny msg]
        as appropriate to complete the flow.

        If the negotiation fails for any reason (e.g. malformed client message or
        network error) [send_challenge_and_expect_response] will raise, you should allow
        this to bubble up.

        You should use [Monitor.protect] to do any necessary cleanup. *)
    val negotiate
      :  log:Mail_log.t
      -> session
      -> send_challenge_and_expect_response:
           (string -> string Deferred.Or_error.t)
      -> [ `Allow of session
         | `Deny of Smtp_reply.t
         ] Deferred.Or_error.t
  end

  module Login(Session : sig
      type t
      val authenticate
        :  log:Mail_log.t
        -> t
        -> username:string
        -> password:string
        -> [ `Allow of t
           | `Deny of Smtp_reply.t
           ] Deferred.Or_error.t
    end) : S with type session=Session.t

  module Plain(Session : sig
      type t
      val authenticate
        :  log:Mail_log.t
        -> ?on_behalf_of:string
        -> t
        -> username:string
        -> password:string
        -> [ `Allow of t
           | `Deny of Smtp_reply.t
           ] Deferred.Or_error.t
    end) : S with type session=Session.t
end

module Client : sig
  module type S = sig
    val mechanism : string
    (** perform the client side authentication negotiation.

        [send_response_and_expect_challenge] should be used to perform a
        challenge/response exchange.

        [`Start_auth] will initiate the AUTH exchange without an initial response. Sending
        a [`Response] initiates the AUTH exchange with an initial response if the exchange
        has not been started yet.

        If the negotiation fails for any reason (e.g. bad credentials or network error)
        [send_response_and_expect_challenge] will raise, you should allow this to bubble
        up.

        You should use [Monitor.protect] to do any necessary cleanup.

        It is an error not to call [send_response_and_expect_challenge] at least once, and
        it is an error to call it after the exchange has been completed. *)
    val negotiate
      :  log:Mail_log.t
      -> send_response_and_expect_challenge:
           ([`Start_auth | `Response of string]
            -> [ `Challenge of string | `Auth_completed] Deferred.t)
      -> unit Deferred.t
  end
  module Login(C : sig
      val username : string
      val password : string
    end) : S
  module Plain(C : sig
      val on_behalf_of : string option
      val username : string
      val password : string
    end) : S
end

