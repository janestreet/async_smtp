open! Core
open! Async

module type Server = sig
  type session

  val mechanism : string

  val negotiate
    :  log:Mail_log.t
    -> session
    -> send_challenge_and_expect_response:(string -> string Smtp_monad.t)
    -> session Smtp_monad.t
end

module type Client = sig
  val require_tls : bool
  val mechanism : string

  val negotiate
    :  log:Mail_log.t
    -> remote:Host_and_port.t option
    -> send_response_and_expect_challenge:
         ([ `Start_auth | `Response of string ]
          -> [ `Challenge of string | `Auth_completed ] Deferred.t)
    -> unit Deferred.t
end

module Plain = struct
  let mechanism = "PLAIN"

  module Server (Session : sig
      type t

      val authenticate
        :  log:Mail_log.t
        -> ?on_behalf_of:string
        -> t
        -> username:string
        -> password:string
        -> t Smtp_monad.t
    end) : Server with type session = Session.t = struct
    open Smtp_monad.Let_syntax

    type session = Session.t

    let mechanism = mechanism

    let negotiate ~log session ~send_challenge_and_expect_response =
      match%bind
        send_challenge_and_expect_response
          "Require ${ON_BEHALF}\\NULL${USERNAME}\\NULL${PASSWORD}"
        >>| String.split ~on:'\000'
      with
      | [ on_behalf_of; username; password ] ->
        let on_behalf_of =
          match on_behalf_of with
          | "" -> None
          | _ -> Some on_behalf_of
        in
        Session.authenticate ~log session ?on_behalf_of ~username ~password
      | _ ->
        Smtp_monad.reject
          ~here:[%here]
          (Smtp_reply.unable_to_accommodate_455
             "AUTH PLAIN request: malformed plain auth string. Expected \
              ${ON_BEHALF}\\NULL${USERNAME}\\NULL${PASSWORD}")
    ;;
  end

  module Client (Cred : sig
      val on_behalf_of : string option
      val username : string
      val password : string
    end) : Client = struct
    let require_tls = true
    let mechanism = mechanism

    let negotiate ~log:_ ~remote:_ ~send_response_and_expect_challenge =
      let response =
        sprintf
          "%s\000%s\000%s"
          (Option.value ~default:"" Cred.on_behalf_of)
          Cred.username
          Cred.password
      in
      match%map send_response_and_expect_challenge (`Response response) with
      | `Auth_completed -> ()
      | `Challenge challenge ->
        raise_s
          [%message
            "Unexpected challenge from server" (mechanism : string) (challenge : string)]
    ;;
  end
end

module Login = struct
  let mechanism = "LOGIN"

  module Server (Session : sig
      type t

      val authenticate
        :  log:Mail_log.t
        -> t
        -> username:string
        -> password:string
        -> t Smtp_monad.t
    end) : Server with type session = Session.t = struct
    type session = Session.t

    let mechanism = mechanism

    let negotiate ~log session ~send_challenge_and_expect_response =
      send_challenge_and_expect_response "Username:"
      >>=? fun username ->
      send_challenge_and_expect_response "Password:"
      >>=? fun password -> Session.authenticate ~log session ~username ~password
    ;;
  end

  module Client (Cred : sig
      val username : string
      val password : string
    end) : Client = struct
    let require_tls = true
    let mechanism = mechanism

    let negotiate ~log:_ ~remote:_ ~send_response_and_expect_challenge =
      match%bind send_response_and_expect_challenge `Start_auth with
      | `Auth_completed ->
        raise_s
          [%message "Auth completed before sending any credentials" (mechanism : string)]
      | `Challenge (_ : string) ->
        (match%bind send_response_and_expect_challenge (`Response Cred.username) with
         | `Auth_completed ->
           raise_s [%message "Unexpected AUTH termination" (mechanism : string)]
         | `Challenge (_ : string) ->
           (match%map send_response_and_expect_challenge (`Response Cred.password) with
            | `Auth_completed -> ()
            | `Challenge challenge ->
              raise_s
                [%message
                  "Unexpected challenge from server"
                    (mechanism : string)
                    (challenge : string)]))
    ;;
  end
end
