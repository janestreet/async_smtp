open! Core
open! Async
open Async_ssl.Std

module Tls : sig
  type t =
    { version : Ssl.Version.t option
    ; options : Ssl.Opt.t list option
    ; name : string option
    ; allowed_ciphers : [ `Secure | `Openssl_default | `Only of string list ]
    ; ca_file : string option
    ; ca_path : string option
        (* All the modes except [Required] only provide security against a passive
           attacker. If you care about real security to a particular destination, you
           should verify that their mail server supports TLS and set [mode] to [`Required]
           and certificate_mode to [`Verify]. *)
    ; mode :
        (* Always attempt TLS negotiation (even when the server does not advertise it) and
           abort if TLS fails. *)
        [ `Required
        | (* Always attempt TLS negotiation (even when the server does not advertise it)
             but if negotiation fails continue without TLS. This is the Default and
             recommended behavior. *)
          `Always_try
        | (* Attempt TLS negotiation if the server advertises it. *)
          `If_available
        ]
    ; certificate_mode :
        (* Accept any and all certificates. Succeed even if certificate is absent or
           malformed. *)
        [ `Ignore
        | (* Require that the certificate exists, matches, and has a valid chain *)
          `Verify
        ]
    }
  [@@deriving sexp, fields ~getters]

  val default : t
end

module Domain_suffix : Identifiable

type t =
  { (* Defaults to [Unix.gethostname ()]. *)
    greeting : string option
      (* First match wins. No match - no TLS. To add a default setting include an empty
         suffix at the end of the list. *)
  ; tls : (Domain_suffix.t * Tls.t) list
      (* Timeout for sending each SMTP command or message block and for receiving each
         SMTP reply. Default: 5min *)
  ; send_receive_timeout : [ `Default | `This of Time_float.Span.t ]
      (* Timeout for the message delivery to complete, after all data has been
         transmitted. Default: timeout * 2 ~ 10m

         The reason this is not the same as [send_receive_timeout] is that in typical
         situations the server will do a lot of work after receiving the end of data and
         before sending the final OK. Thus in practice [send_receive_timeout] can be set
         really low (several seconds), but [final_ok_timeout] needs to be higher.

         Another option would be to have a session timeout. However this is not practical
         since a session may take [O(number of recepients)] to complete. *)
  ; final_ok_timeout : [ `Default | `This of Time_float.Span.t ]
  }
[@@deriving sexp]

val default : t
val load_exn : string -> t Deferred.t
val match_tls_domain : t -> string -> Tls.t option
val has_tls : t -> bool
val send_receive_timeout : t -> Time_float.Span.t
val final_ok_timeout : t -> Time_float.Span.t
