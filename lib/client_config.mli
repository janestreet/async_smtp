open Core.Std
open Async.Std
open Async_ssl.Std

module Tls : sig
  type t =
    { version : Ssl.Version.t option
    ; name : string option
    ; ca_file : string option
    ; ca_path : string option
    (* All the modes except [Required] only provide security against a passive
       attacker. If you care about real security to a particular destination, you should
       verify that their mail server supports TLS and set [mode] to [`Required] and
       certificate_mode to [`Verify]. *)
    ; mode :
        (* Always attempt TLS negotiation (even when the server does not advertise it) and
           abort if TLS fails. *)
        [ `Required
        (* Always attempt TLS negotiation (even when the server does not advertise it) but
           if negotiation fails continue without TLS. This is the Default and recommended
           behavior. *)
        | `Always_try
        (* Attempt TLS negotiation if the server advertises it. *)
        | `If_available
        ]
    ; certificate_mode :
        (* Accept any and all certificates. Succeed even if certificate is absent or
           malformed. *)
        [ `Ignore
        (* Require that the certificate exists, matches, and has a valid chain *)
        | `Verify ]
    } with sexp, fields

  val default : t
end

module Domain_suffix : Identifiable

type t =
  (* Defaults to [Unix.gethostname ()]. *)
  { greeting : string sexp_option
  (* First match wins. No match - no TLS. To add a default setting include an empty suffix
     at the end of the list. *)
  ; tls : (Domain_suffix.t * Tls.t) list
  } with sexp, fields

val default : t

val load_exn : string -> t Deferred.t

val match_tls_domain : t -> string -> Tls.t option

val has_tls : t -> bool
