open! Core
open! Async
open Async_smtp_types

(** This module provides helpers for writing expect tests for testing [Async_smtp] and
    [Async_smtp.Server] plugins. *)

type 'a smtp_flags =
  ?tls:bool
    (** [tls] pretends that START_TLS was negotiated successfully (default: false) *)
  -> 'a

type 'a server_flags =
  ?max_message_size:Byte_units.t
    (** [max_message_size] limits the size of messages accepted by the server. (default:
        no practical limit) *)
  -> ?malformed_emails:[ `Reject | `Wrap ]
       (** [malformed_emails] indicates how a malformed email should be handled. (default:
           [`Reject]) *)
  -> ?server_log:[ Log.Level.t | `None ]
       (** [server_log] controls the amount of detail logged from the server logic,
           excluding the plugins. This is usually not relevant to tests and generates a
           lot of noise. (default: `None) *)
  -> ?plugin:(module Server.Plugin.S with type State.t = unit)
       (** Provide a custom [Server.Plugin.S] with custom logic. (default:
           Server.Plugin.Simple) *)
  -> ?plugin_log:[ Log.Level.t | `None ]
       (** [plugin_log] controls the log level from the plugin logic. (default: `Debug) *)
  -> 'a

type 'a client_flags =
  ?credentials:Credentials.t
    (** Client authentication [credentials] (default: None - no authentication) *)
  -> ?client_greeting:string
       (** [client_greeting] specifies the HELO/EHLO greeting to send. (default:
           "[SMTP TEST CLIENT]") *)
  -> ?client_log:[ Log.Level.t | `None ]
       (** [client_log] controls the log level from the client logic. This is usually not
           relevant to tests and a lot of noise. (default: `None) *)
  -> 'a

(** Helper for creating SMTP Envelopes *)
val envelope
  :  ?sender:string
  -> ?recipients:string list
  -> ?data:string
  -> unit
  -> Smtp_envelope.t

(** {v
 Attempt to send the given envelope to a dummy server.
    Expect test output will be the SMTP session transcript with the following
    format:
    < EHLO Client
    > 200 Server Response
    Custom plugin output
    v} *)
val smtp : (Smtp_envelope.t list -> unit Deferred.t) client_flags server_flags smtp_flags

(** Like [smtp] but instead of the mailcore client you describe the client behaviour
    allowing testing server behaviour in edge cases.

    Use [client] to submit requests to the server, and [server] to document the expected
    responses.

    Example
    {[
      manual_client (fun ~client ~server ~expect_server_close ->
        server "220 [SMTP TEST SERVER]"
        >>= fun () ->
        client "EHLO test"
        >>= fun () ->
        server "250-Ok: Continue, extensions follow:\n250 8BITMIME"
        >>= fun () ->
        client "RESET"
        >>= fun () ->
        server "250 Ok: continue"
        >>= fun () -> client "QUIT" >>= fun () -> server "221 closing connection")
    ]} *)
val manual_client
  : ((client:(string -> unit Deferred.t)
      -> server:(string -> unit Deferred.t)
      -> expect_server_close:(unit -> unit Deferred.t)
      -> unit Deferred.t)
     -> unit Deferred.t)
      server_flags
      smtp_flags

(** Like [manual_client] but you provide the server side of the protocol.

    Use [client] to document expected requests, and [server] to send the responses. *)
val manual_server
  : (Smtp_envelope.t list
     -> (client:(string -> unit Deferred.t)
         -> server:(string -> unit Deferred.t)
         -> expect_client_close:(unit -> unit Deferred.t)
         -> unit Deferred.t)
     -> unit Deferred.t)
      client_flags
      smtp_flags
