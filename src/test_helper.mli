open! Core
open! Async
open Async_smtp_types

(** This module provides helpers for writing expect tests for testing
    [Async_smtp] and [Async_smtp.Server] plugins.
*)

type 'a smtp_flags
(** [tls] pretends that START_TLS was negotiated successfully (default: false) *)
  =  ?tls:bool
  -> 'a

type 'a server_flags
(** [max_message_size] limits the size of messages accepted by the server.
    (default: no practical limit) *)
  =  ?max_message_size:Byte_units.t
  (** [malformed_emails] indicates how a malformed email should be handled.
      (default: [`Reject]) *)
  -> ?malformed_emails:[ `Reject | `Wrap ]
  (** [echo_delivery] will print details of envelopes that are spooled by the plugin.
      This can be very noisy so is generally only of interest if you are testing
      sending behavior.
      (default: false) *)
  -> ?echo_delivery:bool
  (** [server_log] controls the amount of detail logged from the server logic, excluding
      the plugins. This is usually not relevant to tests and generates a lot of noise.
      (default: `None) *)
  -> ?server_log:[Log.Level.t | `None]
  (** Provide a custom [Server.Plugin.S] with custom logic.
      (default: Server.Plugin.Simple) *)
  -> ?plugin:(module Server.Plugin.S)
  (** [plugin_log] controls the log level from the plugin logic. (default: `Debug) *)
  -> ?plugin_log:[Log.Level.t | `None]
  -> 'a

type 'a client_flags
(** Client authentication [credentials] (default: None - no authentication) *)
  =  ?credentials:Credentials.t
  (** [client_greeting] specifies the HELO/EHLO greeting to send.
      (default: "[SMTP TEST CLIENT]") *)
  -> ?client_greeting:string
  (** [client_log] controls the log level from the client logic.
      This is usually not relevant to tests and a lot of noise.
      (default: `None) *)
  -> ?client_log:[Log.Level.t | `None]
  -> 'a

(** Helper for creating SMTP Envelopes *)
val envelope
  :  ?sender:string
  -> ?recipients:string list
  -> ?data:string
  -> unit
  -> Smtp_envelope.t

(** Attempt to send the given envelope to a dummy server.
    Expect test output will be the SMTP session transcript with the following
    format:
    < EHLO Client
    > 200 Server Response
    Custom plugin output *)
val smtp :
  (Smtp_envelope.t list
   -> unit Deferred.t)
    client_flags
    server_flags
    smtp_flags



(** Like [smtp] but instead of the mailcore client you describe the client behaviour
    allowing testing server behaviour in edge cases.

    Use [client] to submit requests to the server, and [server] to document the expected
    responses.

    Example
    {[
      manual_client
        (fun ~client ~server ->
           server "220 [SMTP TEST SERVER]"
           >>= fun () ->
           client "EHLO test"
           >>= fun () ->
           server "250-Ok: Continue, extensions follow:\n\
                   250 8BITMIME"
           >>= fun () ->
           client "RESET"
           >>= fun () ->
           server "250 Ok: continue"
           >>= fun () ->
           client "QUIT"
           >>= fun () ->
           server "221 closing connection"
        )
    ]}
*)
val manual_client :
  ((client:(string -> unit Deferred.t) ->
    server:(string -> unit Deferred.t) ->
    unit Deferred.t)
   -> unit Deferred.t)
    server_flags
    smtp_flags

(** Like [manual_client] but you provide the server side of the protocol.

    Use [client] to document expected requests, and [server] to send the responses. *)
val manual_server :
  (Smtp_envelope.t list
   -> (client:(string -> unit Deferred.t) ->
       server:(string -> unit Deferred.t) ->
       unit Deferred.t)
   -> unit Deferred.t)
    client_flags
    smtp_flags
