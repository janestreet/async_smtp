open Core
open Async
open Async_smtp_types

module Mail_fingerprint : sig
  type t =
    { headers : (string * string) list
    ; md5 : string option
    ; parts : t list
    }
  [@@deriving sexp, fields ~getters]

  val of_email : Email.t -> compute_md5:bool -> t
end

(* A flow is a causal chain of events. If an event B is caused by an event A (such as an
   outgoing email caused by an incoming email) then the log message for B will contain all
   the flow ids of the the log message for A.

   Each message will have multiple flow IDs indicating what it is related to.

   [`Server] - Included on all log messages related to a SMTP session on the server side
   and all log messages relating to any envelope received on the session. Issued when a
   TCP session is established (or an SMTP session is otherwise established)

   [`Client] - Included on all log messages related to a SMTP session on the client side
   No new messages are generated here, so these flow IDs are short lived. NB: When a
   Client session is established this will be the only ID until an envelope is sent, at
   which point the IDs relevant to the envelope will also be included.

   [`Inbound_envelope] - Include on all log messages related to an Inbound envelope, and
   any outbound messages generated from it. It is issued upon 'MAIL FROM',

   [`Outbound_envelope] - Included on all log messages related to an outbound envelope.
   This ID is issued when spooling a message, or when using the client directly,

   [`Cached_connection] - Included on all log messages related to opening/closing/using an
   SMTP connection. Entries with the same ID are using the same underlying connection.
*)
module Flows : sig
  module Kind : sig
    type t =
      [ `Server_session
      | `Client_session
      | `Inbound_envelope
      | `Outbound_envelope
      | `Cached_connection
      ]
    [@@deriving sexp_of, equal]
  end

  module Id : sig
    type t = private string [@@deriving sexp_of]

    val is : t -> Kind.t -> bool
    val kind : t -> Kind.t option

    include Stringable.S with type t := t
    include Comparable.S_plain with type t := t
    include Hashable.S_plain with type t := t

    module For_test : sig
      val create : Kind.t -> int -> t
    end
  end

  (* Represents a set of opaque flow ids. The internal list representation is exposed for
     use when analysing logs, however the order of elements is undefined. *)

  type t = private Id.t list [@@deriving sexp_of]

  val of_list : Id.t list -> t
  val to_list : t -> Id.t list

  (* Should be used with care, but appropriate on some global state messages *)

  val none : t
  val is_none : t -> bool

  (* The [Kind.t] is only informational and is included in the sexp for information only.
     It is not recoverable and not intended for machine processing. two flows that where
     created are never related. [ not (are_related (create t) (create t)) ] *)

  val create : Kind.t -> t

  (* Create a related flow. An extended flow is always related to its parent.
     [ are_related x (extend x t) ] *)

  val extend : t -> Kind.t -> t

  (* Combine two flows to created a flow related to each.
     [ are_related x (union x y) && are_related y (union x y) ] *)

  val union : t -> t -> t

  (* Indicates that two flows where extended from one another.
     [ are_related x x && (are_related x y = are_related y x) ] *)

  val are_related : t -> t -> bool
  val dedup : t -> t
end

module Component : sig
  type t = string list

  include Stringable.S with type t := t

  val parts : t -> string list
  val join : t -> t -> t
  val is_parent : parent:t -> t -> bool
  val unknown : t
  val is_unknown : t -> bool
end

(** Special tags that are used by the mailcoregrep utility in order to parse the log
    messages *)
module Session_marker : sig
  type t =
    [ `Connected
    | `Mail_from
    | `Rcpt_to
    | `Data
    | `Sending
    ]
end

(** Wrapper arround Log.Message.t that allows access to various standardised tag names. *)
module Message : sig
  module Action : Identifiable with type t = string

  (* See the relevant accessors for information about how these are encoded *)

  type 'a with_info =
    flows:Flows.t
    -> component:Component.t
    -> here:Source_code_position.t
    -> ?local_ip_address:Socket.Address.Inet.t
    -> ?remote_address:Host_and_port.t
    -> ?remote_ip_address:Socket.Address.Inet.t
    -> ?email:
         [ `Fingerprint of Mail_fingerprint.t
         | `Email of Email.t
         | `Envelope of Smtp_envelope.t
         ]
    -> ?compute_body_fingerprint_hash:bool
    -> ?message_size:int
    -> ?rfc822_id:string
    -> ?local_id:Smtp_envelope.Id.t
    -> ?sender:[ `Sender of Smtp_envelope.Sender.t | `String of string ]
    -> ?recipients:[ `Email of Email_address.t | `String of string ] list
    -> ?spool_id:string
    -> ?command:Smtp_command.t
    -> ?reply:Smtp_reply.t
    -> ?session_marker:Session_marker.t
    -> ?tags:(string * string) list
    -> 'a

  type t = Log.Message.t [@@deriving sexp_of]

  val create : (Action.t -> t) with_info

  (* Should be used only for extended debug output *)

  val debugf : (('a, unit, string, t) format4 -> 'a) with_info
  val of_error : (Error.t -> t) with_info
  val info : (unit -> t) with_info

  (* Utility accessors for the standard info tags *)

  val level : t -> Log.Level.t
  val time : t -> Time_float.t

  (** Encoded as one tag 'flow' for each Flow id *)
  val flows : t -> Flows.t

  (** The originating component, encoded as tag 'component'. Use of
      [with_flow_and_component] will cause this tag to be rewritten. *)
  val component : t -> Component.t

  (** Alias for the message field. *)
  val action : t -> Action.t

  (* If a value doesn't parse, or is missing this will give back [None] *)

  val find_tag' : t -> tag:string -> f:(string -> 'a) -> 'a option
  val find_tag : t -> tag:string -> string option
  val tags : t -> (string * string) list

  (* tag 'rfc822-id' *)

  val rfc822_id : t -> string option

  (* tag 'local-id' *)

  val local_id : t -> Smtp_envelope.Id.t option

  (* tag 'spool-id' *)

  val spool_id : t -> string option

  (* tag 'sender'. [`String _] if the value doesn't parse *)

  val sender : t -> [ `Sender of Smtp_envelope.Sender.t | `String of string ] option

  (* tag 'recipient', [`String _] if the value doesn't parse, one tag per recipient. nb:
     [create ~recipients:[]] is encoded by a single recipient tag with an empty string.
  *)

  val recipients : t -> [ `Email of Email_address.t | `String of string ] list option

  (* tag 'email-fingerprint' *)

  val email : t -> Mail_fingerprint.t option

  (* tag 'local-ip-address' *)

  val local_ip_address : t -> Socket.Address.Inet.t option

  (* tag 'remote-address' *)

  val remote_address : t -> Host_and_port.t option

  (* tag 'remote-ip-address' *)

  val remote_ip_address : t -> Socket.Address.Inet.t option

  (* tag 'command' *)

  val command : t -> Smtp_command.t option

  (* tag 'reply' *)

  val reply : t -> Smtp_reply.t option

  (* tag 'session-marker' *)

  val session_marker : t -> Session_marker.t option
end

type t = Log.t

(** [with_flow_and_component] - Add additional component and flow ids to log messages.

    The "component" tag (if present) will be prepended or added if missing with
    [component].

    The given flows will be added as additional 'flow' tags (potentially adding a
    duplicate). *)
val with_flow_and_component : flows:Flows.t -> component:Component.t -> t -> t

(* This function is to give external users of this library a chance to control the
   verbosity of our logs. *)

val adjust_log_levels
  :  ?minimum_level:
       Log.Level.t
       (* Only output messages of level > [minimum_level] AND level > [Log.level t] *)
  -> ?remap_info_to:
       Log.Level.t (* Rewrite messages with level [`Info] to level [remap_info_to] *)
  -> ?remap_error_to:
       Log.Level.t (* Rewrite messages with level [`Error] to level [remap_error_to] *)
  -> t
  -> t

(** [message] outputs the given message (if appropriate for the current log level).

    Use [Message.create], [Message.debug] or [Message.of_error] to create the [Message.t].
    e.g. [ Mail_log.info log (lazy Message.create ~component:"world" "hello"); ]

    Special notes about the behaviour of these functions: * The message is dropped without
    forcing if [level] is less that [Log.level t]. * If the message has no loglevel set
    the level to match. * If [t] has information attached to it via
    [with_flow_and_component], add that information to the message. *)
val message : t -> level:Log.Level.t -> Message.t Lazy.t -> unit

val message' : t -> level:Log.Level.t -> Message.t -> unit

(** [info] is shorthand for [message ~level:`Info]. *)
val info : t -> Message.t Lazy.t -> unit

(** [debug] is shorthand for [message ~level:`Debug]. *)
val debug : t -> Message.t Lazy.t -> unit

(** [error] is shorthand for [message ~level:`Error] *)
val error : t -> Message.t Lazy.t -> unit

module Stable : sig
  module Flows : sig
    module V1 : sig
      type t = Flows.t [@@deriving sexp, bin_io]
    end
  end
end
