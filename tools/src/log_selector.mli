open! Core
open Re2
open Async_smtp

module Base : sig
  type t =
    [ `envelope_sender of Regex.t
    | `envelope_recipient of Regex.t
    | `recipient of Regex.t
    | `subject of Regex.t
    | `rfc822_id of Regex.t
    | `flows of Smtp_mail_log.Flows.t
    ] [@@deriving sexp]

  val regex : t -> Regex.t
end

type t = Base.t Blang.t [@@deriving sexp]

(** [permissive_cnf query] returns a simplified conjunctive normal form that is
    more permissive and does not contain negated terms.

    There are a couple of special cases to be aware of:
    - The empty conjunction list (top) is equivalent to [True]
    - A disjunction containing a [Not] is replaced by [True]
    - An empty disjuction list (middle) is equivalent to [False], and the whole
    cnf will be rewritten to [ [ ] ].
*)
val permissive_cnf : t -> Base.t list list

val readme : string

val arg : t Command.Arg_type.t

val matches_message : t -> Smtp_mail_log.Message.t -> bool
