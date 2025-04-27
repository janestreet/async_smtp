open! Core
open Async_smtp

module Base : sig
  type t =
    [ `envelope_sender of Re2.t
    | `envelope_recipient of Re2.t
    | `recipient of Re2.t
    | `subject of Re2.t
    | `rfc822_id of Re2.t
    | `flows of Smtp_mail_log.Flows.t
    ]
  [@@deriving sexp]

  val regex : t -> Re2.t
end

type t = Base.t Blang.t [@@deriving sexp]

(** [permissive_cnf query] returns a simplified conjunctive normal form that is more
    permissive and does not contain negated terms.

    There are a couple of special cases to be aware of:
    - The empty conjunction list (top) is equivalent to [True]
    - A disjunction containing a [Not] is replaced by [True]
    - An empty disjuction list (middle) is equivalent to [False], and the whole cnf will
      be rewritten to [ [ ] ]. *)
val permissive_cnf : t -> Base.t list list

val readme : string
val arg : t Command.Arg_type.t
val matches_message : t -> Smtp_mail_log.Message.t -> bool
