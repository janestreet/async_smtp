open! Core
open Email_message
open Re2

module Base : sig
  (* The "exists" query can equally be accomplished using [sexp query], but
     not the "all" one. *)
  type t =
    [ Email_selector.Base.t
    | `envelope_sender of Regex.t
    | `exists_envelope_recipient of Regex.t
    | `all_envelope_recipients of Regex.t
    ] [@@deriving sexp]

  val matches  : t -> Envelope.t -> bool
  val matches' : t -> Envelope_bodiless.t -> bool
end

type t = Base.t Blang.t [@@deriving sexp]

val matches  : t -> Envelope.t          -> bool
val matches' : t -> Envelope_bodiless.t -> bool

val example : t
