open Core
open With_info

type t [@@deriving sexp_of]

val createf : (('a, unit, string, t) format4 -> 'a) with_reject
val of_exn : (Exn.t -> t) with_reject
val of_error : (Error.t -> t) with_reject
val of_string : (string -> t) with_reject
val of_reject : (Smtp_reply.t -> t) with_here
val of_list : t list -> t
val tag : tag:string -> ?here:Source_code_position.t -> t -> t
val tag' : ?tag:string -> ?here:Source_code_position.t -> t -> t

(** Reject message to send. The server will send an appropriate generic error if this was
    not set. *)
val reject : t -> Smtp_reply.t option

val error : t -> Error.t
