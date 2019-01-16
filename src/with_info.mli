open! Core

type 'a with_here = here:Source_code_position.t -> 'a
type 'a with_maybe_here = ?here:Source_code_position.t -> 'a
type 'a with_reject = ?reject:Smtp_reply.t -> 'a with_here
type 'a with_tag = ?tag:string -> 'a with_here
