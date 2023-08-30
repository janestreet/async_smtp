open! Core
open Async_smtp_types

module type Mech = Auth.Client

type elt
type t = elt list [@@deriving sexp_of, compare, hash]

val anon : t
val login : ?on_behalf_of:string -> username:string -> password:string -> unit -> t
val custom : (module Mech) -> t
val allows_anon : t -> bool

val get_auth_client
  :  t
  -> tls:bool (** Plaintext protocols should not be used without [tls] *)
  -> Smtp_extension.t list
  -> [ `Anon | `Auth_with of (module Mech) ] Or_error.t

module Stable : sig
  module Login : sig
    module V1 : sig
      type t =
        { on_behalf_of : string option
        ; username : string
        ; password : string
        }
      [@@deriving sexp]
    end
  end

  module V1 : sig
    type t =
      { username : string
      ; password : string
      }
  end

  module V2 : sig
    type elt =
      | Login of Login.V1.t
      | Anon

    type t = elt list

    val of_v1 : V1.t -> t
  end

  module V3 : sig
    type nonrec t = t [@@deriving sexp, bin_io]

    val of_v2 : V2.t -> t
  end
end
