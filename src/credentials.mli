open! Core

module Login : sig
  module Stable : sig
    module V1 : sig
      type t =
        { on_behalf_of : string sexp_option
        ; username : string
        ; password : string
        }
    end
    type t = V1.t [@@deriving of_sexp]
  end

  type t = Stable.V1.t =
    { on_behalf_of : string sexp_option
    ; username : string
    ; password : string sexp_opaque;
    } [@@deriving sexp, fields]
end

module Stable : sig
  module V1 : sig
    type t =
      { username : string
      ; password : string
      }
  end
  module V2 : sig
    type elt =
      | Login of Login.Stable.V1.t
      | Anon
    type t = elt list
    val of_v1 : V1.t -> t
  end
end

type elt = Stable.V2.elt =
  | Login of Login.t
  | Anon

type t = elt list [@@deriving sexp]

val anon : t

val login : ?on_behalf_of:string -> username:string -> password:string -> unit -> t

val allows_anon : t -> bool

val get_auth_client
  :  t
  -> tls:bool  (** Plaintext protocols should not be used without [tls] *)
  -> Smtp_extension.t list
  -> [ `Anon
     | `Auth_with of (module Auth.Client.S)
     ] Or_error.t
