module type Mech = Auth.Client

module Stable = struct
  open Core.Core_stable

  module Login = struct
    module V1 = struct
      type t =
        { on_behalf_of : string sexp_option
        ; username : string
        ; password : string;
        } [@@deriving sexp]
    end
  end

  module V1 = struct
    type t =
      { username : string
      ; password : string;
      } [@@deriving sexp]
  end

  module V2 = struct
    type elt =
      | Login of Login.V1.t
      | Anon
    [@@deriving sexp]

    type t = elt list [@@deriving sexp]

    let of_v1 { V1.username; password } =
      let login = { Login.V1.on_behalf_of = None; username; password } in
      [ Login login ]
  end

  module V3 = struct
    type mech = (module Mech)
    (*_ stub [sexp] implementation for [mech] *)
    let sexp_of_mech (module A : Mech) =
      [%sexp (A.mechanism : string)]
    let mech_of_sexp = [%of_sexp:_]

    type elt =
      | Anon
      | Login of Login.V1.t
      | Custom of mech
    [@@deriving sexp]

    type t = elt list [@@deriving sexp]

    let of_v2 = Core.List.map ~f:(function
      | V2.Login login -> Login login
      | V2.Anon        -> Anon)
  end
end

open! Core
open Async_smtp_types

module Login = struct
  type t = Stable.Login.V1.t =
    { on_behalf_of : string sexp_option
    ; username     : string
    ; password     : string sexp_opaque
    } [@@deriving sexp_of]
end

type mech = (module Mech)
let sexp_of_mech = [%sexp_of:Stable.V3.mech]

type elt = Stable.V3.elt =
  | Anon
  | Login of Login.t
  | Custom of mech
[@@deriving sexp_of]

let sexp_of_elt = function
  | Custom mech -> sexp_of_mech mech
  | elt -> sexp_of_elt elt

type t = elt list [@@deriving sexp_of]

let allows_anon = List.exists ~f:(function
  | Login _ | Custom _ -> false
  | Anon -> true)

let anon = [Anon]

let login ?on_behalf_of ~username ~password () =
  [ Login { Login.on_behalf_of; username; password } ]

let custom mech = [ Custom mech ]

let get_methods t ~tls =
  List.concat_map t ~f:(function
    | Anon -> []
    | Login { Login.on_behalf_of; username; password } ->
      if not tls then []
      else begin
        let module Cred = struct
          let on_behalf_of = on_behalf_of
          let username = username
          let password = password
        end in
        (module Auth.Plain.Client(Cred) : Mech)
        :: (if Option.is_none on_behalf_of
            then [ (module Auth.Login.Client(Cred) : Mech) ]
            else [])
      end
    | Custom ((module A : Mech) as mech) ->
      if tls || not (A.require_tls) then [ mech ]
      else [])

let get_auth_client t ~tls extensions =
  let client_mechs = get_methods t ~tls in
  let server_mechs = List.concat_map extensions ~f:(function
    | Smtp_extension.Auth mechs -> mechs
    | _ -> [])
  in
  List.find_map server_mechs ~f:(fun m ->
    List.find client_mechs ~f:(fun (module M : Mech) ->
      String.Caseless.equal m M.mechanism))
  |> function
  | Some mech ->
    Ok (`Auth_with mech)
  | None ->
    if allows_anon t then Ok (`Anon)
    else begin
      let client_mechs =
        List.map client_mechs ~f:(fun (module M : Mech) -> M.mechanism)
      in
      Or_error.error_s
        [%sexp
          "No common auth mechanism available and ANON authentication \
           not allowed by client",
          { client_mechs = (client_mechs : string list)
          ; server_mechs = (server_mechs : string list)
          }]
    end
