open! Core

module Login = struct
  module Stable = struct
    module V1 = struct
      type t =
        { on_behalf_of : string sexp_option
        ; username : string
        ; password : string;
        } [@@deriving sexp]
    end
    type t = V1.t [@@deriving sexp]
  end

  type t = Stable.V1.t =
    { on_behalf_of : string sexp_option
    ; username : string
    ; password : string sexp_opaque;
    } [@@deriving sexp, fields]
end


module Stable = struct
  module V1 = struct
    type t =
      { username : string
      ; password : string;
      } [@@deriving sexp]
  end
  module V2 = struct
    type elt =
      | Login of Login.Stable.V1.t
      | Anon
    [@@deriving sexp]

    type t = elt list [@@deriving sexp]

    let of_v1 { V1.username; password } =
      let login = { Login.Stable.V1.on_behalf_of = None; username; password } in
      [ Login login ]
  end
end

type elt = Stable.V2.elt =
  | Login of Login.t
  | Anon [@@deriving sexp_of]

type t = elt list [@@deriving sexp_of]

let t_of_sexp sexp =
  try Stable.V2.t_of_sexp sexp with
  | _ -> Stable.V1.t_of_sexp sexp |> Stable.V2.of_v1

let anon = [Anon]

let allows_anon = List.exists ~f:(function
  | Login _ -> false
  | Anon -> true)

let login ?on_behalf_of ~username ~password () =
  [ Login { Login.on_behalf_of; username; password } ]

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
        (module Auth.Client.Plain(Cred) : Auth.Client.S)
        :: (if Option.is_none on_behalf_of
            then [ (module Auth.Client.Login(Cred) : Auth.Client.S) ]
            else [])
      end)

let get_auth_client t ~tls extensions =
  let client_mechs = get_methods t ~tls in
  let server_mechs = List.concat_map extensions ~f:(function
    | Smtp_extension.Auth mechs -> mechs
    | _ -> [])
  in
  List.find_map server_mechs ~f:(fun m ->
    List.find client_mechs ~f:(fun (module M : Auth.Client.S) ->
      String.Caseless.equal m M.mechanism))
  |> function
  | Some mech ->
    Ok (`Auth_with mech)
  | None ->
    if allows_anon t then Ok (`Anon)
    else begin
      let client_mechs =
        List.map client_mechs ~f:(fun (module M : Auth.Client.S) -> M.mechanism)
      in
      Or_error.error_s
        [%sexp
          "No common auth mechanism available and ANON authentication \
           not allowed by client",
          { client_mechs = (client_mechs : string list)
          ; server_mechs = (server_mechs : string list)
          }]
    end
