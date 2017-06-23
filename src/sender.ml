open! Core
open Email_message.Std

open Or_error.Monad_infix
type t =
  [ `Null
  | `Email of Email_address.t
  ]
[@@deriving bin_io, sexp, compare, hash]

let of_string_with_arguments ?default_domain ~allowed_extensions str =
  Or_error.try_with (fun () -> Mail_from_lexer.parse_mail_from (Lexing.from_string str))
  |> Or_error.tag ~tag:(sprintf "Failed to parse [Sender.t] from \"%s\"" str)
  >>= fun mail_from  ->
  Sender_argument.list_of_string ~allowed_extensions mail_from.suffix
  >>= fun all_args ->
  match mail_from.sender with
  | `Null -> Ok (`Null, all_args)
  | `Email email ->
    let domain = Option.first_some email.domain default_domain in
    let email_address =
      Email_address.create ?prefix:mail_from.prefix ?domain email.local_part
    in
    Ok (`Email email_address, all_args)
;;

let of_string ?default_domain str =
  of_string_with_arguments ?default_domain ~allowed_extensions:[] str
  >>| function
  | email,[] -> email
  | _,(_::_) -> failwithf "impossible, unexpected extension arguments" ()
;;

let to_string = function
  | `Null -> "<>"
  | `Email email -> Email_address.to_string email
;;

let to_string_with_arguments (sender,args) =
  to_string sender :: List.map args ~f:Sender_argument.to_string
  |> String.concat ~sep:" "
;;

let map t ~f =
  match t with
  | `Null -> t
  | `Email email -> `Email (f email)
;;

module T = struct
  type nonrec t = t [@@deriving sexp, bin_io, compare]

  let to_string = to_string
  let of_string s = of_string s |> Or_error.ok_exn

  let compare a b = match a,b with
    | `Null, `Null -> 0
    | `Email a, `Email b -> Email_address.compare a b
    | `Null, `Email _ -> -1
    | `Email _, `Null -> 1
  ;;

  let hash_fold_t h = function
    | `Null -> h
    | `Email email -> Email_address.hash_fold_t h email
  let hash = Hash.run hash_fold_t
end
include Hashable.Make(T)
include Comparable.Make(T)
include Sexpable.Of_stringable(T)

module Caseless = struct
  module T = struct
    type nonrec t = t [@@deriving bin_io, sexp]
    let compare a b = match a,b with
      | `Null, `Null -> 0
      | `Email a, `Email b -> Email_address.Caseless.compare a b
      | `Null, `Email _ -> -1
      | `Email _, `Null -> 1
    let hash_fold_t h = function
      | `Null -> h
      | `Email a -> Email_address.Caseless.hash_fold_t h a
    let hash = Hash.run hash_fold_t
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make(T)
end

let%test_module _ =
  (module struct
    let check ~should_fail allowed_extensions str =
      match of_string_with_arguments ~allowed_extensions str with
      | Ok mail_from ->
        not should_fail &&
        String.equal str (to_string_with_arguments mail_from)
      | Error _ -> should_fail

    let%test _ = check ~should_fail:false [] "todd@lubin.us"
    let%test _ = check ~should_fail:false [] "<>"
    let%test _ = check ~should_fail:true [] "<> <>"
    let%test _ = check ~should_fail:false [Auth_login] "<> AUTH=<>"
    let%test _ = check ~should_fail:false [Auth_login] "todd lubin <todd@lubin.us> AUTH=<>"
    let%test _ = check ~should_fail:false [Auth_login] "<todd@lubin.us> AUTH=foobar"
    let%test _ = check ~should_fail:false [] "<todd@lubin.us>"
    let%test _ = check ~should_fail:true [] "<todd@lubin.us> AUTH=foobar"
    let%test _ = check ~should_fail:true [Auth_login] "<todd@lubin.us> FOOBAR=foobar"
  end)
