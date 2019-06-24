module Stable0 = struct
  open! Core.Core_stable

  module V1_no_sexp = struct
    type t =
      [ `Null
      | `Email of Email_address.Stable.V1.t
      ]
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 8dd845baf982d3fc6ad2baf59cd9b41f |}]
    ;;
  end
end

open! Core
open Or_error.Let_syntax

module T = struct
  type t =
    [ `Null
    | `Email of Email_address.t
    ]
  [@@deriving compare, hash]

  let to_string = function
    | `Null -> "<>"
    | `Email email -> Email_address.to_string email
  ;;

  let sexp_of_t t = sexp_of_string (to_string t)
end

include T
include Hashable.Make_plain (T)
include Comparable.Make_plain (T)

let of_string_with_arguments ?default_domain ~allowed_extensions str =
  let%bind mail_from =
    Or_error.try_with (fun () ->
      Mail_from_lexer.parse_mail_from (Lexing.from_string str))
    |> Or_error.tag ~tag:(sprintf "Failed to parse [Sender.t] from \"%s\"" str)
  in
  let%bind all_args =
    Sender_argument.list_of_string ~allowed_extensions mail_from.suffix
  in
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
  match%map of_string_with_arguments ?default_domain ~allowed_extensions:[] str with
  | email, [] -> email
  | _, _ :: _ -> failwithf "impossible, unexpected extension arguments" ()
;;

let to_string_with_arguments (sender, args) =
  to_string sender :: List.map args ~f:Sender_argument.to_string
  |> String.concat ~sep:" "
;;

let map t ~f =
  match t with
  | `Null -> t
  | `Email email -> `Email (f email)
;;

module Caseless = struct
  module T = struct
    type nonrec t =
      (* t = *)
      [ `Null
      | `Email of Email_address.Caseless.t
      ]
    [@@deriving compare, hash]

    let sexp_of_t = sexp_of_t
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
end

let%test_module _ =
  (module struct
    let check ~should_fail allowed_extensions str =
      match of_string_with_arguments ~allowed_extensions str with
      | Ok mail_from ->
        (not should_fail) && String.equal str (to_string_with_arguments mail_from)
      | Error _ -> should_fail
    ;;

    let%test _ = check ~should_fail:false [] "foo@bar.com"
    let%test _ = check ~should_fail:false [] "<>"
    let%test _ = check ~should_fail:true [] "<> <>"
    let%test _ = check ~should_fail:false [ Auth [] ] "<> AUTH=<>"
    let%test _ = check ~should_fail:false [ Auth [] ] "foo bar <foo@bar.com> AUTH=<>"
    let%test _ = check ~should_fail:false [ Auth [] ] "<foo@bar.com> AUTH=foobar"
    let%test _ = check ~should_fail:false [] "<foo@bar.com>"
    let%test _ = check ~should_fail:true [] "<foo@bar.com> AUTH=foobar"
    let%test _ = check ~should_fail:true [ Auth [] ] "<foo@bar.com> FOOBAR=foobar"
  end)
;;

module Stable = struct
  include Stable0

  module V1 = struct
    include V1_no_sexp

    include Sexpable.Of_stringable (struct
        type nonrec t = t

        let of_string s = Or_error.ok_exn (of_string s)
        let to_string t = to_string t
      end)
  end
end
