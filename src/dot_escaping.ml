open! Core
open Email_message

module type Escapable = sig
  type t

  val length : t -> int
  val get : t -> int -> char
  val drop_first_char : t -> t
  val to_string_monoid : t -> String_monoid.t
end

module Make (E : Escapable) : sig
  val decode : E.t -> E.t
  val encode : E.t -> String_monoid.t
end = struct
  let first_char_is_period t = E.length t > 0 && E.get t 0 |> Char.equal '.'
  let decode t = if first_char_is_period t then E.drop_first_char t else t

  let encode t =
    if first_char_is_period t
    then String_monoid.concat [ String_monoid.of_char '.'; E.to_string_monoid t ]
    else E.to_string_monoid t
  ;;
end

module For_string = Make (struct
    include String

    let drop_first_char t = slice t 1 (length t)
    let to_string_monoid = String_monoid.of_string
  end)

let decode_line_string = For_string.decode
let encode_line_string = For_string.encode

module For_bigstring = Make (struct
    include Bigstring

    let drop_first_char t = sub_shared t ~pos:1 ~len:(length t - 1)
    let to_string_monoid = String_monoid.of_bigstring
  end)

let decode_line_bigstring = For_bigstring.decode
let encode_line_bigstring = For_bigstring.encode
