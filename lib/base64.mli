(** Simple library for encoding and decoding base64 strings using a file-system and
    web-safe variant of base64 wherein '+' and '/' are replaced with '-' and '_'
    respectively.

    See Table 2 in RFC4648 (http://www.ietf.org/rfc/rfc4648.txt) for the full character
    set. *)
val encode : string -> string
val decode : string -> string

val encode_int_exn  : int -> char
val decode_char     : char -> int

val encode_float    : ?length:int -> float -> string
