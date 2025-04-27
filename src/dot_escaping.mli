open! Core
open Email_message

(** RFC 5321 section 4.5.2 SMTP Transparency (aka "dot escaping") *)

(** [encode_line_*] and [decode_line_*] dot escape and un-dot escape, respectively, a
    given line. It does NOT handle new line breaks in the middle of the supplied strings. *)
val encode_line_string : string -> String_monoid.t

val encode_line_bigstring : Bigstring.t -> String_monoid.t
val decode_line_string : string -> string
val decode_line_bigstring : Bigstring.t -> Bigstring.t
