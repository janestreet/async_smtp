
type email =
  { prefix : string option
  ; local_part : string
  ; domain : string option
  }

val parse_emails : Lexing.lexbuf -> email list
