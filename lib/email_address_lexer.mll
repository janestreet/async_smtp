{
type email =
  { prefix : string option
  ; local_part : string
  ; domain : string option
  }
}

let whitespace = [' ' '\r' '\n' '\t']
let unquoted_prefix_char = [^ '<' '>' '@' ',' '"']
let quoted_prefix = '"' [^ '"']* '"'
let unquoted_prefix = (unquoted_prefix_char # whitespace) unquoted_prefix_char*
let prefix = quoted_prefix | unquoted_prefix
let address_char = [^ '<' '>' '@' ','] # whitespace

let address_part =
  (address_char+ as local_part) ( '@' (address_char+ as domain) )?

let email_without_prefix =
  address_part

let email_with_prefix =
  ((prefix whitespace*)? as prefix) '<' address_part '>'

rule parse_list = parse
  | whitespace* ',' whitespace* { parse_list lexbuf }
  | whitespace* eof             { [] }
  | email_with_prefix           { let email =
                                    { prefix = Some prefix; local_part; domain }
                                  in email :: parse_list lexbuf }
  | email_without_prefix        { let email =
                                    { prefix = None; local_part; domain }
                                  in email :: parse_list lexbuf }

and parse_emails = parse
  | whitespace* { parse_list lexbuf }
