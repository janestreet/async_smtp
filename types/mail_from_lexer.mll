{
  open Mail_from_lexer_types
}

let whitespace = [' ' '\r' '\n' '\t']
let unquoted_prefix_char = [^ '<' '>' '@' ',' '"']
let quoted_prefix = '"' [^ '"']* '"'
let unquoted_prefix = (unquoted_prefix_char # whitespace) unquoted_prefix_char*
let prefix = quoted_prefix | unquoted_prefix
let address_char = [^ '<' '>' '@' ','] # whitespace

let address_part =
  (address_char+ as local_part) ( '@' (address_char+ as domain) )?

let suffix_char = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '<' '>' '=' '@' '.']
let maybe_suffix = ((' ' suffix_char*)? as suffix) eof

let email_without_prefix = address_part maybe_suffix

let email_with_prefix =
  ((prefix whitespace*)? as prefix) '<' address_part '>' maybe_suffix

let empty_email = ((prefix whitespace*)? as prefix) '<' '>' maybe_suffix

rule parse_email_address = parse
  | empty_email
      { { prefix = Some prefix; sender = `Null; suffix } }
  | email_with_prefix
      { { prefix = Some prefix; sender = `Email { local_part; domain }; suffix } }
  | email_without_prefix
      { { prefix = None; sender = `Email { local_part; domain }; suffix } }

and parse_mail_from = parse
  | whitespace* { parse_email_address lexbuf }
