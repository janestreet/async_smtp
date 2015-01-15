{
(* Can't open Core.Std, as the OCamlLex generated code requires Array.create
 * to be of type (int -> 'a -> 'a t), not (len:int -> 'a -> 'a t) *)
module C = Core.Std
open Comm
}

rule
parse_command =
  parse
  | "HELO" (_* as str)        { Helo str }
  | "MAIL FROM:" (_* as str)  { Sender str }
  | "RCPT TO:" (_* as str)    { Recipient str }
  | "DATA"                    { Data }
  | "NOOP"                    { Noop }
  | "HELP"                    { Help }
  | "QUIT"                    { Quit }
  | _                         { assert(false) }
