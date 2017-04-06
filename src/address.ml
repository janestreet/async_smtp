open! Core

type t = [`Inet of Host_and_port.t | `Unix of string]
[@@deriving sexp, compare, bin_io, hash]

let to_string t = sexp_of_t t |> Sexp.to_string
let of_string str = Sexp.of_string str |> t_of_sexp
