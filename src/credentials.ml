open! Core

type t =
  { username : string
  ; password : string;
  } [@@deriving sexp, fields]

let create = Fields.create
