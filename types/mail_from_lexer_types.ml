open Core

type email =
  { local_part : string
  ; domain     : string option
  } [@@deriving sexp, compare]

type email_with_suffix =
  { prefix : string option
  ; sender : [ `Email of email | `Null ]
  ; suffix : string
  } [@@deriving sexp, compare]
