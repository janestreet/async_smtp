open! Core
open! Async

type t =
  { reject : Smtp_reply.t option [@sexp.option]
  ; error : Error.t
  ; here : Source_code_position.t option [@sexp.option]
  }
[@@deriving sexp_of]

let error t =
  match t.here with
  | None -> t.error
  | Some here ->
    Error.of_thunk (fun () ->
      sprintf !"%{Error#hum}\nat %{Source_code_position}" t.error here)
;;

let reject t = t.reject
let of_error ?reject ~here error = { reject; error; here = Some here }
let of_exn ?reject ~here exn = of_error ?reject ~here (Error.of_exn exn)
let of_string ?reject ~here msg = of_error ?reject ~here (Error.of_string msg)
let createf ?reject ~here fmt = ksprintf (of_string ?reject ~here) fmt

let of_reject ~here reject =
  of_error ~reject ~here (Error.create "REJECT" reject [%sexp_of: Smtp_reply.t])
;;

let of_list ts =
  { reject = List.find_map ts ~f:reject
  ; error = Error.of_list (List.map ts ~f:error)
  ; here = None
  }
;;

let tag_error ~tag t = { t with error = Error.tag t.error ~tag }

let maybe_tag_error ?tag t =
  match tag with
  | None -> t
  | Some tag -> tag_error ~tag t
;;

let tag_here ~here t =
  match t.here with
  | None -> { t with here = Some here }
  | Some here' ->
    if Source_code_position.equal here here'
    then t
    else { t with here = Some here; error = error t }
;;

let maybe_tag_here ?here t =
  match here with
  | None -> t
  | Some here -> tag_here ~here t
;;

let tag ~tag ?here t = tag_error ~tag t |> maybe_tag_here ?here
let tag' ?tag ?here t = maybe_tag_error ?tag t |> maybe_tag_here ?here
