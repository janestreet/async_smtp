open! Core
open! Async
module Reject_or_error = Reject_or_error

type 'a t = ('a, Reject_or_error.t) Deferred.Result.t

let error ?reject ~here error =
  Deferred.return (Error (Reject_or_error.of_error ?reject ~here error))
;;

let exn ?reject ~here exn = error ?reject ~here (Error.of_exn exn)
let error_string ?reject ~here str = error ?reject ~here (Error.of_string str)
let errorf ?reject ~here fmt = ksprintf (error_string ?reject ~here) fmt
let reject ~here reject = Deferred.return (Error (Reject_or_error.of_reject ~here reject))

module Sync = struct
  let tag ~tag ?here t = Result.map_error t ~f:(Reject_or_error.tag ~tag ?here)
  let tag' ?tag ?here t = Result.map_error t ~f:(Reject_or_error.tag' ?tag ?here)

  let of_or_error ?tag ~here t =
    Result.map_error t ~f:(Reject_or_error.of_error ~here) |> tag' ?tag ~here
  ;;
end

let ok t = t >>| Result.return
let return_or_error ?tag ~here t = Deferred.return (Sync.of_or_error ?tag ~here t)
let of_or_error ?tag ~here t = t >>| Sync.of_or_error ?tag ~here
let to_or_error = Deferred.Result.map_error ~f:Reject_or_error.error
let tag ~tag ?here t = t >>| Sync.tag ~tag ?here
let tag' ?tag ?here t = t >>| Sync.tag' ?tag ?here

let try_with ?tag ~here f =
  Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log f >>| Sync.of_or_error ?tag ~here
;;

let try_with_join ?tag ~here f = try_with ?tag ~here f >>| Result.join

let try_with_or_error ?tag ~here f =
  Deferred.Or_error.try_with_join ~run:`Schedule ~rest:`Log f
  >>| Sync.of_or_error ?tag ~here
;;

include (Deferred.Result : Monad.S2 with type ('a, 'e) t := ('a, 'e) Deferred.Result.t)
