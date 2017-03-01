open Core

module T = struct
  type t =
    | Sprocket of string
    | Cog of int
  [@@deriving sexp]
end

include T
include Sexpable.To_stringable(T)

module Queue = struct
  type t =
    | Queue1
    | Queue2
    | Queue3
  [@@deriving sexp, enumerate]

  let to_dirname = function
    | Queue1 -> "queue1"
    | Queue2 -> "queue2"
    | Queue3 -> "queue3"
  ;;
end
