open Core
open Async

module Metadata = struct
  module T = struct
    type t =
      | Sprocket of string
      | Cog of int
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)
end

module Data = struct
  type t =
    { serial_number : int
    ; customer : string
    }
  [@@deriving sexp, fields ~iterators:create]

  let load path = Reader.load_sexp path t_of_sexp

  let save ?temp_file t path =
    Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
      Writer.save_sexp ?temp_file ~hum:true path (sexp_of_t t))
  ;;

  let to_string t = Sexp.to_string_hum (sexp_of_t t)
end

module Queue = struct
  type t =
    | Queue1
    | Queue2
    | Queue3
  [@@deriving sexp, enumerate, compare]

  let to_dirname = function
    | Queue1 -> "queue1"
    | Queue2 -> "queue2"
    | Queue3 -> "queue3"
  ;;
end

module Throttle = struct
  let enqueue f = f ()

  (* No throttle *)
end
