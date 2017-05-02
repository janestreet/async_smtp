open Core
open Async
open Json_wheel_jane_street_overlay.Std

module J = Json_type.Build

type tag_format =
  { field : string
  ; to_json : string list -> Json_type.json_type
  }

let json_of_t' ?message ?(tags=[]) () =
  let default_message = function
    | `Sexp sexp -> Json_type.String (Sexp.to_string_mach sexp)
    | `String str -> Json_type.String str
  in
  let message = match message with
    | None -> default_message
    | Some message -> fun msg ->
      try message msg with
      | _ ->  default_message msg
  in
  let tags = String.Map.of_alist_exn tags in
  Staged.stage (fun msg ->
    ([ "@timestamp",
       Log.Message.time msg
       |> Time.to_string_iso8601_basic ~zone:Time.Zone.utc
       |> J.string
     ]
     @
     (Log.Message.level msg
      |> Option.to_list
      |> List.map ~f:(fun l -> "level", J.string (Log.Level.to_string l)))
     @
     [ "message", Log.Message.raw_message msg |> message ]
     @
     (Log.Message.tags msg
      |> String.Map.of_alist_multi
      |> Map.to_alist
      |> List.map ~f:(fun (k,v) ->
        try
          let { field; to_json } = Map.find_exn tags k in
          field, to_json v
        with
        | _ ->
          "tag_" ^ k, match v with
          | [v] -> J.string v
          | v -> List.map v ~f:(fun v -> J.string v) |> J.array))
    ) |> J.objekt)
;;

let json_of_t = Staged.unstage (json_of_t' ())
