open Core
open Async
open Json_wheel_jane_street_overlay.Std

type tag_format =
  (* [field] is the field to use in JSON output. *)
  { field : string
  (* [to_json] translates the string value of the tag into a suitable JSON representation *)
  ; to_json : string list -> Json_type.json_type
  }

(** Translate a [Log.Message.t] into a JSON term suitable for Feeding into ELK.
    Log messages are encoded as a json object with keys:
    @timestamp  -> string of message time ('@' prefix as this is expected by logstash)
    level       -> string of optional message level
    message     -> string of message body
    tag_my_tag  -> string value of tag with key "my_tag"
*)
val json_of_t : Log.Message.t -> Json_type.json_type

(** like [json_of_t] but allows customization of the JSON representation. *)
val json_of_t'
  (* [message] Customizes the representation of the log message put into the message field *)
  :  ?message:([`Sexp of Sexp.t | `String of string] -> Json_type.json_type)
  (* [tags] Customizes the representation of the named tags *)
  -> ?tags:(string * tag_format) list
  -> unit
  -> (Log.Message.t -> Json_type.json_type) Staged.t
