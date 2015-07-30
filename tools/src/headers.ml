open Core.Std
open Async.Std
open Async_smtp.Std
open Email_message.Std

module Field_name = Email_field_name
module Envelope = Smtp_envelope

module Crypto = Cryptokit
module Hash = Crypto.Hash

module Config = struct
  module Header_cond = struct
    type t =
      { name : Field_name.t;
        if_  : [ `Contains of string ] sexp_option;
      } with sexp
    ;;
  end

  module Listed_header_cond = struct
    type t =
      { name : Field_name.t;
        if_ : [ `Contains of string ] sexp_option;
        remove_duplicates : unit sexp_option;
      } with sexp
    ;;
  end

  type t =
    { strip_whitespace      : unit sexp_option;
      normalize_whitespace  : Header_cond.t sexp_list;
      filter                : Header_cond.t sexp_list;
      mask                  : Header_cond.t sexp_list;
      hash                  : Header_cond.t sexp_list;
      dedup                 : Header_cond.t sexp_list;
      sort_emails           : Listed_header_cond.t sexp_list;
      sort_words            : Listed_header_cond.t sexp_list;
      sort                  : sexp_bool;
    } with sexp
  ;;

  let default =
    { strip_whitespace      = None;
      normalize_whitespace  = [];
      filter                = [];
      mask                  = [];
      hash                  = [];
      dedup                 = [];
      sort_emails           = [];
      sort_words            = [];
      sort                  = false
    }
  ;;

  let load file =
    Reader.load_sexp_exn file t_of_sexp
end

module Header = struct
  type t = (Field_name.t * string) with compare
end

let match_header conds =
  let conds =
    List.fold conds
      ~init:Field_name.Map.empty
      ~f:(fun acc {Config.Header_cond.name; if_} ->
        let cond ~name:other ~value:_ =
          Field_name.equal name other
        in
        let cond =
          match if_ with
          | None -> cond
          | Some (`Contains s) ->
            let re = Re2.Regex.escape s |> Re2.Regex.create_exn in
            fun ~name ~value -> cond ~name ~value && Re2.Regex.matches re value
        in
        Map.add_multi acc ~key:name ~data:cond)
  in
  fun ~name ~value ->
    match Map.find conds name with
    | Some conds -> List.exists conds ~f:(fun cond -> cond ~name ~value)
    | None       -> false
;;

let match_listed_header conds =
  let conds =
    List.fold conds
      ~init:Field_name.Map.empty
      ~f:(fun acc {Config.Listed_header_cond.name; if_; remove_duplicates} ->
        let cond ~name:other ~value:_ =
          Field_name.equal name other
        in
        let cond =
          match if_ with
          | None -> cond
          | Some (`Contains s) ->
            let re = Re2.Regex.escape s |> Re2.Regex.create_exn in
            fun ~name ~value -> cond ~name ~value && Re2.Regex.matches re value
        in
        Map.add_multi acc ~key:name ~data:(cond, remove_duplicates))
  in
  fun ~name ~value ->
    match Map.find conds name with
    | None       -> None
    | Some conds ->
      List.find_map conds ~f:(fun (cond, remove_duplicates) ->
        if cond ~name ~value then Some remove_duplicates else None)
;;

let strip_whitespace_headers =
  Envelope.modify_headers ~f:(List.map ~f:(fun (k,v) ->
    (k, String.strip v)))
;;

let normalize_whitespace s =
  let open Re2 in
  let replace pattern replacement s =
    let regex = Regex.create_exn pattern in
    Regex.replace_exn regex s ~f:(fun _m -> replacement)
  in
  let merge_spaces s     = replace "\\s\\s*" " " s in
  let normalize_commas s = replace "\\s*,\\s*" ", " s in
  merge_spaces s |> normalize_commas
;;

let normalize_whitespace_headers cond =
  let cond = match_header cond in
  Envelope.modify_headers ~f:(List.map ~f:(fun (name, value) ->
    name,
    if cond ~name ~value
    then normalize_whitespace value
    else value))
;;

let filter_headers cond =
  let cond = match_header cond in
  Envelope.modify_headers ~f:(List.filter ~f:(fun (name, value) ->
    not (cond ~name ~value)))
;;

let hash_headers cond =
  let hash data =
    data
    |> Crypto.hash_string (Hash.sha256 ())
    |> Hex.to_hex
    |> sprintf "[hidden : sha256 = %s]"
  in
  let cond = match_header cond in
  Envelope.modify_headers ~f:(List.map ~f:(fun (name, value) ->
      name, if cond ~name ~value then hash value else value))
;;

let mask_headers cond =
  let cond = match_header cond in
  Envelope.modify_headers ~f:(List.map ~f:(fun (name, value) ->
    name, if cond ~name ~value then "XXX" else value))
;;

let sort_emails_in_header pattern =
  let f ~remove_duplicates value =
    let remove_duplicates = Option.is_some remove_duplicates in
    (List.sort value ~cmp:Email_address.compare
     |> (if remove_duplicates
         then List.remove_consecutive_duplicates ~equal:Email_address.equal
         else Fn.id)
     |> List.map ~f:Email_address.to_string)
  in
  Envelope.modify_headers ~f:(List.map ~f:(fun (name, value) ->
    let value =
      match match_listed_header pattern ~name ~value with
      | None                   -> value
      | Some remove_duplicates ->
        match Email_address.list_of_string value with
        | Error e ->
          (* Not an error since this is not a reason to trigger the kill
             switch. *)
          Log.Global.info "could not parse %s: %s"
            value (Error.to_string_hum e);
          value
        | Ok emails ->
          f ~remove_duplicates emails |> String.concat ~sep:", "
    in
    name, value))
;;

let sort_words_in_header pattern =
  let f ~remove_duplicates value =
    let remove_duplicates = Option.is_some remove_duplicates in
      (String.split value ~on:' '
       |> List.filter ~f:(fun s -> not (String.is_empty s))
       |> List.sort ~cmp:String.compare
       |> (if remove_duplicates
           then List.remove_consecutive_duplicates ~equal:String.equal
           else Fn.id)
       |> String.concat ~sep:" ")
  in
  Envelope.modify_headers ~f:(List.map ~f:(fun (name, value) ->
    let value =
      match match_listed_header pattern ~name ~value with
      | None       -> value
      | Some remove_duplicates -> f ~remove_duplicates value
    in
    name, value))
;;

let sort_headers =
  Envelope.modify_headers ~f:(List.stable_sort ~cmp:Header.compare)
;;

let sort_envelope_recipients message =
  let recipients =
    Envelope.recipients message
    |> List.stable_sort ~cmp:Email_address.compare
  in
  Envelope.set message ~recipients ()

let dedup_headers conds =
  let equal (name1, value1) (name2, value2) =
    if match_header conds ~name:name1 ~value:value1
    && match_header conds ~name:name2 ~value:value2
    then Field_name.equal name1 name2
    else false
  in
  Envelope.modify_headers ~f:(List.remove_consecutive_duplicates ~equal)

let transform
      { Config. strip_whitespace;
        normalize_whitespace;
        filter;
        sort;
        dedup;
        hash;
        mask;
        sort_emails;
        sort_words }
      message =
  let maybe_sort message =
    if not sort then message
    else message |> sort_headers |> sort_envelope_recipients
  in
  let message =
    match strip_whitespace with
    | None -> message
    | Some () -> strip_whitespace_headers message
  in
  normalize_whitespace_headers normalize_whitespace message
  |> filter_headers filter
  (* Sorting twice since we want to dedup before masking, to sort before
     deduping, and to sort after masking. *)
  |> maybe_sort
  |> dedup_headers dedup
  |> hash_headers hash
  |> mask_headers mask
  |> sort_emails_in_header sort_emails
  |> sort_words_in_header sort_words
  |> maybe_sort
