open Core
open Async
open Async_smtp_types
open Async_smtp

(* We use this tool for testing Async_smtp.Server, but we use the server itself to read in
   the test outputs. This clearly has the potential to mask bugs. To deal with this we
   check in our test setup that reading and writing the exim output without any sorting or
   filtering has no effect. *)

module Envelope = Smtp_envelope
module Sender_address = Envelope.Sender
module Crypto = Crypto.Cryptokit
module Hash = Crypto.Hash

module Envelopes = struct
  type t =
    { sort : [ `Envelope_id | `Sender | `Recipients | `Subject | `Body | `Headers ] list
         [@sexp.list]
    }
  [@@deriving sexp]

  let default = { sort = [] }
end

module Bodies = struct
  module Rewrite = struct
    type t =
      Re2.Stable.V1_no_options.t
      * [ `Rewrite_entire_body_to of string | `Rewrite_all_matches_to of string ]
    [@@deriving sexp]
  end

  type t =
    { rewrites : Rewrite.t list [@sexp.list]
    ; hash : [ `whole | `parts ] option [@sexp.option]
    }
  [@@deriving sexp]

  let default = { rewrites = []; hash = None }

  let mask_body t =
    match t.rewrites with
    | [] -> Fn.id
    | _ ->
      Envelope.modify_email ~f:(fun email ->
        let content_str =
          Email.raw_content email
          |> Email.Raw_content.to_bigstring_shared
          |> Bigstring_shared.to_string
        in
        let rewritten_content_str =
          List.fold t.rewrites ~init:content_str ~f:(fun content_str (re, rewrite_to) ->
            if Re2.matches re content_str
            then (
              match rewrite_to with
              | `Rewrite_entire_body_to content_str -> content_str
              | `Rewrite_all_matches_to template ->
                Re2.rewrite_exn re ~template content_str)
            else content_str)
        in
        if String.equal content_str rewritten_content_str
        then email
        else (
          let headers = Email.headers email in
          let raw_content = Email.Raw_content.of_string rewritten_content_str in
          Email.create ~headers ~raw_content))
  ;;

  let hash_fun data = data |> Crypto.hash_string (Hash.sha256 ()) |> Util.Hex.to_hex

  let hash_body t =
    match t.hash with
    | None -> Fn.id
    | Some `parts ->
      let hash_data octet_stream =
        let module Encoding = Octet_stream.Encoding in
        let encoding = Octet_stream.encoding octet_stream in
        octet_stream
        |> Octet_stream.encoded_contents_string
        |> hash_fun
        |> sprintf
             !"\nPART HIDDEN.\nORIGINAL_ENCODING:%{sexp:Encoding.t}\nHASH:%s\n"
             encoding
        |> Octet_stream.of_string ~encoding:Octet_stream.Encoding.default'
      in
      Envelope.modify_email ~f:(fun email ->
        (* This tool is used to hide the message body to optimize the message comparison.
           In the event of a parse error preserving the original content is more useful
           then raising. *)
        Email.Content.map_data ~on_unparsable_content:`Skip email ~f:hash_data)
    | Some `whole ->
      let hash_body email =
        email |> Email.to_string |> hash_fun |> sprintf "\nBODY HIDDEN.\nHASH=%s\n"
      in
      Envelope.modify_email ~f:(fun email ->
        let headers = Email.headers email in
        let body = hash_body email in
        let email =
          Email.Simple.Expert.content
            ~normalize_headers:`None
            ~encoding:`Quoted_printable
            ~extra_headers:[]
            body
        in
        Email.set_headers
          email
          (List.fold
             ~init:headers
             (Email.headers email |> Email_headers.to_list ~normalize:`None)
             ~f:(fun headers (name, value) ->
               Email_headers.set ~normalize:`None headers ~name ~value)))
  ;;

  let transform t =
    let mask_body = mask_body t in
    let hash_body = hash_body t in
    fun message -> message |> mask_body |> hash_body
  ;;
end

module Config = struct
  type t =
    { headers : Headers.Config.t [@default Headers.Config.default]
    ; bodies : Bodies.t [@default Bodies.default]
    ; messages : Envelopes.t [@default Envelopes.default]
    }
  [@@deriving sexp]

  let default =
    { headers = Headers.Config.default
    ; bodies = Bodies.default
    ; messages = Envelopes.default
    }
  ;;

  let load file = Reader.load_sexp_exn file t_of_sexp
end

module Compare = struct
  let map cmp ~f a b = cmp (f a) (f b)
  let seq = Comparable.lexicographic
end

let compare_message_id = Compare.map ~f:Envelope.id Envelope.Id.compare
let compare_message_sender = Compare.map ~f:Envelope.sender Sender_address.compare

let compare_message_recipients =
  Compare.map ~f:Envelope.recipients (List.compare Email_address.compare)
;;

let compare_message_subject =
  Compare.map
    ~f:(fun e -> Envelope.find_all_headers e "Subject")
    (List.compare String.compare)
;;

let compare_message_body =
  let f envelope = Envelope.email envelope |> Email.raw_content in
  Compare.map ~f Email.Raw_content.compare
;;

let compare_message_headers =
  List.compare Headers.Header.compare
  |> Compare.map ~f:(Email_headers.to_list ~normalize:`None)
  |> Compare.map ~f:Email.headers
  |> Compare.map ~f:Envelope.email
;;

let compare_message_by = function
  | `Envelope_id -> compare_message_id
  | `Sender -> compare_message_sender
  | `Recipients -> compare_message_recipients
  | `Subject -> compare_message_subject
  | `Body -> compare_message_body
  | `Headers -> compare_message_headers
;;

let compare_message seq x y = Compare.seq (List.map seq ~f:compare_message_by) x y

let transform_without_sort config message =
  message
  |> Headers.transform config.Config.headers
  |> Bodies.transform config.Config.bodies
;;

let sort config pipe =
  match config.Config.messages.Envelopes.sort with
  | [] -> pipe
  | order ->
    Pipe.create_reader ~close_on_exception:true (fun out ->
      Pipe.to_list pipe
      >>| List.stable_sort ~compare:(compare_message order)
      >>= Deferred.List.iter ~how:`Sequential ~f:(Pipe.write out))
;;
