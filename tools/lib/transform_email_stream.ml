open Core.Std
open Async.Std
open Email_message.Std
open Async_smtp.Std

(* We use this tool for testing Async_smtp.Server, but we use the server itself
   to read in the test outputs. This clearly has the potential to mask bugs. To
   deal with this we check in our test setup that reading and writing the exim
   output without any sorting or filtering has no effect. *)

module Envelope = Smtp_envelope
module Sender_address = Smtp_sender

module Crypto = Cryptokit
module Hash = Crypto.Hash

module Envelopes = struct
  type t =
    { sort : [ `Envelope_id | `Sender | `Recipients | `Subject | `Body | `Headers ] sexp_list;
    } with sexp
  ;;

  let default =
    { sort = [];
    }
  ;;
end
;;

module Bodies = struct
  type t =
    { mask_if_contains : string sexp_option
    ; hash : [ `whole | `parts ] sexp_option
    } with sexp

  let default = { mask_if_contains = None; hash = None; }

  let mask_body t =
    match t.mask_if_contains with
    | None -> ident
    | Some s ->
      let re = Re2.Regex.escape s |> Re2.Regex.create_exn in
      Envelope.modify_email ~f:fun email ->
        if not (Re2.Regex.matches re (Email.to_string email)) then email
        else begin
          let headers = (Email.headers email :> (string * string) list) in
          Email.Simple.create ~headers ~body:"" |> Or_error.ok_exn
        end

  let hash_fun data =
    data
    |> Crypto.hash_string (Hash.sha256 ())
    |> Hex.to_hex

  let hash_body t =
    match t.hash with
    | None -> ident
    | Some `parts ->
      let hash_data octet_stream =
        let module Encoding = Octet_stream.Encoding in
        let encoding = Octet_stream.encoding octet_stream in
        octet_stream
        |> Octet_stream.to_string
        |> hash_fun
        |> sprintf
             !"\nPART HIDDEN.\nORIGINAL_ENCODING:%{sexp:Encoding.t}\nHASH:%s\n"
             encoding
        |> Octet_stream.of_string
      in
      Envelope.modify_email ~f:fun email ->
        Email.map_data email ~f:hash_data
    | Some `whole ->
      let hash_body email =
        email
        |> Email.to_string
        |> hash_fun
        |> sprintf "\nBODY HIDDEN.\nHASH=%s\n"
      in
      Envelope.modify_email ~f:fun email ->
        let headers = (Email.headers email :> (string * string) list) in
        let body = hash_body email in
        Email.Simple.create ~headers ~body |> Or_error.ok_exn

  let transform t =
    let mask_body = mask_body t in
    let hash_body = hash_body t in
    fun message -> message |> mask_body |> hash_body
end

module Config = struct
  type t =
    { headers  : Headers.Config.t with default( Headers.Config.default );
      bodies   : Bodies.t with default ( Bodies.default );
      messages : Envelopes.t with default( Envelopes.default );
    } with sexp
  ;;

  let default =
    { headers  = Headers.Config.default;
      bodies   = Bodies.default;
      messages = Envelopes.default;
    }

  let load file =
    Reader.load_sexp_exn file t_of_sexp
end
;;

module Compare = struct
  let map cmp ~f = fun a b -> cmp (f a) (f b)
  ;;

  let seq = Comparable.lexicographic
end

let compare_message_id =
  Compare.map ~f:Envelope.id Envelope.Id.compare
;;

let compare_message_sender =
  Compare.map ~f:Envelope.sender Sender_address.compare
;;

let compare_message_recipients =
  Compare.map ~f:Envelope.recipients (List.compare ~cmp:Email_address.compare)
;;

let compare_message_subject =
  Compare.map ~f:(Envelope.get_headers ~name:"Subject") (List.compare ~cmp:String.compare)
;;

let compare_message_body =
  let f envelope =
    Envelope.email envelope |> Email.raw_content
  in
  Compare.map ~f Bigstring_shared.compare
;;

let compare_message_headers =
  List.compare ~cmp:Headers.Header.compare
  |> Compare.map ~f:Email.headers
  |> Compare.map ~f:Envelope.email
;;

let compare_message_by = function
  | `Envelope_id -> compare_message_id
  | `Sender      -> compare_message_sender
  | `Recipients  -> compare_message_recipients
  | `Subject     -> compare_message_subject
  | `Body        -> compare_message_body
  | `Headers     -> compare_message_headers
;;

let compare_message seq =
  List.map seq ~f:compare_message_by
  |> Compare.seq
;;

let transform_without_sort config message =
  message
  |> Headers.transform config.Config.headers
  |> Bodies.transform config.Config.bodies

let sort config pipe =
    match config.Config.messages.Envelopes.sort with
    | [] -> pipe
    | order ->
      Pipe.init (fun out ->
        Pipe.to_list pipe
        >>| List.stable_sort ~cmp:(compare_message order)
        >>= Deferred.List.iter ~f:(Pipe.write out)
      )
