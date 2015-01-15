open Core.Std
open Async.Std
open Types
open Email_message.Std

module Glog = Log.Global

open Common

module Smtp_error = struct
  type t =
  [ `Bad_sender of Reply.t
  | `Bad_data of Reply.t
  | `Bad_recipients of (string * Reply.t) list
  | `Other of Reply.t
  ] with sexp

  let bad_sender e = `Bad_sender e
  let bad_data e = `Bad_data e
  let bad_recipients es = `Bad_recipients es
  let other e = `Other e

  let to_string t =
    Sexp.to_string (sexp_of_t t)

  let to_error t =
    Error.of_string (to_string t)
end

module Smtp_result = struct
  type ('a, 'error) t = ('a, 'error) Result.t Or_error.t Deferred.t

  let reject reply =
    Deferred.return (Ok (Error reply))

  let map_error t ~f =
    t >>| fun x ->
    Or_error.map ~f:(Result.map_error ~f) x

  let ok_exn t =
    t >>| fun x ->
    match Or_error.ok_exn x with
    | Ok x -> x
    | Error e -> failwith (Smtp_error.to_string e)

  include Monad.Make2(struct
    type nonrec ('a, 'error) t = ('a, 'error) t
    let return x = Deferred.return (Ok (Ok x))
    let bind x f =
      x >>=? function
      | Ok x -> f x
      | Error result -> Deferred.return (Ok (Error result))
    let map = `Define_using_bind
  end)

  module List : sig
    val filter_map
      :  f:('a -> ('b option, 'err) t)
      -> 'a list
      -> ('b list, 'err) t
  end = struct
    let filter_map ~f xs =
      let rec filter_map acc = function
        | [] -> return (List.rev acc)
        | x :: xs ->
          f x >>= function
          | None   -> filter_map acc xs
          | Some x -> filter_map (x :: acc) xs
      in
      filter_map [] xs
  end
end

let (>>=??) = Smtp_result.bind

module Step : sig
  (* Wait for reply on commands or single dot. Abort on bad replies except for
     recipient. *)
  type t =
  [ `Command of Command.t
  | `Dot
  | `Other of string
  (* Includes neither DATA nor the dot. *)
  | `Body of Bigstring.t
  ]

  (* Includes the dot. *)
  val read_until_dot : Reader.t -> t list Deferred.t

  val add_helo_if_missing : t list -> t list

  val of_envelope : Envelope.t -> t list

  (* (from:%s to:%s) *)
  val summary : t list -> string

  val send_list
    :  ?reader:Reader.t
    -> writer:Writer.t
    -> t list
    -> (unit, Smtp_error.t) Smtp_result.t
end = struct
  type t =
  [ `Command of Command.t
  | `Dot
  | `Other of string
  | `Body of Bigstring.t
  ]

  let summary ts =
    let open Command in
    let senders =
      List.filter_map ts ~f:(function
      | `Command (Sender sender) -> Some sender
      | _ -> None)
    in
    let recipients =
      List.filter_map ts ~f:(function
      | `Command (Recipient recipient) -> Some recipient
      | _ -> None)
    in
    sprintf "(from:%s to:%s)"
      (List.to_string ~f:Fn.id senders)
      (List.to_string ~f:Fn.id recipients)

  let add_helo_if_missing = function
    | (`Command (Command.Helo _) :: _) as ts -> ts
    | ts -> `Command (Command.Helo (Unix.gethostname ())) :: ts

  let read reader =
    Reader.read_line reader
    >>| function
    | `Eof -> `Eof
    | `Ok "." -> `Ok `Dot
    | `Ok line ->
      match Command.of_string_opt line with
      | Some cmd -> `Ok (`Command cmd)
      | None -> `Ok (`Other line)

  let read_until_dot reader =
    let rec read_loop acc =
      read reader
      >>= function
      | `Eof -> return []
      | `Ok `Dot -> return (List.rev (`Dot :: acc))
      | `Ok t -> read_loop (t :: acc)
    in
    read_loop []

  let receive_one reader ~ok =
    let accumulator = Bigbuffer.create 512 in
    let return_resp buf =
      let reply = buf |> Bigbuffer.big_contents |> Reply.of_bigstring in
      if Reply.is_ok reply
      then Smtp_result.return ok
      else Smtp_result.reject reply
    in
    let rec loop () =
      Reader.read_line reader
      >>= function
      | `Eof ->
        Deferred.Or_error.error_string "Got 'Eof' on connection"
      | `Ok reply_line ->
        match String.length reply_line with
        | 3 ->
          Bigbuffer.add_string accumulator reply_line;
          Bigbuffer.add_char accumulator '\n';
          return_resp accumulator
        | n when n > 3 -> begin
          match String.get reply_line 3 with
          | ' ' ->
            Bigbuffer.add_string accumulator reply_line;
            return_resp accumulator
          | '-' ->
            Bigbuffer.add_string accumulator reply_line;
            Bigbuffer.add_char accumulator '\n';
            loop ()
          | (_ : char) ->
            Deferred.Or_error.error_string "Unexpected character in reply"
        end
        | (_ : int) ->
          Deferred.Or_error.error_string "Reply unexpectedly short"
    in
    loop ()
  ;;

  let receive_one ?reader ~ok =
    match reader with
    | None -> Smtp_result.return ok
    | Some reader -> receive_one reader ~ok

  let send_body ~writer ~body =
    let rec loop ~pos =
      if pos < Bigstring.length body then begin
        if Bigstring.get body pos |> Char.equal '.' then Writer.write writer ".";
        match Bigstring.find '\n' ~pos body with
        | Some offs ->
          Writer.write_bigstring writer body ~pos ~len:(offs+1-pos);
          loop ~pos:(offs+1)
        | None ->
          Writer.write_bigstring writer body ~pos
      end
    in
    loop ~pos:0;
    Writer.write writer "\n";
    Writer.flushed writer

  type recipient_error = string * Reply.t

  let send_one ?reader ~writer (t : t)
      : (recipient_error option, Smtp_error.t) Smtp_result.t =
    match t with
    | `Body body ->
      send_body ~writer ~body
      >>= fun () ->
      Smtp_result.return None
    | (`Command _ | `Other _ | `Dot) as t ->
      let open Command in
      begin match t with
      | `Command (Helo _) ->
        begin
          receive_one ?reader ~ok:()
          |> Smtp_result.map_error ~f:Smtp_error.other
        end
      | _ -> Smtp_result.return ()
      end
      >>=?? fun () ->
      let line =
        match t with
        | `Command cmd -> Command.to_string cmd
        | `Dot -> "."
        | `Other s -> s
      in
      Writer.write writer line;
      Writer.write writer "\n";
      Writer.flushed writer
      >>= fun () ->
      match t with
      | `Other _ -> Smtp_result.return None
      | `Command (Helo _ | Data | Quit | Help | Noop) ->
        begin
          receive_one ?reader ~ok:None
          |> Smtp_result.map_error ~f:Smtp_error.other
        end
      | `Command (Sender _) ->
        begin
          receive_one ?reader ~ok:None
          |> Smtp_result.map_error ~f:Smtp_error.bad_sender
        end
      | `Dot ->
        begin
          receive_one ?reader ~ok:None
          |> Smtp_result.map_error ~f:Smtp_error.bad_data
        end
      | `Command (Recipient recipient) ->
        begin
          receive_one ?reader ~ok:()
          >>=? function
          | Ok () ->
            Smtp_result.return None
          | Error reply ->
            Smtp_result.return (Some (recipient, reply))
        end
  ;;

  let send_list ?reader ~writer ts =
    Smtp_result.List.filter_map ts ~f:(send_one ?reader ~writer)
    >>=?? function
    | [] ->
      Smtp_result.return ()
    | bad_recipients ->
      return (Ok (Error (Smtp_error.bad_recipients bad_recipients)))

  let of_envelope message =
    let open Command in
    let sender = `Command (Sender (Envelope.string_sender message)) in
    let recipients = List.map (Envelope.string_recipients message) ~f:(fun r ->
      `Command (Recipient r))
    in
    let body = `Body (Envelope.email message |> Email.to_bigstring) in
    sender :: recipients @ [`Command Data; body; `Dot]
end

let write writer ?helo pipe =
  let open Command in
  let rec next () =
    Pipe.read pipe
    >>= function
    | `Eof -> Smtp_result.return ()
    | `Ok message ->
      Step.send_list ~writer (Step.of_envelope message)
      >>=?? fun () ->
      next ()
  in
  begin match helo with
  | None ->
    Smtp_result.return ()
  | Some name ->
    Step.send_list ~writer [`Command (Helo name)]
  end
  >>=?? fun () ->
  next ()

let write writer ?helo pipe =
  write writer ?helo pipe
  >>| function
  | Ok (Ok ()) -> Ok ()
  | Error e -> Error e
  (* There will be no SMTP replies when we are writing to a file. *)
  | Ok (Error _) -> assert false

let send destination message =
  let open Command in
  mlog ~severity:`Info ~dir:`Out ~message "(from:%s to:%s) at %s"
    (Envelope.string_sender message)
    (List.to_string ~f:Email_address.to_string (Envelope.recipients message))
    (Host_and_port.to_string destination);
  let destination =
    Tcp.to_host_and_port
      (Host_and_port.host destination)
      (Host_and_port.port destination)
  in
  Monitor.try_with (fun () ->
    Tcp.with_connection destination
      (fun _socket reader writer ->
        let steps =
          Step.of_envelope message |> Step.add_helo_if_missing
        in
        Step.send_list ~reader ~writer steps
        >>=?? fun () ->
        Step.send_list ~reader ~writer [`Command Quit]))
  >>| function
  | Ok or_error -> or_error
  | Error exn   -> Or_error.of_exn exn
;;

let send_raw destination file_reader =
  let destination =
    Tcp.to_host_and_port
      (Host_and_port.host destination)
      (Host_and_port.port destination)
  in
  Monitor.try_with (fun () ->
    Tcp.with_connection destination
      (fun _socket reader writer ->
        Step.read_until_dot file_reader
        >>= fun steps ->
        match List.last steps with
        | None ->
          Smtp_result.return `Eof
        | Some `Dot ->
          let steps = Step.add_helo_if_missing steps in
          Step.send_list ~reader ~writer steps
          >>=?? fun () ->
          Step.send_list ~reader ~writer [`Command Quit]
          >>=?? fun () ->
          Smtp_result.return (`Ok_summary (Step.summary steps))
        | Some _ ->
          Deferred.Or_error.error_string "End of file before single dot."))
  >>| function
  | Ok or_error -> or_error
  | Error exn   -> Or_error.of_exn exn
;;
