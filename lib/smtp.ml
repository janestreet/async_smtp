open Core.Std
open Async.Std
module Octet_stream = Email_message.Octet_stream

type email_id = string
type smtp_email = string * string list * email_id * Email_message.Email.t
type rewriting_rule = smtp_email -> smtp_email list option
type routing_rule = smtp_email -> (string * int) list option

module Commands = struct
  include Comm
  let commands = ["HELO";"MAIL";"FROM";"RCPT";"TO";"DATA";"QUIT";"HELP";"NOOP"]

  let of_string_opt str =
    try Some
      (match
        Lexer.parse_command (Octet_stream.to_lexbuf (Octet_stream.of_string str))
       with
        | Hello s -> Hello (String.lstrip s)
        | Sender s -> Sender (String.lstrip s)
        | Receiver s -> Receiver (String.lstrip s)
        | x -> x)
    with _ -> None

  let to_string = function
  | Hello string -> "HELO " ^ string
  | Sender string -> "MAIL FROM: " ^ string
  | Receiver string -> "RCPT TO: " ^ string
  | Data -> "DATA"
  | Quit -> "QUIT"
  | Help -> "HELP"
  | Noop -> "NOOP"
end

module Replies = struct
  type ok =
  | System_status
  | Help
  | Service_ready
  | Closing_connection
  | Ok_completed
  | Will_forward
  | Will_attempt
  | Start_mail_input

  type not_ok =
  | Not_available
  | Mailbox_unavailable_400
  | Local_error
  | Insufficient_storage
  | Unable_to_accommodate

  type never_ok =
  | Command_not_recognized
  | Syntax_error
  | Command_not_implemented
  | Bad_sequence_of_commands
  | Parameter_not_implemented
  | Mailbox_unavailable_500
  | User_not_local
  | Exceeded_storage_allocation
  | Mailbox_name_not_allowed
  | Trasaction_failed
  | From_to_parameters_bad

  type reply = Ok of ok | Bad of not_ok | Really_bad of never_ok
  type t = reply * string
  exception Unknown_response_code of int
  exception Unexpected_character of char
  let my_name = Unix.gethostname ()

  let to_string = fun (reply,msg) -> match reply with
  | Ok r -> begin
    match r with
    | System_status -> "211"
    | Help ->
      "214-Commands supported:\n214 "
      ^ (String.concat ~sep:" " Commands.commands)
    | Service_ready ->
      "220 "^ my_name ^ " SMTP JS_SMTP v0.001 "
      ^ (Time.to_string_abs (Time.now ()))
    | Closing_connection -> "221 " ^ my_name ^ " closing connection"
    | Ok_completed -> "250 Ok: " ^ msg
    | Will_forward -> "251"
    | Will_attempt -> "252"
    | Start_mail_input -> "354 Enter message, ending with \".\" on a line by itself"
    end
  | Bad r -> begin
    match r with
    | Not_available -> "421"
    | Mailbox_unavailable_400 -> "450"
    | Local_error -> "451"
    | Insufficient_storage -> "452"
    | Unable_to_accommodate -> "455"
    end
  | Really_bad r -> begin
    match r with
    | Command_not_recognized -> "500 unrecognized command"
    | Syntax_error -> "501 Syntactically invalid command"
    | Command_not_implemented -> "502 Command not implemented"
    | Bad_sequence_of_commands -> "503 sender already given"
    | Parameter_not_implemented -> "504 Command parameter not implemented"
    | Mailbox_unavailable_500 -> "550 Mailbox unavailable"
    | User_not_local -> "551 User not local"
    | Exceeded_storage_allocation -> "552 Exceeded storage allocation"
    | Mailbox_name_not_allowed -> "553 Mailbox name not allowed"
    | Trasaction_failed -> "554 Trasaction failed"
    | From_to_parameters_bad -> "555"
    end

  let is_last_line str =
    if Polymorphic_compare.equal 3 (String.length str) then true
    else match String.get str 3 with
    | ' ' -> true
    | '-' -> false
    | c -> raise (Unexpected_character c)

  let of_string str =
    ( begin
      match Int.of_string (String.sub ~pos:0 ~len:3 str) with
      | 211 -> Ok System_status
      | 214 -> Ok Help
      | 220 -> Ok Service_ready
      | 221 -> Ok Closing_connection
      | 250 -> Ok Ok_completed
      | 251 -> Ok Will_forward
      | 252 -> Ok Will_attempt
      | 354 -> Ok Start_mail_input

      | 421 -> Bad Not_available
      | 450 -> Bad Mailbox_unavailable_400
      | 451 -> Bad Local_error
      | 452 -> Bad Insufficient_storage
      | 455 -> Bad Unable_to_accommodate

      | 500 -> Really_bad Command_not_recognized
      | 501 -> Really_bad Syntax_error
      | 502 -> Really_bad Command_not_implemented
      | 503 -> Really_bad Bad_sequence_of_commands
      | 504 -> Really_bad Parameter_not_implemented
      | 550 -> Really_bad Mailbox_unavailable_500
      | 551 -> Really_bad User_not_local
      | 552 -> Really_bad Exceeded_storage_allocation
      | 553 -> Really_bad Mailbox_name_not_allowed
      | 554 -> Really_bad Trasaction_failed
      | 555 -> Really_bad From_to_parameters_bad
      | x -> raise (Unknown_response_code x)
      end
    , str)
end

let exchange reader writer = fun message ->
    Writer.write writer (message ^ "\n");
    Writer.flushed writer >>= fun () ->
    Log.Global.debug "I SAID: %s" message;
    Reader.read_line reader >>= fun s ->
    let response = match s with `Eof -> "EOF" | `Ok a -> a in
    Log.Global.debug "THEY SAID: %s" response;
    return response

module Client = struct
  let rec read_whole_reply reader (strs:string list) : string list Deferred.t =
    match strs with
    | [] -> begin
      Reader.read_line reader >>= function
      | `Eof -> return []
      | `Ok s -> read_whole_reply reader [s]
      end
    | (str::_) as strs ->
      if Replies.is_last_line str then return (List.rev strs)
      else Reader.read_line reader >>= function
      | `Eof -> return (List.rev strs)
      | `Ok s -> read_whole_reply reader (s::strs)

  let is_ok (r:string Deferred.t): unit option Deferred.t = r >>= fun r ->
    match Replies.of_string r with
    | (Replies.Bad _, _ )
    | (Replies.Really_bad _, _) -> return None
    | (Replies.Ok  _, _) -> return (Some ())

  let exchange reader writer message =
    exchange reader writer message >>= fun response ->
    if Replies.is_last_line response
    then return response
    else
      read_whole_reply reader [response] >>= fun strs ->
      return (String.concat strs ~sep:"\n")

  let bind_is_ok t f =
    Deferred.bind (is_ok t) (function
    | Some x -> f x
    | None -> Deferred.return None)

  let send_email reader writer ~from:sender ~to_:receivers message =
    let exchange_str = exchange reader writer in
    let exchange command = exchange_str (Commands.to_string command) in
    Reader.read_line reader >>= function
    | `Eof -> return false
    | `Ok resp ->
      Log.Global.debug "THEY SAID: %s" resp;
      let result =
        bind_is_ok (return resp)
        (fun () -> bind_is_ok (exchange (Commands.Hello (Unix.gethostname ())))
        (fun () -> bind_is_ok (exchange (Commands.Sender sender))
        (fun () -> bind_is_ok (exchange
          (Commands.Receiver (String.concat ~sep:" " receivers)))
        (fun () -> bind_is_ok (exchange Commands.Data)
        (fun () -> bind_is_ok (exchange_str (message ^ "\n."))
        (fun () -> bind_is_ok (exchange Commands.Quit)
          (fun s -> return (Some s))))))))
      in result >>= function
      | None -> return false
      | Some _ -> return true
end

module Server = struct
  let rec read_data buffer reader =
    Reader.read_line reader >>= fun line ->
    match line with
    | `Eof -> return ()
    | `Ok "." -> return ()
    | `Ok s ->
      Bigbuffer.add_string buffer s;
      Bigbuffer.add_char buffer '\n';
      read_data buffer reader >>= fun () -> return ()

  let make_id sender receivers _email =
    let now = Time.hash (Time.now ()) in
    let send = String.hash sender in
    let receivs = String.hash (List.to_string ~f:String.to_string receivers) in
    (Int.to_string now) ^ (Int.to_string send) ^ (Int.to_string receivs)

  let rec speak_smtp partner_name reader writer =
  fun first_message ~sender ~receivers ~email ->
    let exchange reply = exchange reader writer (Replies.to_string reply) in
    let speak_smtp = speak_smtp partner_name reader writer in
    let recurse = speak_smtp ~sender ~receivers ~email in
    exchange first_message >>= fun command_string ->
    match Commands.of_string_opt command_string with
    | None -> recurse Replies.(Really_bad Command_not_recognized,"")
    | Some x -> begin
      match x with
      | Commands.Hello _name -> recurse Replies.(Ok Ok_completed, partner_name)
      | Commands.Sender email_addr -> begin
        match sender with
        | None -> speak_smtp
          ~sender:(Some email_addr) ~receivers ~email Replies.(Ok Ok_completed,"")
        | Some _ -> recurse Replies.(Really_bad Bad_sequence_of_commands,"")
        end
      | Commands.Receiver email_addr -> speak_smtp
        ~sender ~receivers:(email_addr::receivers) ~email Replies.(Ok Ok_completed,"")
      | Commands.Data -> begin
        match email with
        | Some _ -> recurse Replies.(Really_bad Bad_sequence_of_commands,"")
        | None ->
          match (sender,receivers) with
          | (None,[])
          | (Some _, [])
          | (None, _) -> recurse Replies.(Really_bad Bad_sequence_of_commands,"")
          | (Some send, receiv) ->
            exchange Replies.(Ok Start_mail_input,"") >>= fun first_line ->
            let buffer = Bigbuffer.create (String.length first_line) in
            Bigbuffer.add_string buffer first_line;
            Bigbuffer.add_char buffer '\n';
            read_data buffer reader >>= fun () ->
            let email_msg =
              Email_message.Email.of_octet_stream
                (Octet_stream.of_bigstring (Bigbuffer.volatile_contents buffer))
            in
            let id_ = make_id send receiv email_msg in
            speak_smtp
              ~sender ~receivers
              ~email:(Some (id_,email_msg))
              Replies.(Ok Ok_completed, id_)
        end
      | Commands.Quit ->
        exchange Replies.(Ok Closing_connection,"") >>= fun _ ->
        return (sender,receivers,email)
      | Commands.Help -> recurse Replies.(Ok Help,"")
      | Commands.Noop -> recurse Replies.(Ok Ok_completed,"")
      end

  let start_connection partner_name reader writer =
    speak_smtp
      partner_name reader writer
      Replies.(Ok Service_ready,"")
      ~sender:None
      ~receivers:[]
      ~email:None
    >>| fun (from,to_,imsg) ->
    match from with
    | None -> None
    | Some sender_addr ->
      match to_ with
      | [] -> None
      | receiver_addrs ->
        match imsg with
        | None -> None
        | Some (id_,email) -> Some
          ( sender_addr
          , receiver_addrs
          , id_
          , email)

end

module Router = struct
  let __UNUSED_VALUE__make_id sender receivers _email =
    let now = Time.hash (Time.now ()) in
    let send = String.hash sender in
    let receivs = String.hash (List.to_string ~f:String.to_string receivers) in
    (Int.to_string now) ^ (Int.to_string send) ^ (Int.to_string receivs)

  let send_email ~host ~port ~from:sender ~to_:receivers ~id_ message =
    Log.Global.info "%s => (from:%s to:%s) at %s:%d"
      id_
      sender
      (List.to_string ~f:String.to_string receivers)
      host port;
    let destination = Tcp.to_host_and_port host port in
    Tcp.with_connection destination (fun _socket reader writer ->
      Client.send_email reader writer ~from:sender ~to_:receivers message)

  let rewrite rules msg: smtp_email list =
    Array.fold rules ~init:[msg] ~f:(fun msgs rule ->
      List.concat (List.map msgs
          ~f:(fun msg -> match rule msg with
              | None -> [msg]
              | Some msgs -> msgs)))

  let route (rules: routing_rule array) msg: (string * int) list =
    Array.fold rules ~init:[]
      ~f:(fun dests rule ->
        match rule msg with
        | None -> dests
        | Some dsts -> List.concat [dests;dsts])

  let rules_server rewriting_rules routing_rules =
    let rewriting_array = Array.of_list rewriting_rules in
    let routing_array = Array.of_list routing_rules in
    let handler address reader writer =
      let partner =
      begin match address with
      | `Inet (inet, _) -> (Unix.Inet_addr.to_string inet)
      | `Unix s -> s
      end
      in
      Log.Global.info "Connection from %s" partner;
      Server.start_connection partner reader writer >>= function
      | None ->
        return (Log.Global.error "This shouldn't happen: no email (smtp server)")
      | Some ((_,_,id_,_) as msg) ->
        Log.Global.info "%s <= from %s" id_ partner;
        Deferred.all (List.concat
        (List.map (rewrite rewriting_array msg)
          ~f:(fun msg ->
              let from, to_, id_, email = msg in
              List.map (route routing_array msg)
                ~f:(fun (host,port) ->
                send_email
                  ~host ~port
                  ~from ~to_ ~id_
                  (Email_message.Email.to_string email)
                >>| function
                | true  -> Log.Global.info "sent email to %s:%d" host port
                | false -> Log.Global.error "failed to send email to %s:%d" host port))))
        >>| fun _ -> ()
    in
    (fun s r w -> (handler s r w) >>= fun _ -> return ())
end
