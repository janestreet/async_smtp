open Core.Std
open Async.Std
open Async_smtp.Std

module Config = struct
  type t =
    { content_filter_in  : Address.t
    ; smtp_server_config   : Smtp_server.Config.t
    } [@@deriving sexp, fields]
end

type t = Config.t * Log.t

module Id = struct
  include Unique_id.Int ()

  let header_name = "X-JS-Content-Filter-Id"

  let tag t envelope =
    Smtp_envelope.add_header envelope ~name:header_name ~value:(to_string t)

  let untag_exn envelope =
    let id = Set_once.create () in
    let envelope' = Smtp_envelope.filter_headers envelope ~f:(fun ~name ~value ->
      if String.equal name header_name then begin
        Set_once.set_exn id value;
        false
      end else
        true)
    in
    Set_once.get_exn id, envelope'
end

(* The messages we have written to the external content filter and haven't heard back
   yet. *)
let messages : Smtp_envelope.t Ivar.t Id.Table.t = Id.Table.create ()

module Callbacks = struct
  include Smtp_server.Callbacks.Simple

  let process_envelope ~log:_ ~session:_ envelope =
    let id, envelope' = Id.untag_exn envelope in
    begin
      match Hashtbl.find messages (Id.of_string id) with
      | None -> ()
      | Some ivar -> Ivar.fill ivar envelope'
    end;
    return (`Consume "Message consumed")
end

let initialize_exn config ~log =
  Smtp_server.start
    ~log
    ~config:(Config.smtp_server_config config)
    (module Callbacks : Smtp_server.Callbacks.S)
  >>| Or_error.ok_exn
  >>| fun server ->
  Shutdown.at_shutdown (fun () ->
    Smtp_server.close server
    >>| Or_error.ok_exn);
  config, log

let send (config, log) ?(timeout=sec 10.) envelope =
  (* Give each message a unique ID so we know when we get it back *)
  let id = Id.create () in
  Smtp_client.Tcp.with_ ~log ~config:Smtp_client.Config.default
    (Config.content_filter_in config)
    ~f:(fun client ->
      Hashtbl.add_exn messages ~key:id ~data:(Ivar.create ());
      let envelope' = Id.tag id envelope in
      Smtp_client.send_envelope ~log client envelope'
      >>=? function
      | Ok _ -> Deferred.Or_error.ok_unit
      | (Error _) as status ->
        Deferred.Or_error.errorf !"%{Smtp_client.Envelope_status}" status)
  >>= function
  | Error e -> return (Error e)
  | Ok () ->
    Clock.with_timeout timeout (Ivar.read (Hashtbl.find_exn messages id))
    >>| function
    | `Timeout ->
      Hashtbl.remove messages id;
      Error (Error.of_string "Timed out hearing back from relay")
    | `Result envelope' ->
      (* Be careful not to change any fields of the envelope except the email. *)
      Ok (Smtp_envelope.set envelope ~email:(Smtp_envelope.email envelope') ())
