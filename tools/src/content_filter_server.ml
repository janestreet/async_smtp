open Core
open Async
open Async_smtp

module Log = Smtp_mail_log

(* [t] is mostly used as proof that the server was started *)
type t = Log.t

(* The messages we have written out but haven't gotten a response yet. *)
let messages : Smtp_envelope.t Ivar.t Uuid.Table.t = Uuid.Table.create ()

module Id = struct
  let header_name = "X-JS-Tmp-Id"

  let tag id envelope =
    Smtp_envelope.add_header envelope ~name:header_name ~value:(Uuid.to_string id)

  let untag_exn envelope =
    (* There could be multiple ids if this module is nested. Make sure to only delete the
       one we added. *)
    let id_ref = Set_once.create () in
    let envelope' = Smtp_envelope.filter_headers envelope ~f:(fun ~name ~value ->
      if String.equal name header_name
      && Hashtbl.mem messages (Uuid.of_string value) then begin
        Set_once.set_exn id_ref [%here] (Uuid.of_string value);
        false
      end
      else
        true)
    in
    Set_once.get id_ref, envelope'
end

module Server = Smtp_server.Make(struct
    open Smtp_monad.Let_syntax
    module Session = Smtp_server.Plugin.Simple.Session
    module Envelope = struct
      include Smtp_server.Plugin.Simple.Envelope
      let process ~log:_ _session t email =
        let envelope = smtp_envelope t email in
        let id, envelope' = Id.untag_exn envelope in
        Option.iter id ~f:(fun id ->
          match Hashtbl.find messages id with
          | None -> ()
          | Some ivar -> Ivar.fill ivar envelope');
        return (`Consume "Message consumed")
    end
    let rpcs () = []
  end)

let start_exn config ~log =
  Server.start
    ~log
    ~config
  >>| Or_error.ok_exn
  >>| fun _server -> log

let send_receive log ?(timeout=sec 10.) address envelope =
  let component = ["content_filter_server";"send_receive"] in
  (* Give each message a unique ID so we know when we get it back *)
  let id = Uuid.create () in
  Hashtbl.add_exn messages ~key:id ~data:(Ivar.create ());
  Smtp_client.Tcp.with_ ~log ~config:Smtp_client.Config.default
    address
    ~f:(fun client ->
      let envelope' = Id.tag id envelope in
      Smtp_client.send_envelope ~log client envelope'
      >>=? function
      | Ok _ -> Deferred.Or_error.ok_unit
      | (Error _) as status ->
        Deferred.Or_error.errorf !"%{Smtp_client.Envelope_status}" status)
  >>= function
  | Error e ->
    Hashtbl.remove messages id;
    Log.info log (lazy (Log.Message.of_error
                          ~here:[%here]
                          ~flows:Log.Flows.none
                          ~component
                          (Error.tag e ~tag:"error sending")));
    return (Error e)
  | Ok () ->
    Clock.with_timeout timeout (Ivar.read (Hashtbl.find_exn messages id))
    >>| function
    | `Timeout ->
      Hashtbl.remove messages id;
      let error = Error.of_string "Timed out hearing back from relay" in
      Log.info log (lazy (Log.Message.of_error
                            ~here:[%here]
                            ~flows:Log.Flows.none
                            ~component
                            error));
      Error error
    | `Result envelope' ->
      Hashtbl.remove messages id;
      (* Be careful not to change any fields of the envelope except the email. *)
      Ok (Smtp_envelope.set envelope ~email:(Smtp_envelope.email envelope') ())
