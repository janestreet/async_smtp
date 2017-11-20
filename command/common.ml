open Core
open Async
open Async_smtp

module Command : sig
  include module type of Command
  val rpc
    :  summary:string
    -> ?readme:(unit -> string)
    -> ('main, (Rpc.Connection.t -> unit Deferred.t)) Spec.t
    -> 'main
    -> t
  val config_or_rpc
    :  summary:string
    -> ?readme:(unit -> string)
    -> ('main, ([`Config of Smtp_server.Config.t | `Rpc of Rpc.Connection.t] -> unit Deferred.t)) Spec.t
    -> 'main
    -> t
end = struct
  include Command

  let rpc_client_command0 ~host_and_port ~f =
    Rpc.Connection.with_client (Tcp.Where_to_connect.of_host_and_port host_and_port)
      (fun conn -> f conn)
    >>| function
    | Ok a      -> a
    | Error exn -> raise exn
  ;;

  let rpc_server_or_config_flag () =
    let open Command.Spec in
    step (fun m rpc_server config_path ->
      let config =
        Option.map config_path
          ~f:(fun c ->
            Thread_safe.block_on_async_exn
              (fun () -> Smtp_server.Config.load_exn c))
      in
      let rpc_server =
        match rpc_server, config with
        | Some v, (_ : Smtp_server.Config.t option) -> v
        | None, None -> failwith "at least one of -rpc-server or -config required"
        | None, Some config ->
          let port = Smtp_server.Config.rpc_port config in
          Host_and_port.create ~host:"localhost" ~port
      in
      m ~rpc_server ~config)
    +> flag "rpc-server" (optional host_and_port)
         ~doc:"HOST:PORT mailcore instance to query"
    +> flag "config" (optional file) ~doc:"CONFIG Async_smtp config file"
  ;;

  let rpc ~summary ?readme the_spec main =
    Command.async_spec ~summary ?readme
      Command.Spec.(
        the_spec
        ++ step (fun m ~rpc_server ~config:_ () ->
          rpc_client_command0 ~host_and_port:rpc_server ~f:m
        )
        ++ rpc_server_or_config_flag ()
      )
      main

  ;;
  let config_or_rpc ~summary ?readme the_spec main =
    Command.async_spec ~summary ?readme
      Command.Spec.(
        the_spec
        ++ step (fun m ~rpc_server ~config () ->
          match config with
          | Some config ->
            m (`Config config)
          | None ->
            rpc_client_command0 ~host_and_port:rpc_server ~f:(fun client ->
              m (`Rpc client))
        )
        ++ rpc_server_or_config_flag ()
      )
      main

end
