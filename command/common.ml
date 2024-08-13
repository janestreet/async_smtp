open Core
open Async
open Async_smtp

module Command : sig
  include module type of Command

  val rpc
    :  summary:string
    -> ?readme:(unit -> string)
    -> (Rpc.Connection.t -> unit Deferred.t) Param.t
    -> t

  val configs_or_rpc
    :  summary:string
    -> ?readme:(unit -> string)
    -> ([ `Configs of Smtp_server.Config.t * Smtp_spool.Config.t
        | `Rpc of Rpc.Connection.t
        ]
        -> unit Deferred.t)
         Param.t
    -> t
end = struct
  include Command

  let rpc_client_command0 ~host_and_port ~f =
    let open Deferred.Let_syntax in
    match%map
      Rpc.Connection.with_client
        (Tcp.Where_to_connect.of_host_and_port host_and_port)
        (fun conn -> f conn)
    with
    | Ok a -> a
    | Error exn -> raise exn
  ;;

  let rpc_server_or_configs_flag =
    let open Command.Let_syntax in
    [%map_open
      let rpc_server =
        flag
          "rpc-server"
          (optional host_and_port)
          ~doc:"HOST:PORT mailcore instance to query"
      and server_config_path =
        flag
          "server-config"
          (optional Filename_unix.arg_type)
          ~doc:"CONFIG Async_smtp server config file"
      and spool_config_path =
        flag
          "spool-config"
          (optional Filename_unix.arg_type)
          ~doc:"CONFIG Async_smtp spool config file"
      in
      let server_config =
        Option.map server_config_path ~f:(fun c ->
          Thread_safe.block_on_async_exn (fun () -> Smtp_server.Config.load_exn c))
      in
      let spool_config =
        Option.map spool_config_path ~f:(fun c ->
          Thread_safe.block_on_async_exn (fun () -> Smtp_spool.Config.load_exn c))
      in
      let rpc_server =
        match rpc_server, server_config, spool_config with
        | Some v, (_ : Smtp_server.Config.t option), (_ : Smtp_spool.Config.t option) -> v
        | None, None, None ->
          failwith
            "At least one of (-rpc-server) or (-server-config and -spool-config) required"
        | None, Some _, None | None, None, Some _ ->
          failwith "Both -server-config and -spool-config required, or use -rpc-server"
        | None, Some server_config, Some _ ->
          let port = Smtp_server.Config.rpc_port server_config in
          Host_and_port.create ~host:"localhost" ~port
      in
      rpc_server, server_config, spool_config]
  ;;

  let rpc ~summary ?readme main =
    let open Command.Let_syntax in
    Command.async
      ~summary
      ?readme
      [%map_open
        let main = main
        and rpc_server, _server_config, _spool_config = rpc_server_or_configs_flag in
        fun () -> rpc_client_command0 ~host_and_port:rpc_server ~f:main]
      ~behave_nicely_in_pipeline:false
  ;;

  let configs_or_rpc ~summary ?readme main =
    let open Command.Let_syntax in
    Command.async
      ~summary
      ?readme
      [%map_open
        let main = main
        and rpc_server, server_config, spool_config = rpc_server_or_configs_flag in
        fun () ->
          match server_config, spool_config with
          | Some server_config, Some spool_config ->
            main (`Configs (server_config, spool_config))
          | None, None ->
            rpc_client_command0 ~host_and_port:rpc_server ~f:(fun client ->
              main (`Rpc client))
          | _ ->
            (* Other cases should be prevented in rpc_server_or_configs_flag *)
            assert false]
      ~behave_nicely_in_pipeline:false
  ;;
end
