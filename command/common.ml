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
  val config_or_rpc
    :  summary:string
    -> ?readme:(unit -> string)
    -> ([`Config of Smtp_server.Config.t | `Rpc of Rpc.Connection.t] -> unit Deferred.t) Param.t
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

  let rpc_server_or_config_flag =
    let open Command.Let_syntax in
    [%map_open
      let rpc_server =
        flag "rpc-server" (optional host_and_port)
          ~doc:"HOST:PORT mailcore instance to query"
      and config_path =
        flag "config" (optional file) ~doc:"CONFIG Async_smtp config file"
      in
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
      rpc_server, config
    ]
  ;;

  let rpc ~summary ?readme main =
    let open Command.Let_syntax in
    Command.async ~summary ?readme
      [%map_open
        let main = main
        and rpc_server, _config = rpc_server_or_config_flag
        in
        fun () ->
          rpc_client_command0 ~host_and_port:rpc_server ~f:main
      ]
  ;;

  let config_or_rpc ~summary ?readme main =
    let open Command.Let_syntax in
    Command.async ~summary ?readme
      [%map_open
        let main = main
        and rpc_server, config = rpc_server_or_config_flag
        in
        fun () ->
          match config with
          | Some config ->
            main (`Config config)
          | None ->
            rpc_client_command0 ~host_and_port:rpc_server ~f:(fun client ->
              main (`Rpc client))
      ]
  ;;
end
