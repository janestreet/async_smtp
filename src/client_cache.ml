open Core
open Async
open Async_smtp_types

module Address : sig
  type t = Smtp_socket_address.t [@@deriving sexp, bin_io]
  include module type of Smtp_socket_address with type t:=t
end = struct
  include Smtp_socket_address
  include (Smtp_socket_address.Stable.V1 : sig
             type t [@@deriving sexp, bin_io]
           end with type t:=t)
end

module Log = Mail_log

module Config = struct
  type t =
    { max_open_connections          : int
    ; cleanup_idle_connection_after : Time.Span.t
    ; max_connections_per_address   : int
    ; max_connection_reuse          : int
    } [@@deriving sexp, fields, bin_io, compare]

  let create = Fields.create

  let to_cache_config t =
    Cache.Config.create
      ~max_resources:t.max_open_connections
      ~idle_cleanup_after:t.cleanup_idle_connection_after
      ~max_resources_per_id:t.max_connections_per_address
      ~max_resource_reuse:t.max_connection_reuse

  let of_cache_config (cache_config : Cache.Config.t) =
    create
      ~max_open_connections:cache_config.max_resources
      ~cleanup_idle_connection_after:cache_config.idle_cleanup_after
      ~max_connections_per_address:cache_config.max_resources_per_id
      ~max_connection_reuse:cache_config.max_resource_reuse

end

module Tcp_options = struct
  type t =
    { buffer_age_limit   : [`At_most of Time.Span.t | `Unlimited] option
    ; interrupt          : unit Deferred.t                        option
    ; reader_buffer_size : int                                    option
    ; writer_buffer_size : int                                    option
    ; timeout            : Time.Span.t                            option
    }

  let create ?buffer_age_limit ?interrupt ?reader_buffer_size ?writer_buffer_size ?timeout () =
    { buffer_age_limit
    ; interrupt
    ; reader_buffer_size
    ; writer_buffer_size
    ; timeout
    }
end

module Address_and_route = struct
  module T = struct
    type t =
      { address : Address.t
      ; route   : string option
      } [@@deriving bin_io, compare, hash, sexp, fields]
    include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
    let module_name = "Async_smtp.Private.Client_cache.Address_and_route"
  end
  include T
  include Identifiable.Make (T)
end

module Resource = struct
  module Common_args = struct
    type t =
      { tcp_options   : Tcp_options.t
      ; component     : Mail_log.Component.t option
      ; log           : Mail_log.t
      ; client_config : Client_config.t
      }

    let create ?component ~tcp_options ~log ~client_config () =
      { tcp_options
      ; component
      ; log
      ; client_config
      }
  end

  module Key = Address_and_route

  module Args = struct
    type t =
      { common  : Common_args.t
      ; address : Address.t
      ; route   : string option
      } [@@deriving fields]

    let to_string_hum t = Address.to_string t.address

    let key { address; route; _ } = { Key. address; route }

    let create ~common ?route address = { common; route; address }
  end

  type t =
    { close_started  : unit Ivar.t
    ; close_finished : unit Ivar.t
    ; client         : Client.t
    ; flows          : Mail_log.Flows.t
    } [@@deriving fields]

  let open_ (args : Args.t) =
    let common : Common_args.t = args.common in
    let tcp_options : Tcp_options.t = common.tcp_options in
    let log = common.log in
    let close_started = Ivar.create () in
    let close_finished = Ivar.create () in
    let result = Ivar.create () in
    don't_wait_for begin
      Client.Tcp.with_
        ?buffer_age_limit:tcp_options.buffer_age_limit
        ?interrupt:tcp_options.interrupt
        ?reader_buffer_size:tcp_options.reader_buffer_size
        ?writer_buffer_size:tcp_options.writer_buffer_size
        ?timeout:tcp_options.timeout
        ?component:common.component
        ~config:common.client_config
        ~log
        args.address
        ~f:(fun client ->
          Ivar.fill result (Ok client);
          Ivar.read close_finished |> Deferred.ok)
      >>| function
      | Ok () -> ()
      | Error e ->
        (* [result] might have already been filled by [f]. For example, there could have
           been a writer error caught by [Tcp.with_]. *)
        Ivar.fill_if_empty result (Error e)
    end;
    Ivar.read result
    >>|? fun client ->
    let flows = Mail_log.Flows.create `Cached_connection in
    Log.info log (lazy (Log.Message.create
                          ~here:[%here]
                          ~flows
                          ~component:["client_cache"]
                          "connection opened"));
    begin
      Deferred.any
        [ Reader.close_finished (Client_raw.reader client |> Option.value_exn)
        ; Writer.close_finished (Client_raw.writer client)
        ]
      >>> fun () ->
      Log.info log (lazy (Log.Message.create
                            ~here:[%here]
                            ~flows
                            ~component:["client_cache"]
                            "connection closed"));
      Ivar.fill close_finished ()
    end;
    { close_started; close_finished; client; flows }
  ;;

  let close_finished t = Ivar.read t.close_finished

  let is_closed t = Ivar.is_full t.close_started

  let close t =
    Ivar.fill_if_empty t.close_started ();
    Writer.close (Client_raw.writer t.client)
    >>= fun () ->
    Reader.close (Client_raw.reader t.client |> Option.value_exn)
  ;;
end

module Client_cache = Cache.Make(Resource)

module Status = Client_cache.Status

type t =
  { cache : Client_cache.t
  ; common_args : Resource.Common_args.t
  }

let init ?buffer_age_limit ?interrupt ?reader_buffer_size ?writer_buffer_size ?timeout
      ?component ~log ~(cache_config : Config.t) ~client_config () =
  let config = Config.to_cache_config cache_config in
  let tcp_options =
    Tcp_options.create ?buffer_age_limit ?interrupt ?reader_buffer_size
      ?writer_buffer_size ?timeout ()
  in
  let common_args =
    Resource.Common_args.create ?component ~tcp_options ~log ~client_config ()
  in
  { cache = Client_cache.init ~config; common_args }
;;

let close_and_flush t = Client_cache.close_and_flush t.cache
let close_finished t = Client_cache.close_finished t.cache
let close_started t = Client_cache.close_started t.cache

let status t = Client_cache.status t.cache
let config t = Config.of_cache_config (Client_cache.config t.cache)

module Tcp = struct
  let with_' ?give_up ~f ~cache ?route addresses =
    let f (r : Resource.t) =
      f ~flows:r.flows r.client
      >>= function
      | (Error _) as err ->
        (* Close the Connection if the callback returns an [Error] *)
        Resource.close r
        >>| const err
      | (Ok _) as ok ->
        return ok
    in
    let args_list =
      List.map addresses ~f:(Resource.Args.create ~common:cache.common_args ?route)
    in
    Client_cache.with_any_loop ~open_timeout:(Time.Span.of_sec 10.)
      ?give_up cache.cache args_list ~f
    >>| function
    | `Ok (args, res) ->
      `Ok (args.address, res)
    | `Error_opening_all_resources list ->
      let address_list = List.map list ~f:(fun (args, err) -> args.address, err) in
      `Error_opening_all_addresses address_list
    | `Gave_up_waiting_for_resource -> `Gave_up_waiting_for_address
    | `Cache_is_closed -> `Cache_is_closed
  ;;
end
