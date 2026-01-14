open Core
open Async
module Log = Mail_log
module Config = Resource_cache.Address_config

module Tcp_options = struct
  type t =
    { buffer_age_limit : [ `At_most of Time_float.Span.t | `Unlimited ] option
    ; interrupt : unit Deferred.t option
    ; reader_buffer_size : int option
    ; writer_buffer_size : int option
    ; timeout : Time_float.Span.t option
    ; time_source : Time_source.t option
    }

  let create
    ?buffer_age_limit
    ?interrupt
    ?reader_buffer_size
    ?writer_buffer_size
    ?timeout
    ?time_source
    ()
    =
    { buffer_age_limit
    ; interrupt
    ; reader_buffer_size
    ; writer_buffer_size
    ; timeout
    ; time_source
    }
  ;;
end

module Resource = struct
  module Key = Address_and_route

  module Common_args = struct
    type t =
      { tcp_options : Tcp_options.t
      ; component : Mail_log.Component.t option
      ; log : Mail_log.t
      ; client_config : Client_config.t
      }
    [@@deriving fields ~iterators:create]

    let create = Fields.create
  end

  type t =
    { close_started : unit Ivar.t
    ; close_finished : unit Ivar.t
    ; client : Client.t
    ; flows : Mail_log.Flows.t
    }

  let open_ address (args : Common_args.t) =
    let tcp_options : Tcp_options.t = args.tcp_options in
    let log = args.log in
    let close_started = Ivar.create () in
    let close_finished = Ivar.create () in
    let result = Ivar.create () in
    don't_wait_for
      (match%map
         Client.Tcp.with_
           ?buffer_age_limit:tcp_options.buffer_age_limit
           ?interrupt:tcp_options.interrupt
           ?reader_buffer_size:tcp_options.reader_buffer_size
           ?writer_buffer_size:tcp_options.writer_buffer_size
           ?timeout:tcp_options.timeout
           ?component:args.component
           ~config:args.client_config
           ?credentials:(Address_and_route.credentials address)
           ~log
           (Address_and_route.address address)
           ~f:(fun client ->
             Ivar.fill_exn result (Ok client);
             Ivar.read close_finished |> Deferred.ok)
       with
       | Ok () -> ()
       | Error e ->
         (* [result] might have already been filled by [f]. For example, there could have
            been a writer error caught by [Tcp.with_]. *)
         Ivar.fill_if_empty result (Error e));
    Ivar.read result
    >>|? fun client ->
    let flows = Mail_log.Flows.create `Cached_connection in
    Log.info
      log
      (lazy
        (Log.Message.create
           ~here:[%here]
           ~flows
           ~component:[ "client_cache" ]
           "connection opened"));
    (Deferred.any
       [ Reader.close_finished (Client_raw.reader client |> Option.value_exn)
       ; Writer.close_finished (Client_raw.writer client)
       ]
     >>> fun () ->
     Log.info
       log
       (lazy
         (Log.Message.create
            ~here:[%here]
            ~flows
            ~component:[ "client_cache" ]
            "connection closed"));
     Ivar.fill_exn close_finished ());
    { close_started; close_finished; client; flows }
  ;;

  let close_finished t = Ivar.read t.close_finished
  let has_close_started t = Ivar.is_full t.close_started

  let close t =
    Ivar.fill_if_empty t.close_started ();
    let%bind () = Writer.close (Client_raw.writer t.client) in
    Reader.close (Client_raw.reader t.client |> Option.value_exn)
  ;;
end

module Client_cache = struct
  include Resource_cache.Make (Resource) ()

  let init ~config k = init ~config ~log_error:(Async.Log.Global.string ~level:`Error) k
end

module Status = struct
  include Client_cache.Status

  module Stable = struct
    module V2 = Make_stable.V1 (Address_and_route.Stable.V2)
  end
end

type t = Client_cache.t

let init
  ?buffer_age_limit
  ?interrupt
  ?reader_buffer_size
  ?writer_buffer_size
  ?timeout
  ?time_source
  ?component
  ~log
  ~cache_config
  ~client_config
  ~connection_cache_warming
  ()
  =
  let config = Config.to_cache_config cache_config in
  let tcp_options =
    Tcp_options.create
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      ?time_source
      ()
  in
  let args = Resource.Common_args.create ~component ~tcp_options ~log ~client_config in
  let cache = Client_cache.init ~config args in
  Option.iter
    connection_cache_warming
    ~f:(fun (connection_cache_warming : Spool_config.Connection_cache_warming.t) ->
      Client_cache.keep_cache_warm
        cache
        ~on_error_opening_resource:(fun ~key:address ~error ->
          [%log.info
            "Failed to open connection when warming cache"
              (address : Address_and_route.t)
              (error : Error.t)])
        ~num_resources_to_keep_open_per_key:
          connection_cache_warming.num_connections_per_address
        connection_cache_warming.addresses_to_keep_warm);
  cache
;;

let close_and_flush t = Client_cache.close_and_flush t
let close_finished t = Client_cache.close_finished t
let close_started t = Client_cache.close_started t
let status t = Client_cache.status t
let config t = Config.of_cache_config (Client_cache.config t)

module Tcp = struct
  let with_' ?give_up ~f ~cache:t ?route ?credentials addresses =
    let f (r : Resource.t) =
      match%bind f ~flows:r.flows r.client with
      | Error _ as err ->
        (* Close the Connection if the callback returns an [Error] *)
        Resource.close r >>| const err
      | Ok _ as ok -> return ok
    in
    let addresses =
      List.map addresses ~f:(fun address ->
        { Address_and_route.address; credentials; route })
    in
    match%map
      Client_cache.with_any_loop
        ~open_timeout:(Time_ns.Span.of_sec 10.)
        ?give_up
        t
        addresses
        ~f
    with
    | `Ok (key, res) -> `Ok (Address_and_route.address key, res)
    | `Error_opening_all_resources errors ->
      let errors =
        List.map errors ~f:(fun (key, err) -> Address_and_route.address key, err)
      in
      `Error_opening_all_addresses errors
    | `Gave_up_waiting_for_resource -> `Gave_up_waiting_for_address
    | `Cache_is_closed -> `Cache_is_closed
  ;;
end
