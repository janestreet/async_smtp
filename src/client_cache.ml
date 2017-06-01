open Core
open Async

module Log = Mail_log

module Config = struct
  type t =
    { max_open_connections          : int
    ; cleanup_idle_connection_after : Time.Span.t
    ; max_connections_per_address   : int
    ; max_connection_reuse          : int
    } [@@deriving sexp, fields]

  let create = Fields.create
end

module Tcp_options = struct
  type t =
    { buffer_age_limit   : [`At_most of Time.Span.t | `Unlimited] option
    ; interrupt          : unit Deferred.t                        option
    ; reader_buffer_size : int                                    option
    ; timeout            : Time.Span.t                            option
    }

  let create ?buffer_age_limit ?interrupt ?reader_buffer_size ?timeout () =
    { buffer_age_limit
    ; interrupt
    ; reader_buffer_size
    ; timeout
    }
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

  module Args = struct
    module T = struct
      type t =
        { common : Common_args.t
        ; address : Address.t
        } [@@deriving fields]

      let to_string_hum t = Address.to_string t.address

      let sexp_of_t t = Address.sexp_of_t t.address

      (* Make sure not to hash [t.common] because there is a [Deferred.t] in there *)
      let hash t = Hashtbl.hash t.address

      let compare t1 t2 = Address.compare t1.address t2.address
    end
    include T
    include Comparable.Make_plain(T)
    include Hashable.  Make_plain(T)

    let create ~common address = { common; address }
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

type t =
  { cache : Client_cache.t
  ; common_args : Resource.Common_args.t
  }

let init ?buffer_age_limit ?interrupt ?reader_buffer_size ?timeout
      ?component ~log ~(cache_config : Config.t) ~client_config () =
  let config =
    Cache.Config.create
      ~max_resources:cache_config.max_open_connections
      ~idle_cleanup_after:cache_config.cleanup_idle_connection_after
      ~max_resources_per_id:cache_config.max_connections_per_address
      ~max_resource_reuse:cache_config.max_connection_reuse
  in
  let tcp_options =
    Tcp_options.create ?buffer_age_limit ?interrupt ?reader_buffer_size ?timeout ()
  in
  let common_args =
    Resource.Common_args.create ?component ~tcp_options ~log ~client_config ()
  in
  { cache = Client_cache.init ~config; common_args }
;;

let close_and_flush t = Client_cache.close_and_flush t.cache
let close_finished t = Client_cache.close_finished t.cache
let close_started t = Client_cache.close_started t.cache

module Tcp = struct
  let with_' ?give_up ~f ~cache addresses =
    let f (t : Resource.t) = f ~flows:t.flows t.client in
    let args_list =
      List.map addresses ~f:(Resource.Args.create ~common:cache.common_args)
    in
    let rec with_any_loop ~failed = function
      | [] ->
        return (`Error_opening_resource failed)
      | args_list ->
        Client_cache.with_any' ~open_timeout:(Time.Span.of_sec 10.)
          ?give_up cache.cache args_list
          ~f:(fun r ->
            f r
            >>= function
            | (Error _) as err ->
              (* Close the Connection if the callback returns an [Error] *)
              Resource.close r
              >>| const err
            | (Ok _) as ok ->
              return ok)
        >>= function
        | `Ok (args, res) ->
          return (`Ok (Resource.Args.address args, res))
        | `Gave_up_waiting_for_resource
        | `Cache_is_closed as res -> return res
        | `Error_opening_resource (failed_args, e) ->
          let remaining =
            List.filter args_list
              ~f:(fun args ->
                not (Resource.Args.equal args failed_args))
          in
          with_any_loop
            ~failed:((Resource.Args.address failed_args, e) :: failed)
            remaining
    in
    with_any_loop ~failed:[] args_list
end
