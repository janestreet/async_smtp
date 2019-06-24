module Stable = struct
  open Core.Core_stable

  module Address_and_route = struct
    module V1 = struct
      type t =
        { address : Host_and_port.V1.t
        ; route : string option
        }
      [@@deriving sexp, bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 72331b37f209d3a3e9f43e2a7ce58395 |}]
      ;;
    end
  end
end

open Core
open Async
module Log = Mail_log
module Config = Resource_cache.Address_config

module Tcp_options = struct
  type t =
    { buffer_age_limit : [ `At_most of Time.Span.t | `Unlimited ] option
    ; interrupt : unit Deferred.t option
    ; reader_buffer_size : int option
    ; writer_buffer_size : int option
    ; timeout : Time.Span.t option
    }

  let create
        ?buffer_age_limit
        ?interrupt
        ?reader_buffer_size
        ?writer_buffer_size
        ?timeout
        ()
    =
    { buffer_age_limit; interrupt; reader_buffer_size; writer_buffer_size; timeout }
  ;;
end

module Address_and_route = struct
  module Stable = Stable.Address_and_route

  module T = struct
    type t = Stable.V1.t =
      { address : Host_and_port.t
      ; route : string option
      }
    [@@deriving compare, hash, sexp_of, fields]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
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
    [@@deriving fields]

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
           ~log
           (Address_and_route.address address)
           ~f:(fun client ->
             Ivar.fill result (Ok client);
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
     Ivar.fill close_finished ());
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
    module V1 = Make_stable.V1 (Address_and_route.Stable.V1)
  end
end

type t =
  { cache : Client_cache.t
  ; load_balance : bool
  }

let init
      ?buffer_age_limit
      ?interrupt
      ?reader_buffer_size
      ?writer_buffer_size
      ?timeout
      ?component
      ~log
      ~cache_config
      ~client_config
      ~load_balance
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
      ()
  in
  let args = Resource.Common_args.create ~component ~tcp_options ~log ~client_config in
  { cache = Client_cache.init ~config args; load_balance }
;;

let close_and_flush t = Client_cache.close_and_flush t.cache
let close_finished t = Client_cache.close_finished t.cache
let close_started t = Client_cache.close_started t.cache
let status t = Client_cache.status t.cache
let config t = Config.of_cache_config (Client_cache.config t.cache)

module Tcp = struct
  let with_' ?give_up ~f ~cache:t ?route addresses =
    let f (r : Resource.t) =
      match%bind f ~flows:r.flows r.client with
      | Error _ as err ->
        (* Close the Connection if the callback returns an [Error] *)
        Resource.close r >>| const err
      | Ok _ as ok -> return ok
    in
    let addresses =
      List.map addresses ~f:(fun address -> { Address_and_route.address; route })
    in
    match%map
      Client_cache.with_any_loop
        ~open_timeout:(Time_ns.Span.of_sec 10.)
        ?give_up
        ~load_balance:t.load_balance
        t.cache
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
