open Core
open Async
open Async_smtp_types

let ( ^- ) a b = a ^ "-" ^ b

let rpc ?(version = 0) (type q r) ~name q r =
  let module Q = (val q : Binable.S with type t = q) in
  let module R = (val r : Binable.S with type t = r) in
  Rpc.Rpc.create
    ~name
    ~version
    ~bin_query:Q.bin_t
    ~bin_response:R.bin_t
    ~include_in_error_count:Only_on_exn
;;

let pipe_rpc ?(version = 0) (type q r e) ~name q r e =
  let module Q = (val q : Binable.S with type t = q) in
  let module R = (val r : Binable.S with type t = r) in
  let module E = (val e : Binable.S with type t = e) in
  Rpc.Pipe_rpc.create
    ~name
    ~version
    ~bin_query:Q.bin_t
    ~bin_response:R.bin_t
    ~bin_error:E.bin_t
    ()
;;

let state_rpc ?(version = 0) (type q s u e) ~name q s u e =
  let module Q = (val q : Binable.S with type t = q) in
  let module S = (val s : Binable.S with type t = s) in
  let module U = (val u : Binable.S with type t = u) in
  let module E = (val e : Binable.S with type t = e) in
  Rpc.State_rpc.create
    ~name
    ~version
    ~bin_query:Q.bin_t
    ~bin_state:S.bin_t
    ~bin_update:U.bin_t
    ~bin_error:E.bin_t
    ()
;;

let or_error (type a) m : (module Binable.S with type t = a Or_error.t) =
  let module M = (val m : Binable.S with type t = a) in
  (module struct
    type t = M.t Or_error.t [@@deriving bin_io]
  end)
;;

let list (type a) m : (module Binable.S with type t = a list) =
  let module M = (val m : Binable.S with type t = a) in
  (module struct
    type t = M.t list [@@deriving bin_io]
  end)
;;

let option (type a) m : (module Binable.S with type t = a option) =
  let module M = (val m : Binable.S with type t = a) in
  (module struct
    type t = M.t option [@@deriving bin_io]
  end)
;;

let pair (type a b) (m1, m2) : (module Binable.S with type t = a * b) =
  let module M1 = (val m1 : Binable.S with type t = a) in
  let module M2 = (val m2 : Binable.S with type t = b) in
  (module struct
    type t = M1.t * M2.t [@@deriving bin_io]
  end)
;;

let triple (type a b c) (m1, m2, m3) : (module Binable.S with type t = a * b * c) =
  let module M1 = (val m1 : Binable.S with type t = a) in
  let module M2 = (val m2 : Binable.S with type t = b) in
  let module M3 = (val m3 : Binable.S with type t = c) in
  (module struct
    type t = M1.t * M2.t * M3.t [@@deriving bin_io]
  end)
;;

let binable (type a) m : (module Binable.S with type t = a) = m
let string = binable (module String)
let int = binable (module Int)
let unit = binable (module Unit)
let bool = binable (module Bool)
let span = binable (module Time_float.Stable.Span.V2)
let retry_interval = binable (module Smtp_envelope.Retry_interval.Stable.V2)
let error = binable (module Error.Stable.V2)
let smtp_event = binable (module Smtp_events.Event)
let id = binable (module Spool.Stable.Message_id.V1)
let spool_status_v2 = binable (module Spool.Stable.Status.V2)
let spool_status = binable (module Spool.Stable.Status.V3)
let spool_event = binable (module Spool.Stable.Event.V1)
let send_info = binable (module Spool.Stable.Send_info.V1)
let recover_info = binable (module Spool.Stable.Recover_info.V2)
let gc_stat = binable (module Gc.Stable.Stat.V2)
let pid = binable (module Pid.Stable.V1)
let cache_status_v2 = binable (module Client_cache.Status.Stable.V2)
let cache_config = binable (module Client_cache.Config.Stable.V1)

module Monitor = struct
  (* Including a sequence number. We broadcast a heartbeat message (with error = None)
     every 10 seconds.. *)
  let errors = pipe_rpc ~name:"errors" unit (pair (int, option error)) error
end

module Smtp_events = struct
  let prefix = "server"

  let events =
    pipe_rpc ~name:(prefix ^- "envelope-received") unit smtp_event (or_error unit)
  ;;
end

module Spool = struct
  module Cache = struct
    let status = pipe_rpc ~version:2 ~name:"cache-status" span cache_status_v2 error
    let config = rpc ~name:"cache-config" unit cache_config
  end

  let prefix = "spool"

  module Status = struct
    let v2 = rpc ~name:(prefix ^- "status") unit spool_status_v2 ~version:2
    let v3 = rpc ~name:(prefix ^- "status") unit spool_status ~version:3

    module V4 = Streamable.Plain_rpc.Make (struct
        let name = prefix ^- "status"
        let version = 4
        let client_pushes_back = false

        type query = unit [@@deriving bin_io]
        type response = Spool.Stable.Status.V3.t [@@deriving bin_io]

        module Response = Spool.Stable.Status.V3
      end)

    let v4 = V4.rpc

    let callee =
      let open Babel.Callee.Rpc in
      singleton v2
      |> map_response ~f:Spool.Stable.Status.V2.of_v3
      |> add ~rpc:v3
      |> map_response ~f:Or_error.ok_exn
      |> Babel.Callee.Streamable_plain_rpc.add ~rpc:v4
    ;;

    let caller =
      let open Babel.Caller.Rpc in
      singleton v2
      |> map_response ~f:Spool.Stable.Status.V2.to_v3
      |> add ~rpc:v3
      |> map_response ~f:Or_error.return
      |> Babel.Caller.Streamable_plain_rpc.add ~rpc:v4
    ;;

    let dispatch client =
      let open Deferred.Or_error.Let_syntax in
      let%bind client = Versioned_rpc.Connection_with_menu.create client in
      let%bind result = Babel.Caller.Rpc.dispatch_multi caller client () in
      Deferred.return result
    ;;
  end

  let freeze = rpc ~name:(prefix ^- "freeze") (list id) (or_error unit) ~version:1

  let send =
    rpc ~name:(prefix ^- "send") (pair (list retry_interval, send_info)) (or_error unit)
  ;;

  let remove = rpc ~name:(prefix ^- "remove") (list id) (or_error unit)
  let recover = rpc ~name:(prefix ^- "recover") recover_info (or_error unit) ~version:1
  let events = pipe_rpc ~name:(prefix ^- "events") unit spool_event error
end

module Gc = struct
  let stat = rpc ~name:"gc-stat" unit gc_stat
  let quick_stat = rpc ~name:"gc-quick-stat" unit gc_stat
  let full_major = rpc ~name:"gc-full-major" unit unit
  let major = rpc ~name:"gc-major" unit unit
  let minor = rpc ~name:"gc-minor" unit unit
  let compact = rpc ~name:"gc-compact" unit unit
  let stat_pipe = pipe_rpc ~name:"gc-stat-pipe" unit gc_stat error
end

module Process = struct
  let pid = rpc ~name:"proc-pid" unit pid
end
