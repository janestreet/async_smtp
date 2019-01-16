open! Core
open! Async

module Monitor : sig
  val rpcs : unit -> unit Rpc.Implementation.t list
end

module Spool : sig
  val rpcs : Spool.t Rpc.Implementation.t list
end

module Smtp_events : sig
  val rpcs : Smtp_events.t Rpc.Implementation.t list
end

module Gc : sig
  val rpcs : unit Rpc.Implementation.t list
end

module Process : sig
  val rpcs : unit Rpc.Implementation.t list
end
