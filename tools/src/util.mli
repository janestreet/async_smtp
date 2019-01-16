open Core
open Async
open Async_smtp

module Host_and_port : sig
  include module type of Host_and_port

  val of_string : port:int -> string -> t
  val inet_address : t -> Tcp.Where_to_connect.inet
end

module Address : sig
  val param_anon : Host_and_port.t Command.Param.t
  val param_server : Host_and_port.t Command.Param.t
end

module Smtp_client_config : sig
  include module type of Smtp_client.Config

  val default : t Lazy.t
  val param : t Command.Param.t
end

module Hex : sig
  val to_hex : string -> string
end
