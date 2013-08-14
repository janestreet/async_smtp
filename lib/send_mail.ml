open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std

let send_email ~host ~port ~sender ~receivers message =
  let destination = Tcp.to_host_and_port host port in
  Tcp.with_connection destination (fun _socket reader writer ->
    Smtp.Client.send_email reader writer ~from:sender ~to_:receivers message)
