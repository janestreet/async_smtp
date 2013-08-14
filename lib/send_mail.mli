val send_email :
  host:string
  -> port:int
  -> sender:string
  -> receivers:string list
  -> string
  -> bool Async.Std.Deferred.t