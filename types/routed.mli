open! Core

type 'a t =
  { envelope : 'a
  (** Next hops to try. If the first one fails, we are done, otherwise try sending to the
      second one, etc. *)
  ; next_hop_choices : Host_and_port.t list
  ; retry_intervals : Retry_interval.t list
  }
[@@deriving sexp_of, fields ~getters ~iterators:create, compare, hash]

type 'a create =
  envelope:'a
  -> next_hop_choices:Host_and_port.t list
  -> retry_intervals:Retry_interval.t list
  -> 'a t

type 'a set =
  ?sender:Sender.t
  -> ?sender_args:Sender_argument.t list
  -> ?recipients:Email_address.t list
  -> ?rejected_recipients:Email_address.t list
  -> ?route:string option
  -> ?next_hop_choices:Host_and_port.t list
  -> ?retry_intervals:Retry_interval.t list
  -> 'a
