open! Core

(** See multispool_intf.ml for main documentation *)

module Make (S : Multispool_intf.Spoolable.S) :
  Multispool_intf.S with module Spoolable := S

module Monitor : sig
  module Make (S : Multispool_intf.Spoolable.S) :
    Multispool_intf.Monitor.S with module Spoolable := S
end

module For_testing : sig
  (** A rudimentary [Name_generator] for use in tests that guarantees that the
      Lexicographic order corresponds to the time ordering *)
  module Lexicographic_time_order_name_generator :
    Multispool_intf.Name_generator.S with type t = int

  (** Like [Make] above, but ensures a stricter ordering for use in test cases.
      [S.Name_generator] must provide a strict time ordering, such as
      [Lexicographic_time_order_name_generator]. *)
  module Make (S : Multispool_intf.Spoolable.S) :
    Multispool_intf.S with module Spoolable := S
end
