open Core
open Async

(** {0 Multispool}

    Multispool allows multiple, separate processes to cooperate via a filesystem-based
    queue. The design was influenced by various UNIX tools that use typical POSIX-y
    flush/close/rename semantics to provide atomic filesystem operations (Maildir, in
    particular--see http://www.qmail.org/man/man5/maildir.html for an overview).

    One or more processes may place files in a queue, wait for files to appear in queues
    (and handle them), or iterate over files in a queue.

    {1 Usage Summary}

    A spool is physically represented by a directory, and logically as a module created by
    applying the [Multispool] functor. A spool deals in a data type that implements the
    [Spoolable] interface. This interface tells the spool how to encode and decode items
    for on-disk storage and how to map queue names to directories on disk. See
    ../test/lib/widget.ml for an example [Spoolable.t].

    An existing spool is opened with [load] and a new one is created with [create].

    Use [enqueue] to add items to a queue, optionally reserving a name beforehand with
    [reserve_name].

    If you want to wait on entries to appear, use [Queue_reader.iter].  If you want to
    make periodic passes over all entries in a queue, use [Queue_reader.iter_available].

    Lower-level functionality is available in the [Expert] module.

    {1 Implementation Details}

    Enqueueing attempts to create a file with a unique name within the .registry/
    directory by open(2)ing the file with the O_CREAT and O_EXCL flag (which will fail if
    the name exists). If it fails, a new name is generated via [Spoolable.Name_generator]
    and the process repeats. Once a file is created in .registry/, it remains as a name
    "reservation" and a file (with the same name) is created in the desired queue. Keeping
    the empty file in .registry/ ensures that no other process can create the same file
    name while this name is in use within the spool. *)

module Name_generator = struct
  (** Generates filenames for enqueued [Spoolable.t]s.  [t] is a user-supplied input to
      name generation via [name]. *)
  module type S = sig
    type t
    val next : t -> attempt:int -> string
  end
end

module Spoolable = struct
  module type S = sig
    type t

    (** Use Stringable.S interface for disk persistence *)
    include Stringable.S with type t := t

    (** [Queue.t] is an enumerable type that represents the available queues the mapping
        to directory names on-disk. *)
    module Queue : sig
      type t [@@deriving sexp, enumerate]

      val to_dirname : t -> string
    end

    module Name_generator : Name_generator.S
  end
end

module type S = sig
  type t [@@deriving sexp_of]
  type spool = t

  (** Set to a Spoolable module when including this module and when applying
      [Multispool.Make] *)
  module Spoolable : Spoolable.S

  val dir : t -> string

  (** Open a [Multispool.t].  This function will fail by default if the spool directory
      does not exist, does not look like a spool, or does not contain the set of
      directories named after the strings returned by [Spoolable.Queue.to_dir].  Pass
      [~create_if_missing:true] to create the necessary directories.

      Note that, even if [~create_if_missing:()] is specified, this function will still
      fail if the supplied directory is non-empty and not already a spool. *)
  val load : ?create_if_missing:unit -> string -> t Deferred.Or_error.t

  (** Open a [Multispool.t] with no spool directory validation. *)
  val load_unsafe : string -> t

  (** Open a [Multispool.t] and create the spool directory if necessary. This is
      functionally identical to [load ?create_if_missing:()]. *)
  val create : string -> t Deferred.Or_error.t

  (** An [Entry] is associated with a particular queue *)
  module Entry : sig
    type t [@@deriving sexp_of]

    (** Create an entry.  This is exposed so that an [Entry.t] may be created from a file
        name on disk *)
    val create : spool -> Spoolable.Queue.t -> name:string -> t

    (** Get contents of this [Entry.t] in an unsafe way.  No checkout is performed and the
        data is read directly from the queue file.  If you need to later update this data,
        consider revalidating the contents after checkout and before writing. *)
    val contents_unsafe : t -> Spoolable.t Deferred.Or_error.t

    val stat : t -> Unix.Stats.t Deferred.Or_error.t

    val spool : t -> spool
    val queue : t -> Spoolable.Queue.t
    val name  : t -> string
  end

  val list : t -> Spoolable.Queue.t -> Entry.t list Deferred.Or_error.t

  module Unique_name : sig
    type t = private string

    val reserve
      :  spool
      -> Spoolable.Name_generator.t
      -> t Deferred.Or_error.t
  end

  (** Add a [Spoolable.t] to a queue.  An [Entry.t] is returned, but it may make sense to
      ignore it. *)
  val enqueue
    :  t
    -> Spoolable.Queue.t
    -> Spoolable.t
    -> [ `Reserve of Spoolable.Name_generator.t | `Use of Unique_name.t ]
    -> Entry.t Deferred.Or_error.t

  (** Do something with the contents of an entry within a user-supplied function [f].  Use
      [with_entry] if you expect to be the only user of an [Entry.t] and it is an error if
      the Entry.t is grabbed by another process (or otherwise disappears).  See [checkout]
      for a lower-level interface. *)
  val with_entry
    :  f:(Spoolable.t ->
          ([ `Save of Spoolable.t * Spoolable.Queue.t | `Remove ] * 'a) Deferred.t)
    -> Entry.t
    -> 'a Deferred.Or_error.t

  (** Like [with_entry], but use [with_entry'] if you expect that another process might
      race to grab an [Entry.t] and want straightforward handling.  See [checkout'] for a
      lower-level interface.*)
  val with_entry'
    :  f:(Spoolable.t ->
          ([ `Save of Spoolable.t * Spoolable.Queue.t | `Remove ] * 'a) Deferred.t)
    -> Entry.t
    -> [ `Ok of 'a | `Not_found ] Deferred.Or_error.t

  (** Interface for iteration and waiting on queue activity.  Multiple processes will not
      interfere with one another. *)
  module Queue_reader : sig
    type t

    val create
      :  spool
      -> Spoolable.Queue.t
      -> t Deferred.Or_error.t

    (** Iterate over entries in a queue and call [f] on each, and wait for a new entry if
        the list is exhausted. *)
    val iter
      :  ?stop:unit Deferred.t
      -> f:(Spoolable.t ->
            [ `Save of Spoolable.t * Spoolable.Queue.t | `Remove ] Deferred.t)
      -> t
      -> unit Deferred.Or_error.t

    (** Iterate over entries in a queue and call [f] on each, if any are available.  Do
        not wait. *)
    val iter_available
      :  f:(Spoolable.t ->
            [ `Save of Spoolable.t * Spoolable.Queue.t | `Remove ] Deferred.t)
      -> t
      -> unit Deferred.Or_error.t
  end

  module Expert : sig
    (** A spooled entry that is checked out, independent of any particular queue.  No
        other process using this interface will be able to interfere with a
        [Checked_out_entry.t] (unlike an [Entry.t], which may be stolen out from under
        you). *)
    module Checked_out_entry : sig
      type t

      val contents : t -> Spoolable.t
      val update : t -> f:(Spoolable.t -> Spoolable.t) -> t

      (** Atomically return a [Checked_out_entry.t] to a queue.  The [Checked_out_entry.t]
          should be forgotten after this. *)
      val save : t -> Spoolable.Queue.t -> unit Deferred.Or_error.t

      (** Delete a [Checked_out_entry.t] from disk.  The [Checked_out_entry.t] should be
          forgotten after this. *)
      val remove : t -> unit Deferred.Or_error.t
    end

    (** Check out an [Entry.t].  Use [checkout] if you expect to be the only user of an
        [Entry.t] and it is an error if the Entry.t is grabbed by another process (or does
        not exist).  See [with_entry] for a higher-level interface. *)
    val checkout : Entry.t -> Checked_out_entry.t Deferred.Or_error.t

    (** Check out an [Entry.t].  Use [checkout'] if you expect that another process might
        race to grab an [Entry.t].  See [with_entry'] for a higher-level interface. *)
    val checkout'
      :  Entry.t
      -> [ `Not_found | `Ok of Checked_out_entry.t ] Deferred.Or_error.t

    module Queue_reader : sig
      (** Wait for and dequeue the next entry that appears. *)
      val dequeue
        :  ?stop:unit Deferred.t
        -> Queue_reader.t
        -> [ `Stopped | `Checked_out of (Checked_out_entry.t * Queue_reader.t)] Deferred.Or_error.t

      (** Dequeue the next entry that that is available, if any.  Do not wait. *)
      val dequeue_available
        :  Queue_reader.t
        -> ([ `Nothing_available | `Checked_out of Checked_out_entry.t ] * Queue_reader.t) Deferred.Or_error.t
    end
  end
end
