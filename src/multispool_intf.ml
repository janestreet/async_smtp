open Core
open Async

(** {0 Multispool}

    Multispool allows multiple, separate processes to cooperate via a filesystem-based
    queue. The design was influenced by various UNIX tools that use typical POSIX-y
    write/fsync/close/rename semantics to provide atomic filesystem operations (Maildir,
    in particular--see http://www.qmail.org/man/man5/maildir.html for an overview).

    One or more processes may place files in a queue, wait for files to appear in queues
    (and handle them), or iterate over files in a queue.

    {1 Usage Summary}

    A spool is physically represented by a directory, and logically as a module created by
    applying the [Multispool] functor. A spool deals in a data type that implements the
    [Spoolable] interface. This interface tells the spool how to encode and decode items
    for on-disk storage and how to map queue names to directories on disk. See
    ../test/lib/widget.ml for an example [Spoolable] implementation.

    An existing spool is opened with [load] and a new one is created with [create].

    Use [enqueue] to add items to a queue, optionally reserving a name beforehand with
    [reserve_name].

    If you want to wait on entries to appear, use [Queue_reader.iter]. If you want to make
    periodic passes over all entries in a queue, use [Queue_reader.iter_available].

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
  (** Generates filenames for enqueued [Spoolable]s. [t] is a user-supplied input to name
      generation via [name]. *)
  module type S = sig
    module Unique_name : sig
      type t

      val to_string : t -> string
    end

    type t

    val next : t -> attempt:int -> Unique_name.t
  end
end

module Spoolable = struct
  module type S = sig
    (** [Spoolable.Metadata.t] should be smallish since it is read and written more
        frequently than [Spoolable.Data.t]. *)
    module Metadata : sig
      type t

      (** [of_string] and [to_string] are used to persist and read [t] on disk. *)
      include Stringable.S with type t := t
    end

    (** [Spoolable.Data.t] is where the "real" data lives and it allows for data-specific
        [load] and [save] functionality. *)
    module Data : sig
      type t

      val load : string -> t Deferred.Or_error.t
      val save : ?temp_file:string -> t -> string -> unit Deferred.Or_error.t
    end

    (** [Queue.t] is an enumerable type that represents the available queues and the
        mapping to directory names on-disk. *)
    module Queue : sig
      type t [@@deriving sexp, enumerate, compare]

      val to_dirname : t -> string
    end

    module Name_generator : Name_generator.S

    (** All operations that touch disk are passed through [Throttle.enqueue] *)
    module Throttle : sig
      val enqueue : (unit -> 'a Deferred.t) -> 'a Deferred.t
    end
  end
end

module type S = sig
  type t [@@deriving sexp_of]
  type spool = t

  module Spoolable : Spoolable.S

  val dir : t -> string

  (** Open a [Multispool.t]. This function will fail by default if the spool directory
      does not exist, does not look like a spool, or does not contain the set of
      directories named after the strings returned by [Spoolable.Queue.to_dir]. Pass
      [~create_if_missing:()] to create the necessary directories.

      Note that, even if [~create_if_missing:()] is specified, this function will still
      fail if the supplied directory is non-empty and not already a spool. *)
  val load : ?create_if_missing:unit -> string -> t Deferred.Or_error.t

  (** Open a [Multispool.t] with no spool directory validation. *)
  val load_unsafe : string -> t

  (** Open a [Multispool.t] and create the spool directory if necessary. This is
      functionally identical to [load ?create_if_missing:()]. *)
  val create : string -> t Deferred.Or_error.t

  (** Provide access to a [Spoolable.Data.t]. [Data_file.t] functions as a "handle" to the
      underlying data so the user can choose when to read a [Spoolable.Data.t]. *)
  module Data_file : sig
    type t

    val path : t -> string
    val load : t -> Spoolable.Data.t Deferred.Or_error.t
    val save : t -> contents:Spoolable.Data.t -> unit Deferred.Or_error.t
    val stat : t -> Unix.Stats.t Deferred.Or_error.t
  end

  (** An [Entry] is associated with a particular queue *)
  module Entry : sig
    type t [@@deriving sexp_of]

    val stat : t -> Unix.Stats.t Deferred.Or_error.t
    val spool : t -> spool
    val queue : t -> Spoolable.Queue.t
    val name : t -> string

    (** create an [Entry.t] from a file name on disk. There is no validation done to
        ensure that the corresponding entry exists in the spool. The validation is
        performed when using the [Entry.t]. *)
    val create : spool -> Spoolable.Queue.t -> name:string -> t

    (** Direct operations that provide no validation or exclusive access guarantees. *)
    module Direct : sig
      (** No checkout is performed and the data is read directly from the queue file. If
          you need to later update this data, consider revalidating the contents after
          checkout and before writing. *)
      val contents : t -> Spoolable.Metadata.t Deferred.Or_error.t

      (** Get the data_file associated with an [Entry.t]. It is unsafe to operate on this
          directly ouside of a checkout, much like [contents]. *)
      val data_file : t -> Data_file.t

      (** Save contents directly to the file path derived from [t]. There are no
          validation or exclusive access guarantees. This will atomically clobber over any
          existing file. *)
      val save : t -> Spoolable.Metadata.t -> unit Deferred.Or_error.t

      (** Delete an [Entry.t] from disk along with its registry file and data_file. There
          are no validation or exclusive access guarantees. *)
      val remove : t -> unit Deferred.Or_error.t
    end
  end

  val list : t -> Spoolable.Queue.t -> Entry.t list Deferred.Or_error.t

  module Unique_name : sig
    val reserve
      :  spool
      -> Spoolable.Name_generator.t
      -> Spoolable.Name_generator.Unique_name.t Deferred.Or_error.t
  end

  (** Add a [Spoolable] to a queue. An [Entry.t] is returned, but it may make sense to
      ignore it. *)
  val enqueue
    :  t
    -> Spoolable.Queue.t
    -> Spoolable.Metadata.t
    -> Spoolable.Data.t
    -> [ `Reserve of Spoolable.Name_generator.t
       | `Use of Spoolable.Name_generator.Unique_name.t
       ]
    -> Entry.t Deferred.Or_error.t

  (** Do something with the contents of an entry within [f]. Use [with_entry] if you
      expect to be the only user of an [Entry.t] and it is an error if the Entry.t is
      grabbed by another process (or otherwise disappears). See [checkout] for a
      lower-level interface. *)
  val with_entry
    :  f:
         (Spoolable.Metadata.t
          -> Data_file.t
          -> ([ `Save of Spoolable.Metadata.t * Spoolable.Queue.t | `Remove ] * 'a)
               Deferred.t)
    -> Entry.t
    -> 'a Deferred.Or_error.t

  (** Like [with_entry], but use [with_entry'] if you expect that another process might
      race to grab an [Entry.t] and want straightforward handling. See [checkout'] for a
      lower-level interface. *)
  val with_entry'
    :  f:
         (Spoolable.Metadata.t
          -> Data_file.t
          -> ([ `Save of Spoolable.Metadata.t * Spoolable.Queue.t | `Remove ] * 'a)
               Deferred.t)
    -> Entry.t
    -> [ `Ok of 'a | `Not_found ] Deferred.Or_error.t

  (** Interface for iteration and waiting on queue activity. Multiple processes will not
      interfere with one another. *)
  module Queue_reader : sig
    type t

    val create : spool -> Spoolable.Queue.t -> t Deferred.Or_error.t

    (** Iterate over entries in a queue and call [f] on each, and wait for a new entry if
        the list is exhausted. *)
    val iter
      :  ?stop:unit Deferred.t
      -> f:
           (Spoolable.Metadata.t
            -> Data_file.t
            -> [ `Save of Spoolable.Metadata.t * Spoolable.Queue.t | `Remove ] Deferred.t)
      -> t
      -> unit Deferred.Or_error.t

    (** Iterate over entries in a queue and call [f] on each, if any are available. Do not
        wait. *)
    val iter_available
      :  f:
           (Spoolable.Metadata.t
            -> Data_file.t
            -> [ `Save of Spoolable.Metadata.t * Spoolable.Queue.t | `Remove ] Deferred.t)
      -> t
      -> unit Deferred.Or_error.t
  end

  module Expert : sig
    (** A spooled entry that is checked out, independent of any particular queue. No other
        process using this interface will be able to interfere with a
        [Checked_out_entry.t] (unlike an [Entry.t], which may be stolen out from under
        you). *)
    module Checked_out_entry : sig
      type t

      val name : t -> string
      val queue : t -> Spoolable.Queue.t
      val contents : t -> Spoolable.Metadata.t
      val update : t -> f:(Spoolable.Metadata.t -> Spoolable.Metadata.t) -> t
      val data_file : t -> Data_file.t

      (** Atomically return a [Checked_out_entry.t] to a queue. The [Checked_out_entry.t]
          should be forgotten after this. *)
      val save : t -> Spoolable.Queue.t -> unit Deferred.Or_error.t

      (** Delete a [Checked_out_entry.t] (along with its registry file and data_file). The
          [Checked_out_entry.t] should be forgotten after this. *)
      val remove : t -> unit Deferred.Or_error.t
    end

    (** Check out an [Entry.t]. Use [checkout] if you expect to be the only user of an
        [Entry.t] and it is an error if the Entry.t is grabbed by another process (or does
        not exist). See [with_entry] for a higher-level interface. *)
    val checkout : Entry.t -> Checked_out_entry.t Deferred.Or_error.t

    (** Check out an [Entry.t]. Use [checkout'] if you expect that another process might
        race to grab an [Entry.t]. See [with_entry'] for a higher-level interface. *)
    val checkout'
      :  Entry.t
      -> [ `Not_found | `Ok of Checked_out_entry.t ] Deferred.Or_error.t

    (** Get a hold of all currently checked out entries in the given [queue]. This
        operation breaks the invariant that each [t] has a single owner. It should only be
        used in cases where it is easy to reason about what processes are potentially
        manipulating the spool. *)
    val list_checkouts_unsafe
      :  spool
      -> Spoolable.Queue.t
      -> Checked_out_entry.t list Deferred.Or_error.t

    module Queue_reader : sig
      (** Wait for and dequeue the next entry that appears. *)
      val dequeue
        :  ?stop:unit Deferred.t
        -> Queue_reader.t
        -> [ `Stopped | `Checked_out of Checked_out_entry.t * Queue_reader.t ]
             Deferred.Or_error.t

      (** Dequeue the next entry that that is available, if any. Do not wait. *)
      val dequeue_available
        :  Queue_reader.t
        -> ([ `Nothing_available | `Checked_out of Checked_out_entry.t ] * Queue_reader.t)
             Deferred.Or_error.t
    end
  end
end

(** [Multispool.Monitor] provides consistency checks for a spool. *)
module Monitor = struct
  module type S = sig
    type t

    module Spoolable : Spoolable.S

    module File_with_mtime : sig
      type t =
        { filename : string
        ; mtime : Time_float.t
        }
      [@@deriving sexp_of]
    end

    module Dir : sig
      type t =
        | Registry
        | Tmp
        | Data
        | Queue of Spoolable.Queue.t
        | Queue_checkout of Spoolable.Queue.t

      val name_on_disk : t -> string
    end

    module Problem : sig
      type t =
        | Too_old of File_with_mtime.t * Dir.t
        | Orphaned of File_with_mtime.t * Dir.t
        | Duplicated of File_with_mtime.t * Dir.t list
      [@@deriving sexp_of, compare]

      include Comparable.S_plain with type t := t
    end

    module Event : sig
      type t =
        | Start of Time_float.t * Problem.t
        | End of Time_float.t * Problem.t
      [@@deriving sexp_of, compare]

      include Comparable.S_plain with type t := t
    end

    module Limits : sig
      type t =
        { max_checked_out_age : Time_float.Span.t (* default: 10 minutes *)
        ; max_tmp_file_age : Time_float.Span.t (* default: 10 minutes *)
        ; max_queue_ages : (Spoolable.Queue.t * Time_float.Span.t) list
        }
      [@@deriving sexp]

      val create
        :  ?max_checked_out_age:Time_float.Span.t
        -> ?max_tmp_file_age:Time_float.Span.t
        -> ?max_queue_ages:(Spoolable.Queue.t * Time_float.Span.t) list
        -> unit
        -> t
    end

    module Spec : sig
      type t =
        { spool_dir : string
        ; limits : Limits.t
        }
      [@@deriving sexp]

      val create : spool_dir:string -> limits:Limits.t -> t
      val param : t Command.Param.t
    end

    (** Does not create a spool. *)
    val create : Spec.t -> t Deferred.Or_error.t

    val run_once : t -> Problem.t list Deferred.Or_error.t

    module Daemon : sig
      type monitor = t

      type t =
        { check_every : Time_float.Span.t (* default: 15 seconds *)
        ; alert_after_cycles : int (* default: 2 cycles *)
        }

      val create : ?check_every:Time_float.Span.t -> ?alert_after_cycles:int -> unit -> t
      val param : t Command.Param.t
      val start : t -> monitor:monitor -> f:(Event.t -> unit Deferred.t) -> unit
    end
  end
end
