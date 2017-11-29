open! Core
open! Async
open! Async_smtp_types

module Config : sig
  module Header_cond : sig
    type t =
      { name : Email_headers.Name.t;
        if_  : [ `Contains of string ] sexp_option;
      } [@@deriving sexp]
    ;;
  end

  module Listed_header_cond : sig
    type t =
      { name : Email_headers.Name.t;
        if_ : [ `Contains of string ] sexp_option;
        remove_duplicates : unit sexp_option;
      } [@@deriving sexp]
    ;;
  end

  type t =
    { strip_whitespace      : unit sexp_option;
      normalize_whitespace  : Header_cond.t sexp_list;
      filter                : Header_cond.t sexp_list;
      mask                  : Header_cond.t sexp_list;
      hash                  : Header_cond.t sexp_list;
      dedup                 : Header_cond.t sexp_list;
      (* read in as list of emails and sort *)
      sort_emails           : Listed_header_cond.t sexp_list;
      (* read in as list of whitespace-separated words and sort *)
      sort_words            : Listed_header_cond.t sexp_list;
      sort                  : sexp_bool;
    } [@@deriving sexp]
  ;;

  val default : t

  val load : string -> t Deferred.t
end

module Header : sig
  type t = (Email_headers.Name.t * Email_headers.Value.t) [@@deriving compare]
end

val transform : Config.t -> Smtp_envelope.t -> Smtp_envelope.t
