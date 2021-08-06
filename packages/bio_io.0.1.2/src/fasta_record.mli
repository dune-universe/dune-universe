(** A record type for FASTA files.

    {1 Overview}

    {2 Example 1}

    If you have a fasta file something like this:

    {[
      >s1 apple pie
      ACTG
      actg
    ]}

    Then you would get a [record] something like this:

    {[
      Fasta_record.id record (* "s1" *) Fasta_record.desc record
        (* Some "apple pie" *) Fasta_record.seq record
      (* "ACTGactg" *)
    ]}

    {2 Example 2}

    If you have a fasta file something like this:

    {[
      >s1
      ACTG
      actg
    ]}

    Then you would get a [record] something like this:

    {[
      Fasta_record.id record (* "s1" *) Fasta_record.desc record (* None *)
        Fasta_record.seq record
      (* "ACTGactg" *)
    ]}

    {2 Example 3}

    To change a part of the [Fasta_record] use the [with_*] functions. E.g.,

    {[ Fasta_record.with_id "apple" record ]}

    would change give you a [t] with the [id] set to ["apple"]. *)

open! Core_kernel

(** {1 API} *)

type t [@@deriving sexp]

exception Exn of string [@@deriving sexp]

val create : id:string -> desc:string option -> seq:string -> t
(** [create ~id ~desc ~seq] creates a new [t]. Shouldn't raise as literally any
    values of the correct type are accepted. *)

val of_header_exn : string -> t
(** [of_header_exn header] returns a [t] from a FASTA header. May raise
    exceptions. Used internally for parsing FASTA files, but the code consuming
    the [bio_io] module probably won't need to use this function. *)

val of_header : string -> t Or_error.t
(** [of_header header] is like [of_header_exn header] except that it returns
    [Or_error.t] rather than raising exceptions. *)

val to_string : t -> string
(** [to_string t] returns a string representation of [t] ready to print to a
    FASTA output file. *)

val serialize : t -> string
(** [serialize t] returns the [Sexp] of [t] as a string. *)

val equal : t -> t -> bool
(** [equal this other] returns [true] if all fields of two [t]s are the same. *)

val ( = ) : t -> t -> bool
(** [Fasta_record.(this = other)] is like [equal this other]. *)

val id : t -> string
(** [id t] returns the [id] of the [t]. *)

val desc : t -> string option
(** [desc t] returns the [desc] (description) of the [t]. *)

val seq : t -> string
(** [seq t] returns the [seq] of the [t]. *)

val with_id : string -> t -> t
(** [with_id new_id t] returns a [t] with [new_id] instead of the original [id]. *)

val with_seq : string -> t -> t
(** [with_seq new_seq t] returns a [t] with [new_seq] instead of the original
    [seq]. *)

val with_desc : string option -> t -> t
(** [with_desc new_desc t] returns a [t] with [new_desc] instead of the original
    [desc]. *)
