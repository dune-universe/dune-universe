open Batteries;;
open Pds_reachability_work;;

(** This module type describes the interface for a work collection. *)
module type Work_collection =
sig
  (* The module defining the work type. *)
  module W : Work_type

  (** The type of a work collection. *)
  type work_collection

  (** An equality test for work collections. *)
  val equal_work_collection : work_collection -> work_collection -> bool

  (** A comparator for work collections. *)
  val compare_work_collection : work_collection -> work_collection -> int

  (** A pretty-printer for work collections. *)
  val pp_work_collection : Format.formatter -> work_collection -> unit

  (** A conversion from work collections to strings. *)
  val show_work_collection : work_collection -> string


  (** An empty work collection. *)
  val empty : work_collection

  (** Adds a work unit to a work collection. *)
  val offer : W.t -> work_collection -> work_collection

  (** Extracts a work unit from a work collection.  The only guaratees made by
      this routine are as follows:

      - Each offered work element can be taken exactly once.
      - A work element is always provided unless the collection is empty.

      In particular, there are no guarantees made about the order in which the
      offered elements are taken.
  *)
  val take : work_collection -> work_collection * W.t option

  (** Determines if a work collection is empty. *)
  val is_empty : work_collection -> bool

  (** Determines the number of elements to be taken from a given work
      collection. *)
  val size : work_collection -> int

(** Enumerates the elements in a work collection in the order in which they
    would be taken. *)
  val enum : work_collection -> W.t Enum.t

(** Exports a work collection as JSON. *)
  val to_yojson : work_collection -> Yojson.Safe.json
end;;

(** This module type describes a work collection implementation.  Such an
    implementation will, given a work type, produce a work collection. *)
module type Work_collection_template =
  functor (W : Work_type) -> Work_collection with module W = W
