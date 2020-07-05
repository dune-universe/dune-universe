(** A module defining a generative functor which creates UID modules. *)

open Jhupllib_pp_utils;;

(** Defines the type of a module supporting UID generation. *)
module type Uid_module =
sig
  (** The type of UIDs in this module. *)
  type t
  (** The type of UID generation contexts in this module. *)
  type context
  (** The default context with which UIDs are created. *)
  val default_context : context
  (** Creates a fresh context from which to generate UIDs.  The UIDs in this
      context are not distinguishable from the UIDs from any other context
      from the same module. *)
  val new_context : unit -> context
  (** Creates a fresh UID.  If a context is provided, it is used; otherwise, the
      default context is used. *)
  val fresh : ?context:context -> unit -> t
  (** Determines if two UIDs are equal. *)
  val equal : t -> t -> bool
  (** Compares two UIDs. *)
  val compare : t -> t -> int
  (** Prints a UID. *)
  val pp : t pretty_printer
  (** Converts a UID to a string. *)
  val show : t -> string
end;;

module Make () : Uid_module;;
