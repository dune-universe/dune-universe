(**
   This module provides a means by which a registry of "witnesses" can be
   created.  Here, a witness is a representative of another (typically more
   complex) value.  A witness registry is a mutable structure which
   monotonically accumulates values, mapping distinct values to distinct
   witnesses.  The primary use of such a registry is to accelerate comparison
   operations.  For instance, using witnesses as keys in a tree-based dictionary
   may be faster than using the original values if the comparison between two
   values is an expensive operation.
*)

open Batteries;;

(**
   This module type describes the information required to create a witness
   registry module.
*)
module type Spec =
sig
  type t
  val compare : t -> t -> int
end;;

(**
   This is the type of a witness registry module.
*)
module type Registry =
sig
  (** The type of a witness registry. *)
  type t

  (** The type of elements stored in the registry. *)
  type elt

  (** The type of a witness in the registry. *)
  type witness

  (** A function to produce an empty witness registry.  Registries use mutable
      data structures to cache results, so each empty registry must be created
      separately. *)
  val empty_registry : unit -> t

  (** Obtains a witness for the provided value.  If the value already has a
      witness, it is returned; otherwise, a new witness is created and
      returned.  If the same element is added to two different registries, it
      will not be given the same witness for each. *)
  val witness_of : t -> elt -> witness

  (** Obtains a value for the provided witness.  Raises Not_found if no such
      witness is stored in the provided registry. *)
  val element_of : t -> witness -> elt

  (** Determines if two witnesses are equal.  Two witnesses are equal only if
      they witness the same value. *)
  val equal_witness : witness -> witness -> bool

  (** Compares two witnesses.  This comparison operation is arbitrary; although
      the element type must be comparable, there is no guarantee of a connection
      between the comparison of elements and the comparison of their
      witnesses. *)
  val compare_witness : witness -> witness -> int
end;;

(**
   A functor which creates witness registries.
*)
module Make(S : Spec) : Registry with type elt = S.t =
struct
  type elt = S.t;;
  type witness = int;;

  let equal_witness : int -> int -> bool =
    (* This use of "==" (instead of "=") is intentional: OCaml's == is a single
       machine instruction and, on primitive integers, identity is equality. *)
    (==)
  ;;

  let compare_witness : int -> int -> int =
    Pervasives.compare
  ;;

  module Witness_ord =
  struct
    type t = witness
    let compare = compare_witness
  end;;

  module Witness_map = Map.Make(Witness_ord);;

  module Element_map = Map.Make(S);;

  let next_available_witness : witness ref = ref 0;;

  type t =
    { witness_to_value : elt Witness_map.t ref;
      value_to_witness : witness Element_map.t ref
    }
  ;;

  let empty_registry () =
    { witness_to_value = ref Witness_map.empty;
      value_to_witness = ref Element_map.empty;
    }
  ;;

  let witness_of (r : t) (x : elt) : witness =
    match Element_map.Exceptionless.find x !(r.value_to_witness) with
    | None ->
      let w = !next_available_witness in
      next_available_witness := w + 1;
      r.value_to_witness := Element_map.add x w !(r.value_to_witness);
      r.witness_to_value := Witness_map.add w x !(r.witness_to_value);
      w
    | Some w -> w
  ;;

  let element_of (r : t) (w : witness) : elt =
    Witness_map.find w !(r.witness_to_value)
  ;;
end;;

(** The type of a registry with an escort.  Escorts pair the witnesses with
    their registries to make operations such as pretty-printing easier.  This
    module only defines escorts and their basic comparison operations.  More
    operations can be added by including utils modules produced by the other
    functors in this module. *)
module type Escorted_registry =
sig
  include Registry;;
  type escorted_witness
  val witness_of_escorted_witness : escorted_witness -> witness
  val registry_of_escorted_witness : escorted_witness -> t
  val element_of_escorted_witness : escorted_witness -> elt
  val escorted_witness_of : t -> elt -> escorted_witness
  val share_escort : escorted_witness -> elt -> escorted_witness
  val equal_escorted_witness : escorted_witness -> escorted_witness -> bool
  val compare_escorted_witness : escorted_witness -> escorted_witness -> int
end;;

(** A functor to make registries with escorts. *)
module Make_escorted(S : Spec) : Escorted_registry with type elt = S.t =
struct
  module Escorted_registry = Make(S);;
  include Escorted_registry;;
  type escorted_witness = t * witness;;
  let witness_of_escorted_witness (_,w) = w;;
  let registry_of_escorted_witness (r,_) = r;;
  let element_of_escorted_witness (r,w) = element_of r w;;
  let escorted_witness_of r e = (r, witness_of r e);;
  let share_escort (r,_) e = (r, witness_of r e);;
  let equal_escorted_witness (_,w1) (_,w2) =
    equal_witness w1 w2
  ;;
  let compare_escorted_witness (_,w1) (_,w2) =
    compare_witness w1 w2
  ;;
end;;

(** The type of a pretty-printing utility module for witness registries. *)
module type Pp_utils =
sig
  type escorted_witness

  (** A pretty printer for escorted witnesses (given a pretty printer for their
      values. *)
  val pp_escorted_witness : escorted_witness Jhupllib_pp_utils.pretty_printer
end;;

(** A functor to produce a pretty-printing utility module. *)
module Make_pp
    (R : Escorted_registry)
    (P : Jhupllib_pp_utils.Pp with type t = R.elt)
  : Pp_utils with type escorted_witness := R.escorted_witness =
struct
  let pp_escorted_witness fmt ew =
    P.pp fmt @@ R.element_of
      (R.registry_of_escorted_witness ew)
      (R.witness_of_escorted_witness ew)
  ;;
end;;

(** The type of a to-yojson utility module for witness registries. *)
module type To_yojson_utils =
sig
  type escorted_witness

  (** A pretty printer for escorted witnesses (given a pretty printer for their
      values. *)
  val escorted_witness_to_yojson : escorted_witness -> Yojson.Safe.json
end;;

(** A functor to produce a pretty-printing utility module. *)
module Make_to_yojson
    (R : Escorted_registry)
    (Y : Jhupllib_yojson_utils.To_yojson_type with type t = R.elt)
  : To_yojson_utils with type escorted_witness := R.escorted_witness
=
struct
  let escorted_witness_to_yojson ew =
    Y.to_yojson @@ R.element_of
      (R.registry_of_escorted_witness ew)
      (R.witness_of_escorted_witness ew)
  ;;
end;;
