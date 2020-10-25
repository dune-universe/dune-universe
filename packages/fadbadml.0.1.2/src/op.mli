(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

(** Predefined operator modules *)

(** [float] operators.
    All the operators are aliases for the ones found in [Pervasives].  *)
module Float : Types.OpS with type elt = float and type scalar = float

(** [float] operators with comparison. Extends {!Op.Float}. *)
module OrderedFloat : Types.OrderedOpS
                      with type elt = float
                      and type scalar = float
