(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 20014 INRIA                                 *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev:: 559                          $:  Revision of last commit       *)
(*  $Author:: pogodall                  $:  Author of last commit         *)
(*  $Date:: 2014-09-05 13:50:55 +0200 (#$:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

(** This modules implements a functor that build a simple minded
    dependency manager. The later is used to store dependencies and to
    retrieve the data dependeding on an element *)

(** The module signature of the dependency manager *)
module type Manager_sig =
  sig
    (** The type of the dependency manager *)
    type t

    (** The type of the elements which the manager stores the
        dependencies between *)
    type elt

    (** The empty manager *)
    val empty : t

    (** [add elt1 elt2 m] returns a manager similar to [m] except that
        the dependency of [elt1] on [elt2] ([elt1] depends on [elt2])
        has been added *)
    val add_dependency : elt -> elt -> t -> t

    (** [dependencies elt m] returns an ordered list of the elements
        that are dependant from [elt] according to the manager
        [m]. This list is such that if [i_k] and [i_j] belong to the
        list and [k<j] then there is no dependency on [i_j] for
        [i_k].*)
    val dependencies : elt -> t -> elt list

    (** [merge m1 m2] returns a new dependency manager that take into
        accounts all the dependencies in [m1] and in [m2]. If some
        element has dependencies in [m1] and in [m2] then all the
        dependencies remain*)
    val merge : t -> t -> t 

    (** [roots m] returns the list of elements that depend on no other
        element *)
    val roots : t -> elt list
  end


module Make(O:sig type t val compare:t->t->int val to_string:t->string end) : Manager_sig with type elt=O.t
