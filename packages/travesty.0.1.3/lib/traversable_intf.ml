(* This file is part of 'travesty'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Signatures for monadic traversal. *)

open Base

(** {2:generic The generic signature}

    As with {{!Mappable}Mappable}, we define the signature of
    traversable structures in an arity-generic way, then specialise
    it for arity-0 and arity-1 types.
*)

(** [Generic] describes monadic traversal on either an arity-0 or
   arity-1 type.

    - For arity-0 types, use {{!S0}S0}: ['a t] becomes [t], and
      ['a elt] becomes [elt];
    - For arity-1 types, use {{!S1}S1}: ['a t] becomes ['a t],
      and ['a elt] becomes ['a].
*)
module type Generic = sig
  include Types_intf.Generic
  (** [Generic] refers to the container type as ['a t], and the element type
      as ['a elt]; substitute [t]/[elt] (arity-0) or ['a t]/['a] (arity-1)
      accordingly below. *)

  module M : Monad.S
  (** [M] is the monad over which we're fold-mapping. *)

  val map_m : 'a t -> f:('a elt -> 'b elt M.t) -> 'b t M.t
  (** [map_m c ~f] maps [f] over every [t] in [c], threading through
     monadic state. *)
end

(** {2:sigs Basic signatures} *)

(** [S0] is the signature of a monadic traversal over arity-0
   types. *)
module type S0 = sig
  include Types_intf.S0
  include Generic with type 'a t := t and type 'a elt := elt
end

(** [S1] is the signature of a monadic traversal over arity-1
   types. *)
module type S1 = sig
  type 'a t
  (** The type of the container to map over. *)

  (** [S1]s can traverse: when the container type is ['a t],
      the element type is ['a]. *)
  include Generic with type 'a t := 'a t and type 'a elt := 'a
end

(** {2:build Building containers from traversable types} *)

(** {3 Input signatures} *)

(** [Basic_container0] is the signature that traversable containers of
   arity 0 must implement. *)
module type Basic_container0 = sig
  type t
  (** The container type. *)

  module Elt : Equal.S
  (** [Elt] contains the element type, which must have equality. *)

  module On_monad (M : Monad.S) : S0 with type t := t
                                      and type elt := Elt.t
                                      and module M := M
  (** [On_monad] implements monadic traversal for a given monad [M]. *)
end

(** [Basic_container1] is the signature that traversable containers
   of arity 1 must implement. *)
module type Basic_container1 = sig
  type 'a t
  (** The container type. *)

  module On_monad (M : Monad.S) : S1 with type 'a t := 'a t and module M := M
  (** [On_monad] implements monadic traversal for a given monad. *)
end

(** {3 Helper signatures} *)

(** [Generic_on_monad] extends [Generic] to contain various derived
   operators; we use it to derive the signatures of the various
   [On_monad] modules. *)
module type Generic_on_monad = sig
  include Generic

  val fold_map_m
    :  'a t
    -> f    : ('acc -> 'a elt -> ('acc * 'b elt) M.t)
    -> init : 'acc
    -> ('acc * 'b t) M.t
  (** [fold_map_m c ~f ~init] folds [f] monadically over every [t] in
     [c], threading through an accumulator with initial value
     [init]. *)

  val fold_m
    :  'a t
    -> init : 'acc
    -> f    : ('acc -> 'a elt -> 'acc M.t)
    -> 'acc M.t
  (** [fold_m x ~init ~f] folds the monadic computation [f] over [x],
      starting with initial value [init], and returning the final
      value inside the monadic effect. *)

  val iter_m : 'a t -> f:('a elt -> unit M.t) -> unit M.t
  (** [iter_m x ~f] iterates the monadic computation [f] over [x],
      returning the final monadic effect. *)

  val mapi_m : f:(int -> 'a elt -> 'b elt M.t) -> 'a t -> 'b t M.t
  (** [mapi_m ~f x] behaves as [mapM], but also supplies [f] with the
      index of the element.  This index should match the actual
      position of the element in the container [x]. *)
end

(** [On_monad1] extends [Generic_on_monad] with functionality that
    only works on arity-1 containers. *)
module type On_monad1 = sig
  type 'a t

  include Generic_on_monad with type 'a t := 'a t and type 'a elt := 'a

  val sequence_m : 'a M.t t -> 'a t M.t
  (** [sequence_m x] lifts a container of monads [x] to a monad
      containing a container, by sequencing the monadic effects from
      left to right. *)
end

(** [Generic_container] is a generic interface for traversable
   containers, used to build [Container0] (arity-0) and [Container1]
   (arity-1). *)
module type Generic_container = sig
  include Types_intf.Generic

  module On_monad
    : functor (M : Monad.S)
      -> Generic_on_monad with type 'a t   := 'a t
                           and type 'a elt := 'a elt
                           and module M := M
  (** [On_monad] implements monadic traversal operators for
      a given monad [M]. *)

  include Container.Generic with type 'a t   := 'a t
                             and type 'a elt := 'a elt
  (** We can do generic container operations. *)

  include Mappable.Generic with type 'a t   := 'a t
                            and type 'a elt := 'a elt
  (** We can do non-monadic mapping operations. *)

  val fold_map
    :  'a t
    -> f    : ('acc -> 'a elt -> ('acc * 'b elt))
    -> init : 'acc
    -> ('acc * 'b t)
  (** [fold_map c ~f ~init] folds [f] over every [t] in [c], threading
     through an accumulator with initial value [init]. *)

  val mapi : f : (int -> 'a elt -> 'b elt) -> 'a t -> 'b t
  (** [mapi ~f t] maps [f] across [t], passing in an increasing
      position counter. *)


  module With_errors : Generic_on_monad with type 'a t := 'a t
                                         and type 'a elt := 'a elt
                                         and module M := Or_error
  (** [With_errors] specialises [On_monad] to the error monad. *)
end

(** {3 Signatures for traversable containers} *)

(** [S0_container] is a generic interface for arity-0 traversable
    containers. *)
module type S0_container = sig
  include Types_intf.S0
  include Generic_container with type 'a t := t and type 'a elt := elt
  include Mappable.S0_container with type t := t and type elt := elt
end

(** [S1_container] is a generic interface for arity-1 traversable
    containers.  It also includes the extensions from {{!Mappable}Mappable}. *)
module type S1_container = sig
  (** ['a t] is the type of the container, parametrised over the
      element type ['a]. *)
  type 'a t

  module On_monad (M : Monad.S)
    : On_monad1 with type 'a t := 'a t and module M := M
  (** [On_monad] implements monadic folding and mapping operators for
      a given monad [M], including arity-1 specific operators. *)

  module With_errors : On_monad1 with type 'a t := 'a t
                                  and module M := Or_error
  (** [With_errors] is shorthand for [On_monad (Or_error)]. *)

  include Generic_container with type 'a t := 'a t
                             and type 'a elt := 'a
                             and module On_monad := On_monad
                             and module With_errors := With_errors
  ;;

  include Mappable.S1_container with type 'a t := 'a t
  include Mappable.Extensions1 with type 'a t := 'a t

  module With_elt (Elt : Equal.S)
    : S0_container with type t := Elt.t t and type elt := Elt.t
  (** [With_elt (Elt)] demotes this [S1_container] to a
      {{!S0_container}S0_container} by fixing the element type to that mentioned
      in [Elt]. *)
end
