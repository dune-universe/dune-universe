(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

(** The main groups of signatures provided by this module are:

    - {{!basic} Basic{i n}}: minimal definition of new modules without an
      existing [Container] instance;
    - {{!bcon} Basic{i n} _container}: minimal definition of new modules with
      an existing [Container] instance;
    - {{!s} S{i n}}: full traversable containers, produced by applying
      functors to either of the two groups.

    We also define other signatures, mostly for internal book-keeping. They
    may be useful elsewhere, however. *)

open Base

(** {3 Inner-traversal signatures}

    These signatures form the inner body of the [On_monad] functor in the
    main signatures. They all have names ending with [_on_monad], and assume
    the existence of a monad [M].

    While they aren't interesting on their own, they do contain (in slightly
    abstract form) the specific functions needed to build, and provided on
    building, traversable containers. *)

(** {4:omgeneric The generic signatures}

    As with {{!Mappable} Mappable}, we define some signatures for traversable
    structures in an arity-generic way, then specialise them for arity-0 and
    arity-1 types. *)

(** [Basic_generic_on_monad] describes monadic traversal on either an arity-0
    or arity-1 type.

    - For arity-0 types, ['a t] becomes [t] and ['a elt] becomes [elt].
    - For arity-1 types, ['a t] becomes ['a t] and ['a elt] becomes ['a]. *)
module type Basic_generic_on_monad = sig
  include Generic_types.Generic
  (** [Generic] refers to the container type as ['a t], and the element type
      as ['a elt]; substitute [t]/[elt] (arity-0) or ['a t]/['a] (arity-1)
      accordingly below. *)

  module M : Monad.S
  (** [M] is the monad over which we're fold-mapping. *)

  val map_m : 'a t -> f:('a elt -> 'b elt M.t) -> 'b t M.t
  (** [map_m c ~f] maps [f] over every [t] in [c], threading through monadic
      state.

      Example:

      {[
        (* Travesty_base_exts.List adds monadic traversals to a list;
           With_errors (in S1_container) implements them on the On_error
           monad. *)

        let f x =
          Or_error.(if 0 < x then error_string "negative!" else ok x)
        in
        List.With_errors.map_m integers ~f
      ]} *)
end

(** [Generic_on_monad] extends [Generic] to contain various derived
    operators; we use it to derive the signatures of the various [On_monad]
    modules.

    - For arity-0 types, ['a t] becomes [t] and ['a elt] becomes [elt].
    - For arity-1 types, ['a t] becomes ['a t] and ['a elt] becomes ['a]. *)
module type Generic_on_monad = sig
  include Basic_generic_on_monad

  val fold_map_m :
       'a t
    -> f:('acc -> 'a elt -> ('acc * 'b elt) M.t)
    -> init:'acc
    -> ('acc * 'b t) M.t
  (** [fold_map_m c ~f ~init] folds [f] monadically over every [t] in [c],
      threading through an accumulator with initial value [init]. *)

  val fold_m :
    'a t -> init:'acc -> f:('acc -> 'a elt -> 'acc M.t) -> 'acc M.t
  (** [fold_m x ~init ~f] folds the monadic computation [f] over [x],
      starting with initial value [init], and returning the final value
      inside the monadic effect. *)

  val iter_m : 'a t -> f:('a elt -> unit M.t) -> unit M.t
  (** [iter_m x ~f] iterates the monadic computation [f] over [x], returning
      the final monadic effect. *)

  val mapi_m : f:(int -> 'a elt -> 'b elt M.t) -> 'a t -> 'b t M.t
  (** [mapi_m ~f x] behaves as [mapM], but also supplies [f] with the index
      of the element. This index should match the actual position of the
      element in the container [x]. *)
end

(** {4:ombasic Basic signatures} *)

(** [Basic0_on_monad] is the inner signature of a monadic traversal over
    arity-0 types. *)
module type Basic0_on_monad = sig
  include Generic_types.S0

  include Basic_generic_on_monad with type 'a t := t and type 'a elt := elt
end

(** [Basic1_on_monad] is the inner signature of a monadic traversal over
    arity-1 types. *)
module type Basic1_on_monad = sig
  type 'a t
  (** The type of the container to map over. *)

  include Basic_generic_on_monad with type 'a t := 'a t and type 'a elt := 'a
end

(** {4:oms Expanded signatures} *)

(** [S1_on_monad] extends [Generic_on_monad] with functionality that only
    works on arity-1 containers. *)
module type S1_on_monad = sig
  type 'a t

  include Generic_on_monad with type 'a t := 'a t and type 'a elt := 'a

  val sequence_m : 'a M.t t -> 'a t M.t
  (** [sequence_m x] lifts a container of monads [x] to a monad containing a
      container, by sequencing the monadic effects from left to right. *)
end

(** {3 Basic signatures}

    Any traversable type can be turned into a Core container, using the
    monadic fold to implement all container functionality. The unified
    signature of a container with monadic traversals is {{!S0} S0} (arity 0)
    or {{!S1} S1} (arity 1).

    To satisfy these signatures for new types, implement {{!Basic0} Basic0}
    or {{!Basic1} Basic1}, and use the corresponding [MakeN] functor.

    For types that are _already_ Core containers, or types where custom
    implementation of the Core signature are desired, implement
    {{!Basic0_container} Basic0_container} or {{!Basic1_container}
    Basic1_container}, and use the [MakeN_container] functors. *)

(** {4:basic For modules without a [Container] implementation} *)

(** [Basic0] is the minimal signature that traversable containers of arity 0
    must implement to be extensible into {{!S0} S0}. *)
module type Basic0 = sig
  type t
  (** The container type. *)

  module Elt : Equal.S
  (** [Elt] contains the element type, which must have equality. *)

  (** [On_monad] implements monadic traversal for a given monad [M]. *)
  module On_monad (M : Monad.S) :
    Basic0_on_monad with type t := t and type elt := Elt.t and module M := M
end

(** [Basic1] is the minimal signature that traversable containers of arity 1
    must implement to be extensible into. *)
module type Basic1 = sig
  type 'a t
  (** The container type. *)

  (** [On_monad] implements monadic traversal for a given monad. *)
  module On_monad (M : Monad.S) :
    Basic1_on_monad with type 'a t := 'a t and module M := M
end

(** {4:bcon For modules with a [Container] implementation} *)

(** [Basic0_container] combines {{!Basic0} Basic0} and the Base container
    signature, and is used for extending existing containers into
    {{!S0_container} S0_container} s. *)
module type Basic0_container = sig
  include Basic0

  include Container.S0 with type t := t and type elt := Elt.t
end

(** [Basic1_container] combines {{!Basic1} Basic1} and the Base container
    signature, and is used for extending existing containers into
    {{!S1_container} S1_container} s. *)
module type Basic1_container = sig
  include Basic1

  include Container.S1 with type 'a t := 'a t
end

(** {3:s Signatures for traversable containers} *)

(** [Generic] is a generic interface for traversable containers, used to
    build [S0] (arity-0) and [S1] (arity-1). *)
module type Generic = sig
  include Generic_types.Generic

  (** [On_monad] implements monadic traversal operators for a given monad
      [M]. *)
  module On_monad (M : Monad.S) :
    Generic_on_monad
      with type 'a t := 'a t
       and type 'a elt := 'a elt
       and module M := M

  (** We can do generic container operations. *)
  include Container.Generic with type 'a t := 'a t and type 'a elt := 'a elt

  (** We can do non-monadic mapping operations. *)
  include
    Mappable_types.Generic with type 'a t := 'a t and type 'a elt := 'a elt

  val fold_map :
    'a t -> f:('acc -> 'a elt -> 'acc * 'b elt) -> init:'acc -> 'acc * 'b t
  (** [fold_map c ~f ~init] folds [f] over every [t] in [c], threading
      through an accumulator with initial value [init]. *)

  val mapi : f:(int -> 'a elt -> 'b elt) -> 'a t -> 'b t
  (** [mapi ~f t] maps [f] across [t], passing in an increasing position
      counter. *)

  (** [With_errors] specialises [On_monad] to the error monad. *)
  module With_errors :
    Generic_on_monad
      with type 'a t := 'a t
       and type 'a elt := 'a elt
       and module M := Or_error
end

(** [S0] is a generic interface for arity-0 traversable containers. *)
module type S0 = sig
  module Elt : Equal.S
  (** Elements must have equality. While this is an extra restriction on top
      of the Core equivalent, it is required by {{!Traversable.Make0} Make0},
      and helps us define chaining operations. *)

  include Generic_types.S0 with type elt = Elt.t
  (** We export [Elt.t] as [elt] for compatibility with Core-style
      containers. *)

  include Generic with type 'a t := t and type 'a elt := Elt.t

  include Mappable_types.S0_container with type t := t and type elt := Elt.t
end

(** [S1] is a generic interface for arity-1 traversable containers. It also
    includes the extensions from {{!Mappable} Mappable}. *)
module type S1 = sig
  type 'a t
  (** ['a t] is the type of the container, parametrised over the element type
      ['a]. *)

  (** [On_monad] implements monadic folding and mapping operators for a given
      monad [M], including arity-1 specific operators. *)
  module On_monad (M : Monad.S) :
    S1_on_monad with type 'a t := 'a t and module M := M

  (** [With_errors] is shorthand for [On_monad (Or_error)]. *)
  module With_errors :
    S1_on_monad with type 'a t := 'a t and module M := Or_error

  include
    Generic
      with type 'a t := 'a t
       and type 'a elt := 'a
       and module On_monad := On_monad
       and module With_errors := With_errors

  include Mappable_types.S1_container with type 'a t := 'a t

  include Mappable_types.Extensions1 with type 'a t := 'a t
end
