(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019, 2020 by Matt Windsor

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

    These signatures form the inner body of the [On] functors in the main
    signatures. They all have names ending with [_on_applicative] or
    [_on_monad], and assume the existence of a applicative functor or monad
    [M].

    While they aren't interesting on their own, they do contain (in slightly
    abstract form) the specific functions needed to build, and provided on
    building, traversable containers. *)

(** {4:omgeneric The generic signatures}

    As with {!Mappable}, we define some signatures for traversable structures
    in an arity-generic way, then specialise them for arity-0 and arity-1
    types. *)

(** [Basic_generic_on_applicative] describes traversal on either an arity-0
    or arity-1 type.

    - For arity-0 types, ['a t] becomes [t] and ['a elt] becomes [elt].
    - For arity-1 types, ['a t] becomes ['a t] and ['a elt] becomes ['a]. *)
module type Basic_generic_on_applicative = sig
  (** [Generic] refers to the container type as ['a t], and the element type
      as ['a elt]; substitute [t]/[elt] (arity-0) or ['a t]/['a] (arity-1)
      accordingly below. *)
  include Generic_types.Generic

  (** [M] is the applicative functor over which we're fold-mapping. *)
  module M : Applicative.S

  val map_m : 'a t -> f:('a elt -> 'b elt M.t) -> 'b t M.t
  (** [map_m c ~f] maps [f] over every [t] in [c], threading through an
      applicative functor.

      Example:

      {[
        (* Travesty_base_exts.List adds applicative traversals to a list;
           With_errors (in S1_container) implements them on the On_error
           applicative functor. *)

        let f x =
          Or_error.(if 0 < x then error_string "negative!" else ok x)
        in
        List.With_errors.map_m integers ~f
      ]} *)
end

(** [Generic_on_applicative] extends [Generic] to contain various derived
    operators; we use it to derive the signatures of the various [On_*]
    modules.

    - For arity-0 types, ['a t] becomes [t] and ['a elt] becomes [elt].
    - For arity-1 types, ['a t] becomes ['a t] and ['a elt] becomes ['a]. *)
module type Generic_on_applicative = sig
  include Basic_generic_on_applicative

  val iter_m : 'a t -> f:('a elt -> unit M.t) -> unit M.t
  (** [iter_m x ~f] iterates the computation [f] over [x], returning the
      final applicative effect. *)
end

(** [Generic_on_monad] extends [Generic_on_applicative] to contain various
    derived operators that require monads; we use it to derive the signatures
    of the various [On_monad] modules.

    - For arity-0 types, ['a t] becomes [t] and ['a elt] becomes [elt].
    - For arity-1 types, ['a t] becomes ['a t] and ['a elt] becomes ['a]. *)
module type Generic_on_monad = sig
  (** The monad on which these operators are defined. *)
  module M : Monad.S

  (** All applicative operators are available through lowering the monad to
      an applicative functor. *)
  include Generic_on_applicative with module M := Monad_exts.App(M)

  val fold_map_m :
       'a t
    -> f:('acc -> 'a elt -> ('acc * 'b elt) M.t)
    -> init:'acc
    -> ('acc * 'b t) M.t
  (** [fold_map_m c ~f ~init] folds [f] applicatively over every [t] in [c],
      threading through an accumulator with initial value [init]. *)

  val fold_m :
    'a t -> init:'acc -> f:('acc -> 'a elt -> 'acc M.t) -> 'acc M.t
  (** [fold_m x ~init ~f] folds the applicative computation [f] over [x],
      starting with initial value [init], and returning the final value
      inside the applicative effect. *)

  val iter_m : 'a t -> f:('a elt -> unit M.t) -> unit M.t
  (** [iter_m x ~f] iterates the computation [f] over [x], returning the
      final applicative effect. *)

  val mapi_m : f:(int -> 'a elt -> 'b elt M.t) -> 'a t -> 'b t M.t
  (** [mapi_m ~f x] behaves as [map_m], but also supplies [f] with the index
      of the element. This index should match the actual position of the
      element in the container [x]. *)
end

(** {4:ombasic Basic signatures} *)

(** [Basic0_on_applicative] is the inner signature of a traversal over
    arity-0 types. *)
module type Basic0_on_applicative = sig
  include Generic_types.S0

  include
    Basic_generic_on_applicative with type 'a t := t and type 'a elt := elt
end

(** [Basic1_on_applicative] is the inner signature of a traversal over
    arity-1 types. *)
module type Basic1_on_applicative = sig
  (** The type of the container to map over. *)
  type 'a t

  include
    Basic_generic_on_applicative with type 'a t := 'a t and type 'a elt := 'a
end

(** {4:oms Expanded signatures} *)

(** [S1_on_applicative] extends [Generic_on_applicative] with functionality
    that only works on arity-1 containers. *)
module type S1_on_applicative = sig
  type 'a t

  include Generic_on_applicative with type 'a t := 'a t and type 'a elt := 'a

  val sequence_m : 'a M.t t -> 'a t M.t
  (** [sequence_m x] lifts a container of applicatives [x] to an applicative
      containing a container, by sequencing the applicative effects from left
      to right. *)
end

(** [S1_on_monad] extends [Generic_on_monad] with functionality that only
    works on arity-1 containers. *)
module type S1_on_monad = sig
  type 'a t

  include Generic_on_monad with type 'a t := 'a t and type 'a elt := 'a

  val sequence_m : 'a M.t t -> 'a t M.t
  (** [sequence_m x] lifts a container of applicatives [x] to an applicative
      containing a container, by sequencing the applicative effects from left
      to right. *)
end

(** {3 Basic signatures}

    Any traversable type can be turned into a Base container, using the
    applicative fold to implement all container functionality. The unified
    signature of a container with traversals is {!S0} (arity 0) or {!S1}
    (arity 1).

    To satisfy these signatures for new types, implement {!Basic0} or
    {!Basic1}, and use the corresponding [MakeN] functor. Note that you must
    supply an [On] module in terms of applicatives, but not an [On_monad]
    module; previous versions of Travesty required the opposite.

    For types that are _already_ Base containers, or types where custom
    implementation of the Base signature are desired, implement
    {!Basic0_container} or {!Basic1_container}, and use the [MakeN_container]
    functors. *)

(** {4:basic For modules without a [Container] implementation} *)

(** [Basic0] is the minimal signature that traversable containers of arity 0
    must implement to be extensible into {!S0}. *)
module type Basic0 = sig
  (** The container type. *)
  type t

  (** [Elt] contains the element type, which must have equality. *)
  module Elt : Equal.S

  (** [On] implements applicative traversal for a given applicative [M]. *)
  module On (M : Applicative.S) :
    Basic0_on_applicative
      with type t := t
       and type elt := Elt.t
       and module M := M
end

(** [Basic1] is the minimal signature that traversable containers of arity 1
    must implement to be extensible into. *)
module type Basic1 = sig
  (** The container type. *)
  type 'a t

  (** [On] implements traversal for a given applicative functor. *)
  module On (M : Applicative.S) :
    Basic1_on_applicative with type 'a t := 'a t and module M := M
end

(** {4:bcon For modules with a [Container] implementation} *)

(** [Basic0_container] combines {!Basic0} and the Base container signature,
    and is used for extending existing containers into {!S0_container}s. *)
module type Basic0_container = sig
  include Basic0

  include Container.S0 with type t := t and type elt := Elt.t
end

(** [Basic1_container] combines {!Basic1} and the Base container signature,
    and is used for extending existing containers into {!S1_container} s. *)
module type Basic1_container = sig
  include Basic1

  include Container.S1 with type 'a t := 'a t
end

(** {3:s Signatures for traversable containers} *)

(** [Generic] is a generic interface for traversable containers, used to
    build [S0] (arity-0) and [S1] (arity-1). *)
module type Generic = sig
  include Generic_types.Generic

  (** [On] implements traversal operators for a given applicative [M]. *)
  module On (M : Applicative.S) :
    Generic_on_applicative
      with type 'a t := 'a t
       and type 'a elt := 'a elt
       and module M := M

  (** [On_monad] implements traversal operators for a given monad [M].
      Compared to [On(Monad_exts.App(M))], this adds various derived
      operators available only for monads. *)
  module On_monad (M : Monad.S) :
    Generic_on_monad
      with type 'a t := 'a t
       and type 'a elt := 'a elt
       and module M := M

  (** We can do generic container operations. *)
  include Container.Generic with type 'a t := 'a t and type 'a elt := 'a elt

  (** We can do non-applicative mapping operations. *)
  include
    Mappable_types.Generic with type 'a t := 'a t and type 'a elt := 'a elt

  val fold_map :
    'a t -> f:('acc -> 'a elt -> 'acc * 'b elt) -> init:'acc -> 'acc * 'b t
  (** [fold_map c ~f ~init] folds [f] over every [t] in [c], threading
      through an accumulator with initial value [init]. *)

  val mapi : f:(int -> 'a elt -> 'b elt) -> 'a t -> 'b t
  (** [mapi ~f t] maps [f] across [t], passing in an increasing position
      counter. *)

  (** [With_errors] specialises [On_applicative] to the error applicative. *)
  module With_errors :
    Generic_on_monad
      with type 'a t := 'a t
       and type 'a elt := 'a elt
       and module M := Or_error
end

(** [S0] is a generic interface for arity-0 traversable containers. *)
module type S0 = sig
  (** Elements must have equality. While this is an extra restriction on top
      of the Base equivalent, it is required by {{!Traversable.Make0} Make0},
      and helps us define chaining operations. *)
  module Elt : Equal.S

  (** We export [Elt.t] as [elt] for compatibility with Core-style
      containers. *)
  include Generic_types.S0 with type elt = Elt.t

  include Generic with type 'a t := t and type 'a elt := Elt.t

  include Mappable_types.S0_container with type t := t and type elt := Elt.t
end

(** [S1] is a generic interface for arity-1 traversable containers. It also
    includes the extensions from {!Mappable}. *)
module type S1 = sig
  (** ['a t] is the type of the container, parametrised over the element type
      ['a]. *)
  type 'a t

  (** [On] implements applicative folding and mapping operators for a given
      applicative [M], including arity-1 specific operators. *)
  module On (M : Applicative.S) :
    S1_on_applicative with type 'a t := 'a t and module M := M

  (** [On_monad] implements applicative folding and mapping operators for a
      given monad [M], including arity-1 specific operators. *)
  module On_monad (M : Monad.S) :
    S1_on_monad with type 'a t := 'a t and module M := M

  (** [With_errors] is shorthand for [On_monad (Or_error)]. *)
  module With_errors :
    S1_on_monad with type 'a t := 'a t and module M := Or_error

  include
    Generic
      with type 'a t := 'a t
       and type 'a elt := 'a
       and module On := On
       and module On_monad := On_monad
       and module With_errors := With_errors

  include Mappable_types.S1_container with type 'a t := 'a t

  include Mappable_types.Extensions1 with type 'a t := 'a t
end
