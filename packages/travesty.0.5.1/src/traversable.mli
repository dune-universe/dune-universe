(* This file is part of 'travesty'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Signatures and functors for containers and data structures that can be
    mapped across with a monadic side-effect.

    Modules based on this pattern resembles the Haskell Traversable
    typeclass, but with two differences:

    - We currently define traversability in terms of monads, not applicative
      functors (this might change in the future, but monads are generally
      more well-understood and well-supported in the OCaml/Core ecosystem);
    - as a result, the main 'traverse' function is called [map_m]. *)

open Base

(** {2 Signatures} *)

(** @inline *)
include module type of Traversable_intf

(** {2 Making traversable containers}

    Monadic traversal is sufficient to define [fold], and, therefore, all of
    the key functionality for a Base-style container. As such, our
    signatures and functors subsume those for building containers. *)

(** {3 New containers} *)

(** [Make] makes an {{!S0} S0} from a {{!Basic0} Basic0}. *)
module Make0 (I : Basic0) : S0 with module Elt = I.Elt and type t = I.t

(** [Make] makes an {{!S1} S1} from a {{!Basic1} Basic1}. *)
module Make1 (I : Basic1) : S1 with type 'a t = 'a I.t

(** {3 Extending existing containers with monadic traversals} *)

(** [Make0_container] makes an {{!S0} S0} from a
    {{!Basic0_container} Basic0_container}. *)
module Make0_container (I : Basic0_container) :
  S0 with module Elt = I.Elt and type t = I.t

(** [Make1_container] makes an {{!S1} S1} from a
    {{!Basic_container1} Basic1_container}. *)
module Make1_container (I : Basic1_container) : S1 with type 'a t = 'a I.t

(** {2 Combining and modifying traversable containers} *)

(** {3 Chaining} *)

(** [Chain0] chains two {{!S0} S0} instances together, traversing each
    element of the outer instance with the inner instance. *)
module Chain0 (Outer : S0) (Inner : S0 with type t := Outer.Elt.t) :
  S0 with module Elt = Inner.Elt and type t = Outer.t

(** {3 Fixing the element type} *)

(** [Fix_elt (I) (Elt)] demotes an {{!S1} S1} [S] to an {{!S0} S0} by fixing
    the element type to that mentioned in [Elt]. *)
module Fix_elt (I : S1) (Elt : Equal.S) :
  S0 with module Elt = Elt and type t = Elt.t I.t

(** {2 Helper functions} *)

(** Utility functions for building traversals. *)
module Helpers (M : Monad.S) : sig
  (** [traversal] is shorthand for a traversal function over [M]. *)
  type 'a traversal = 'a -> 'a M.t

  (** {3 Variants}

      Functions beginning [proc_variant] are useful for building traversable
      containers on top of Variantslib's [map] function.

      Here's an example where we define a generic traversal function over a
      variant type using {{!proc_variant1} proc_variant1} and
      {{!proc_variant3} proc_variant3}, then use it to build a traversable
      container instance for inspecting and modifying a specific type of
      data regardless of variant.

      {[
        (* This type describes x86 operands: *)
        type t =
          | Location of Location.t
          | Immediate of Disp.t
          | String of string
          | Typ of string
          | Bop of t * operator * t [@@deriving variants]

        (* We use the helpers to build an intermediate mapper... *)
        module Base_map (M : Monad.S) = struct
          module F = Travesty.Traversable.Helpers (M)

          let rec map_m (x : t) ~location ~immediate ~string ~typ ~bop
            : t M.t =
            Variants.map x
              ~location:(F.proc_variant1 location)
              ~immediate:(F.proc_variant1 immediate)
              ~string:(F.proc_variant1 string)
              ~typ:(F.proc_variant1 typ)
              (* Note that this recursively folds down the operands,
                 and that the [bop] function only receives the operator. *)
              ~bop:(F.proc_variant3 (fun (l, b, r) ->
                  let open M.Let_syntax in
                  let%bind l' = map_m ~location ~immediate ~string ~typ ~bop l in
                  let%bind b' = bop b in
                  let%map  r' = map_m ~location ~immediate ~string ~typ ~bop r in
                  (l', b', r')))
          ;;
        end

        (* ...then use it to build a traversable container over all of
           the symbols in an operand. *)
        module On_symbols
          : Travesty.Traversable.S0_container with type t := t
                                               and type elt := string =
          Travesty.Traversable.Make_container0 (struct
            type nonrec t = t
            module Elt = String

            module On_monad (M : Monad.S) = struct
              module B = Base_map (M)
              (* Recursively using other traversables: *)
              module L = Location.On_symbols.On_monad (M)
              module D = Disp.On_symbols.On_monad (M)

              let map_m t ~f =
                B.map_m t
                  ~location:(L.map_m ~f)
                  ~immediate:(D.map_m ~f)
                  (* These don't contain symbols: *)
                  ~string:M.return
                  ~typ:M.return
                  ~bop:M.return
            end
          end)
      ]} *)

  val proc_variant0 : unit traversal -> 'cont Base.Variant.t -> 'cont M.t
  (** [proc_variant0 f variant] lifts a traversal [f] over a Variantslib
      nullary variant constructor [variant]. *)

  val proc_variant1 :
    'a traversal -> ('a -> 'cont) Base.Variant.t -> 'a -> 'cont M.t
  (** [proc_variant1 f variant a] lifts a traversal [f] over a Variantslib
      unary variant constructor [variant] with argument [a]. *)

  val proc_variant2 :
       ('a * 'b) traversal
    -> ('a -> 'b -> 'cont) Base.Variant.t
    -> 'a
    -> 'b
    -> 'cont M.t
  (** [proc_variant2 f variant a b] lifts a traversal [f] over a Variantslib
      binary variant constructor [variant] with arguments [a] and [b]. *)

  val proc_variant3 :
       ('a * 'b * 'c) traversal
    -> ('a -> 'b -> 'c -> 'cont) Base.Variant.t
    -> 'a
    -> 'b
    -> 'c
    -> 'cont M.t
  (** [proc_variant3 f variant a b c] lifts a traversal [f] over a
      Variantslib ternary variant constructor [variant] with arguments [a],
      [b], and [c]. *)

  (** {3 Fields}

      The function [proc_field] is useful for building traversable
      containers on top of Fieldslib's [fold] function.

      Here's an example where we define a generic traversal function over a
      record type using {{!proc_field} proc_field}, then use it to build a
      traversable container instance for inspecting and modifying a specific
      type of data inside the record.

      {[
        (* Type for holding x86 memory references. *)
        type t =
          { seg    : Reg.t   option (* segment register *)
          ; disp   : Disp.t  option (* displacement *)
          ; base   : Reg.t   option (* base register *)
          ; index  : Index.t option (* index *)
          } [@@deriving fields]

        (* First, build a generic traversal function (this isn't,
           itself, a Traversable)... *)
        module Base_map (M : Monad.S) = struct
          module F = Travesty.Traversable.Helpers (M)
          let map_m indirect ~seg ~disp ~base ~index =
            Fields.fold
              ~init:(M.return indirect)
              ~seg:(F.proc_field seg)
              ~disp:(F.proc_field disp)
              ~base:(F.proc_field base)
              ~index:(F.proc_field index)
        end

        (* Now, we can build a traversable container instance.
             This one extracts symbols from memory references. *)
        module On_symbols
          : Travesty.Traversable.S0_container with type t := t
                                               and type elt := string =
          Travesty.Traversable.Make_container0 (struct
            type nonrec t = t
            module Elt = String
            module Set = String.Set

            module On_monad (M : Monad.S) = struct
              module B = Base_map (M)
              module D = Disp.On_symbols.On_monad (M)
              module O = My_option.On_monad (M)

              let map_m t ~f =
                B.map_m t
                  (* Chained monadic traversal. *)
                  ~disp:(O.map_m ~f:(D.map_m ~f))
                  (* Segments, bases, and indices have no symbols. *)
                  ~seg:M.return
                  ~base:M.return
                  ~index:M.return
            end
          end)
      ]} *)

  val proc_field :
       'elt traversal
    -> 'cont M.t
    -> ([> `Set_and_create], 'cont, 'elt) Field.t_with_perm
    -> 'cont M.t
  (** [proc_field f state field container original] lifts a traversal [f] to
      a form comparible with Fieldslib's [fold] function. *)
end
