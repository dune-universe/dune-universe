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

(** Extension signatures for containers.

    This module contains various signatures that enumerate extensions that
    apply to Core containers of both {{!S0} arity 0} and {{!S1} arity 1}. *)

open Base

(** {2:generic Generic extensions}

    As is often the case in [Travesty], we define an arity-generic signature
    first, then specialise it for arity-0 and arity-1 containers. *)

module type Generic = sig
  (** [Generic_extensions] refers to the container type as ['a t], and the
      element type as ['a elt]; substitute [t]/[elt] (arity-0) or
      ['a t]/['a] (arity-1) accordingly below. *)
  include Generic_types.Generic

  (** {3 Testing for a specific number of elements}

      The following functions help in checking whether a container has a
      particular, commonly-required number of elements (zero or one, one,
      two, and so on). *)

  val at_most_one : 'a t -> 'a elt option Or_error.t
  (** [at_most_one xs] returns [Ok None] if [xs] is empty; [Ok Some(x)] if
      it contains only [x]; and an error otherwise.

      Examples (using
      {{!Travesty_base_exts.List} an extended version of List}):

      {[
        List.at_most_one []     (* ok None *)
             at_most_one [1]    (* ok (Some 1) *)
             at_most_one [1; 2] (* error -- too many *)
      ]} *)

  val one : 'a t -> 'a elt Or_error.t
  (** [one xs] returns [Ok x] if [xs] contains only [x], and an error
      otherwise.

      Examples (using
      {{!Travesty_base_exts.List} an extended version of List}):

      {[
        List.one []     (* error -- not enough *)
             one [1]    (* ok 1 *)
             one [1; 2] (* error -- too many *)
      ]} *)

  val two : 'a t -> ('a elt * 'a elt) Or_error.t
  (** [two xs] returns [Ok (x, y)] if [xs] is a list containing only [x] and
      [y] in that order, and an error otherwise.

      Examples (using
      {{!Travesty_base_exts.List} an extended version of List}):

      {[
        List.two []        (* error -- not enough *)
             two [1]       (* error -- not enough *)
             two [1; 2]    (* ok (1, 2) *)
             two [1; 2; 3] (* error -- too many *)
      ]} *)

  (** {3 Miscellaneous extensions} *)

  val max_measure : measure:('a elt -> int) -> ?default:int -> 'a t -> int
  (** [max_measure ~measure ~default xs] measures each item in [xs]
      according to [measure], and returns the highest measure reported. If
      [xs] is empty, return [default] if given, and [0] otherwise. *)
end

(** {3 Containers of predicates}

    The following functions concern containers of predicates (functions of
    type ['a -> bool]). *)

module type Generic_predicate = sig
  (** The generic type of predicate containers. *)
  type 'a t

  (** The generic type of predicate target elements. *)
  type 'a item

  val any : 'a item -> predicates:'a t -> bool
  (** [any x ~predicates] tests [x] against [predicates] until one returns
      [true], in which case it returns [true]; or all return [false], in
      which case it returns [false]. *)

  val all : 'a item -> predicates:'a t -> bool
  (** [any x ~predicates] tests [x] against [predicates] until one returns
      [false], in which case it returns [false]; or all return [true], in
      which case it returns [true]. *)

  val none : 'a item -> predicates:'a t -> bool
  (** [none x ~predicates] is the same as [any x] with all predicates in
      [predicates] negated. It tests [x] against [predicates] until one
      returns [true], in which case it returns [false]; or all return
      [false], in which case it returns [true]. *)
end

(** {2:a0 Arity-0 container extensions}

    These extensions target arity-0 containers (implementations of
    [Container.S0]). *)

(** Extensions for a [Container.S0].

    This signature contains the generic extensions outlined in
    {{!Generic_extensions} Generic_extensions}. *)
module type S0 = sig
  include Generic_types.S0

  include Generic with type 'a t := t and type 'a elt := elt
end

(** Extensions for a [Container.S0] whose elements are predicates.

    This signature extends and constrains {{!Extensions0} Extensions0}. *)
module type S0_predicate = sig
  (** Type of predicate containers *)
  type t

  (** Type of items being tested against predicates. *)
  type item

  include S0 with type t := t and type elt := item -> bool

  include Generic_predicate with type 'a t := t and type 'a item := item
end

(** {2:a1 Arity-1 container extensions}

    These extensions target arity-1 containers (implementations of
    [Container.S1]). *)

(** Extensions for a [Container.S1].

    This signature contains both the generic extensions outlined in
    {{!Generic_extensions} Generic_extensions} as well as extensions that
    require the ability to change the element type mid-flight. *)
module type S1 = sig
  (** The type of the container to extend. *)
  type 'a t

  include Generic with type 'a t := 'a t and type 'a elt := 'a

  (** Predicate extensions are available on all arity-1 containers, provided
      that we fix the element type parameter to ['a -> bool]. *)
  include
    Generic_predicate
    with type 'a t := ('a -> bool) t
     and type 'a item := 'a
end
