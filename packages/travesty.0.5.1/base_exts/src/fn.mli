(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Miscellaneous function combinators.

    [Fn] contains various higher-order functions in the style of Base's [Fn]
    module. *)

(** {2 Constant predicates}

    These are convenience shorthands for [Base.Fn.const]. *)

val always : 'a -> bool
(** [always x] is [true]. *)

val never : 'a -> bool
(** [never x] is [false]. *)

(** {2 Pointwise liftings of operators}

    These complement [Base.Fn.non]. *)

val conj : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** [conj f g] lifts [&&] over predicates [f] and [g]. It is
    short-circuiting: [g] is never called if [f] returns false.

    Examples:

    {[
      let is_zero = Int.(conj is_non_negative is_non_positive)
    ]}

    {[
      (* Short-circuiting: *)
      conj always (fun () -> failwith "oops") (); (* --> exception *)
      conj never  (fun () -> failwith "oops") (); (* --> false *)
    ]} *)

val disj : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** [disj f g] lifts [||] over predicates [f] and [g]. It is
    short-circuiting: [g] is never called if [f] returns true.

    Examples:

    {[
      let is_not_zero = Int.(disj is_negative is_positive)
    ]}

    {[
      (* Short-circuiting: *)
      disj never  (fun () -> failwith "oops") (); (* --> exception *)
      disj always (fun () -> failwith "oops") (); (* --> false *)
    ]} *)

val ( &&& ) : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** [f &&& g] is [conj f g]. *)

val ( ||| ) : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** [f ||| g] is [disj f g]. *)

(** {2 Miscellaneous combinators} *)

val on : ('a -> 'b) -> 'a -> 'a -> f:('b -> 'b -> 'r) -> 'r
(** [on lift x y ~f] lifts a binary function [f] using the lifter [lift]. It
    does the same thing as the `on` function from Haskell, but with
    arguments flipped to make sense without infixing.

    Effectively, it's [Base.Comparable.lift], but with a slightly different
    signature.

    Example:

    {[
      let ints = on fst    ~f:Int.equal (42, "banana") (42, "apple") in
      let strs = on snd ~f:String.equal (42, "banana") (42, "apple") in
      ints, strs (* --> true, false *)
    ]} *)
