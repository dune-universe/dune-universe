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

(** Miscellaneous function combinators.

    [T_fn] contains various higher-order functions in the style of
    Core_kernel's [Fn] module.
 *)

val on
  :  ('a -> 'b)
  -> ('b -> 'b -> 'r)
  -> ('a -> 'a -> 'r)
(** [on lift f] lifts a binary function [f] using the lifter [lift].
    It does the same thing as the `on` function from Haskell, but
    with arguments flipped to make sense without infixing.

    Example:

    {[
      let ints = on fst    Int.equal (42, "banana") (42, "apple") in
      let strs = on snd String.equal (42, "banana") (42, "apple") in
      ints, strs (* --> true, false *)
    ]}
*)

val conj : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** [conj f g] lifts [&&] over predicates [f] and [g].
    It is short-circuiting: [g] is never called if [f] returns
    false.

    Examples:

    {[
      let is_zero = Int.(conj is_non_negative is_non_positive)
    ]}

    {[
      (* Short-circuiting: *)
      conj (fun () -> true)  (fun () -> failwith "oops") (); (* --> exception *)
      conj (fun () -> false) (fun () -> failwith "oops") (); (* --> false *)
    ]}
*)

val disj : ('a -> bool) -> ('a -> bool) -> 'a -> bool
(** [disj f g] lifts [||] over predicates [f] and [g].
    It is short-circuiting: [g] is never called if [f] returns
    true.

    Examples:

    {[
      let is_not_zero = Int.(disj is_negative is_positive)
    ]}

    {[
      (* Short-circuiting: *)
      disj (fun () -> false) (fun () -> failwith "oops") (); (* --> exception *)
      disj (fun () -> true)  (fun () -> failwith "oops") (); (* --> false *)
    ]}
*)
