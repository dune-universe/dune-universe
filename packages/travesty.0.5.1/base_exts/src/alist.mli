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

(** Associative-list extensions. *)

(** Defined to let this module be used directly in chaining operations etc. *)
type ('k, 'v) t = ('k, 'v) Base.List.Assoc.t

(** Associative lists are bi-mappable; the left type is keys, and the right
    type is values. For example:

    {[
        bi_map [("foo", 27); ("bar", 53); ("baz", 99)]
          ~left:String.capitalize ~right:Int.neg
        (* returns: [("Foo", -27); ("Bar", -53); ("Baz", -99)] *)
    ]} *)
include Travesty.Bi_mappable.S2 with type ('l, 'r) t := ('l, 'r) t

val compose :
  ('a, 'b) t -> ('b, 'c) t -> equal:('b -> 'b -> bool) -> ('a, 'c) t
(** [compose a b ~equal] produces an associative list that returns [(x, z)]
    for each [(x, y)] in [a] such that a [(y', z)] exists in [b], and
    [equal y y'] is true. *)
