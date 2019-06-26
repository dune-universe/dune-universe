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

open Travesty
include Bi_traversable.Chain_Bi2_Traverse1 (Tuple2) (List)

let compose_match (type a b c) (k : a) (v : b) (k' : b) (v' : c)
    ~(equal : b -> b -> bool) : (a * c) option =
  Base.Option.some_if (equal v k') (k, v')

let compose_one (type a b c) (bc : (b, c) t) ((k, v) : a * b)
    ~(equal : b -> b -> bool) : (a * c) list =
  Base.List.filter_map bc ~f:(fun (k', v') -> compose_match k v k' v' ~equal)

let compose (type a b c) (ab : (a, b) t) (bc : (b, c) t)
    ~(equal : b -> b -> bool) : (a, c) t =
  Base.List.concat_map ab ~f:(compose_one bc ~equal)
