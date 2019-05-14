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

(** An expanded version of [Core_kernel]'s pair (2-tuple) module.

    This module expands and merges both [Core_kernel.Tuple2] and
    {{!Travesty.Base_exts.Tuple2} Base_exts.Tuple2}.

    This expanded overlay contains a {{!Travesty.Bi_mappable} bi-mappable}
    implementation for pairs *)

(** Type of 2-tuples. *)
type ('l, 'r) t = 'l * 'r

(** Pairs are trivially bi-mappable; the left type is [fst], and the right
    type is [snd]. For example:

    {[
      bi_map ("foo", 27) ~left:String.capitalize ~right:Int.neg
      (* returns: ("Foo", -27) *)
    ]} *)
include Travesty.Bi_mappable.S2 with type ('l, 'r) t := ('l, 'r) t
