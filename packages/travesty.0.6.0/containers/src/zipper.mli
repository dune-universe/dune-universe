(* This file is part of 'act'.

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

(** [Zipper] implements the 'list zipper' data structure.

    A list zipper contains two lists: a 'right list', which represents a
    sequence of data to be processed; and a 'left list', which collects the
    sequence of data already processed in reverse order. At any point, the
    first element in the right list---the 'cursor'---represents the item
    currently being processed.

    The usual use case of a zipper is to 'slide' across the right list,
    moving elements onto the left list one by one, and then 'rewind' the
    left list back onto the right for further processing.

    These versions of the list zipper contains a few extensions. First, many
    operations can be parametrised by a monad---this will usually be an
    error monad like [Or_error], but can be anything else (like a state
    transformer).

    Second, items in zippers constructed using {{!Make_marked} Make_marked}
    can be [mark]ed, attaching a tag to them; later on, if the item still
    exists in the zipper, the zipper can be rewound back to the mark using
    [recall]. *)

open Base

(** {2 Signatures}

    For input and output signatures for this module's functors, see
    {{!Zipper_types} Zipper_types}. *)

(** {2 Plain zippers} *)

(** [Plain] is a basic list zipper, without specialised functionality. *)
module Plain : Zipper_types.S

(** {2 Marked zippers} *)

(** [Make_marked] makes a marked zipper from a [Basic_mark]. *)
module Make_marked (Mark : Zipper_types.Basic_mark) :
  Zipper_types.S_marked with type mark := Mark.t

(** [Int_mark_zipper] is a marked zipper whose marks are integers. *)
module Int_mark_zipper : Zipper_types.S_marked with type mark := int
