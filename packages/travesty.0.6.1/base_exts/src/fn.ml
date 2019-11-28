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

let on (type a b c) (lift : a -> b) (x : a) (y : a) ~(f : b -> b -> c) : c =
  f (lift x) (lift y)

let conj (type a) (f : a -> bool) (g : a -> bool) (x : a) : bool = f x && g x

let ( &&& ) (type a) (f : a -> bool) (g : a -> bool) : a -> bool = conj f g

let disj (type a) (f : a -> bool) (g : a -> bool) (x : a) : bool = f x || g x

let ( ||| ) (type a) (f : a -> bool) (g : a -> bool) : a -> bool = disj f g

let always (type a) (_ : a) : bool = true

let never (type a) (_ : a) : bool = false

module Compose_syntax = struct
  let ( >> ) (type a b c) (f : a -> b) (g : b -> c) : a -> c =
    Base.Fn.compose g f
end
