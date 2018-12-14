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

(** Generic monad extensions.

    [T_monad] contains a signature and functor for adding
    various {{!exts}extensions} to [Core]-style monads. *)

open Base

(** {2:exts Extensions} *)

(** [Extensions] contains extensions for a [Monad.S].

    To create an instance of [Extensions], use {{!Extend}Extend}. *)
module type Extensions = sig
  type 'a t
  (** The type of the extended monad. *)

  val when_m : bool -> f:(unit -> unit t) -> unit t
  (** [when_m predicate ~f] returns [f ()] when [predicate] is true,
     and [return ()] otherwise. *)

  val unless_m : bool -> f:(unit -> unit t) -> unit t
  (** [unless_m predicate ~f] returns [f ()] when [predicate] is
     false, and [return ()] otherwise. *)
end

module Extend (M : Monad.S) : Extensions with type 'a t := 'a M.t
(** [Extend] creates {{!Extensions}Extensions} for a
    [Monad.S]. *)

(** {2:misc Miscellaneous} *)

module S2_to_S (M : Monad.S2) (B : T)
  : Monad.S with type 'a t := ('a, B.t) M.t
(** [S2_to_S] demotes an arity-2 monad [M] to an arity-1 one,
    fixing its second type to be [B.t]. *)
