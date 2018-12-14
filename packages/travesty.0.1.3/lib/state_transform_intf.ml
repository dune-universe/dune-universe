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

open Base

(** [Generic_types] contains generic versions of the types used in
    [Generic_builders] and [Generic_runners]. *)
module type Generic_types = sig
  type ('a, 's) t
  (** [t] is the type of the state monad. *)

  type 'a final
  (** [final] is the type of returned results.  In transformers, this
     becomes ['a Inner.t]; otherwise, it becomes just ['a]. *)

  type 's state
  (** [state] is the type used to represent the state outside of its
     monad.  In [S], ['s state] becomes [x] for some type [x]; in
     [S2], ['s state] becomes ['s]. *)
end

(** [Generic_builders] contains generic versions of the 'builder'
    functions common to all state monad signatures. *)
module type Generic_builders = sig
  include Generic_types

  val make : ('s state -> ('s state * 'a) final) -> ('a, 's) t
  (** [make] creates a context-sensitive computation that can modify
      both the current context and the data passing through. *)

  (** {3 Specialised builders} *)

  val peek : ('s state -> 'a final) -> ('a, 's) t
  (** [peek] creates a context-sensitive computation that can look at
      the current context, but not modify it. *)

  val modify : ('s state -> 's state final) -> (unit, 's) t
  (** [modify] creates a context-sensitive computation that can look at
      and modify the current context. *)

  val return : 'a final -> ('a, 's) t
  (** [return] lifts a value or monad into a stateful computation. *)
end

(** [Generic_runners] contains generic versions of the 'runner'
    functions common to all state monad signatures. *)
module type Generic_runners = sig
  include Generic_types

  val run' : ('a, 's) t -> 's state -> ('s state * 'a) final
  (** [run'] unfolds a [t] into a function from context
      to final state and result. *)

  val run : ('a, 's) t -> 's state -> 'a final
  (** [run] unfolds a [t] into a function from context to final
     result.  To get the final context, use [run'] or call [peek] at
     the end of the computation. *)
end

(** [Fix] contains the signature for fixpoint builders. *)
module type Fix = sig
  type ('a, 's) t

  val fix
    :  f:(('a -> ('a, 's) t) -> 'a -> ('a, 's) t)
    -> 'a
    -> ('a, 's) t
  (** [fix ~f init] builds a fixed point on [f].

      At each step, [f] is passed a continuation [mu] and a value [a].
      It may choose to return a recursive application of [mu], or
      some value derived from [a].

      To begin with, [f] is applied to [mu] and [init]. *)
end

(** [Generic] contains the signature bits common to all
   state transformers. *)
module type Generic = sig
  include Generic_builders with type 'a final := 'a

  module Inner : Monad.S
  (** [Inner] is the monad to which we're adding state. *)

  include Generic_runners with type ('a, 's) t := ('a, 's) t
                           and type 'a final := 'a Inner.t
                           and type 's state := 's state
  (** State transformers have the same runner signatures as
      state monads, but lifted into the inner monad. *)

  include Fix with type ('a, 's) t := ('a, 's) t

  module Monadic : Generic_builders with type 'a state := 'a state
                                     and type 'a final := 'a Inner.t
                                     and type ('a, 's) t := ('a, 's) t
  (** [Monadic] contains a version of the builder interface that
      can interact with the inner monad ([Inner]) this state
      transformer is overlaying. *)
end

(** [S] is the signature of state monad transformers with a fixed
   state type.

    Each [S] computation returns its value inside another monad---for
   example, [Or_error].  We can use this to build computations that
   are both stateful and can fail, for instance. *)
module type S = sig
  type state
  (** The fixed state type. *)

  include Monad.S
  include T_monad.Extensions with type 'a t := 'a t
  include Generic with type ('a, 's) t := 'a t
                   and type 's state := state
  include Monad_transform.S_fixed with type 'a t := 'a t
                                   and module Inner := Inner
end

(** [Basic] is the signature that must be implemented
    by state systems being lifted into [S_transform] instances. *)
module type Basic = sig
  type t
  (** The type of the state. *)

  module Inner : Monad.S
  (** [Inner] is the monad to which we're adding state. *)
end

(** [S2] is the signature of state transformers parametrised over both
   value and state types. *)
module type S2 = sig
  include Monad.S2
  include Generic with type ('a, 's) t := ('a, 's) t
                   and type 's state := 's
end
