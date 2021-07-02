(** {1 Bits Providers}

   This module contains the signatures of the modules that can be given to the
   various functors in {!MakeRandom}, specifying how to provide bits. *)

(** {2 Basic}

   Degenerated, stateless bits provider. This is basically only a function
   [bits] taking nothing as argument and returning a random value, either
   30-bits with {!Basic30} or 64-bits with {!Basic64}. When using such an
   interface, all the initialisation, state manipulation, etc., must be done
   outside of the functor. It is therefore preferable to provide a full
   interface (see below). *)

module type Basic30 = sig
  val bits : unit -> int
  (** Must return a random number on 30 bits, that is between 0 (inclusive) and
     [2^30-1] (exclusive). *)
end

module type Basic64 = sig
  val bits : unit -> int64
  (** Similar to {!Basic30.bits} except it returns an [int64] number. All the 64
     bits must be random. *)
end

(** {2 State}

   Bits provider taking a state as input. This is basically the pair of an
   abstract type [state] and a function [bits] taking the state as argument and
   returning a random value, either 30-bits with {!State30} or 64-bits with
   {!State64}. *)

module type State30 = sig
  type state
  (** The type of PRNG states. *)

  val bits : state -> int
  (** Provide 30 random bits in an integer. *)
end

module type State64 = sig
  type state

  val bits : state -> int64
  (** Similar to {!State30.bits} except it returns an [int64] number. All the 64
     bits must be random. *)
end

(** {2 Init}

   State-manipulation functions. If some of them are very similar to the
   signature of {!Stdlib.Random} (and thus not bringing much improvement), there
   is one big change which lies within these [init_size] and [init] values: they
   are a convenience to build the {!Stdlib.Random.full_init} function.

   The [full_init] function takes a state and an array of {b any size}
   containing {b any kind} of integers (in particular, the array can be empty or
   it can contain only very-low-entropy integers). Instead, this functor
   requires a value [init_size] stating how many values you will need and a
   function [init] taking a state and an array of values of initialising the
   state from it. The difference is that the array of values (either 30-bits in
   {!Init30} or 64-bits in {!Init64}) is always of size [init_size] and all the
   values have already "rather-good"-entropy (that is, as much as possible given
   the initial seed).

   Finally, instead of giving a default state, we only ask for a default seed
   from which the default state will be initialised. Any value can be used here
   (we like to use 135801055; can you guess why?). *)

module type Init30 = sig
  type state

  val new_state : unit -> state
  (** Create a new non-initialised state. *)

  val assign : state -> state -> unit
  (** [assign s1 s2] copies [s2] into [s1]. *)

  val init_size : int
  (** Required size for the array in {!init}. *)

  val init : state -> int array -> unit
  (** Initialise the state based on the values in the array. The array contains
     {!init_size} random 30-bits integers. *)

  val default_seed : int
  (** Seed that will be given to {!init} to generate the default state. *)
end

module type Init64 = sig
  type state

  val new_state : unit -> state
  val assign : state -> state -> unit

  val init_size : int

  val init : state -> int64 array -> unit
  (** Same as {!Init30.init} except receives an array of [int64] where all the
     bits are random. *)

  val default_seed : int
end

(** {2 Full}

   Full providers. These are only the combination of a bits provider (eg.
   {!State30}) and state-manipulation functions (eg. {!Init30}). The common uses
   would only require {!Full30} or {!Full64} depending on whether the PRNG
   manipulates values of 30 or 64 bits (usually, the output value is of same
   precision as the state).

   Some other functors are provided if the state and the bits provider differ.
   This is for instance useful if the bits provider returns 64-bits outputs and
   is written in C. In this case, it is more interesting to turn it into a
   30-bits provider in C and bind this 30-bits function (because 30-bits
   integers are faster to convert to OCaml than 64-bits integers). In order to
   limit the code duplication, one can keep the state manipulation functions as
   64 bits and, in such a situation, we end up providing {!Full30Init64}. *)

module type Full30 = sig
  type state
  include State30 with type state := state
  include Init30 with type state := state
end

module type Full64 = sig
  type state
  include State64 with type state := state
  include Init64 with type state := state
end

module type Full30Init64 = sig
  type state
  include State30 with type state := state
  include Init64 with type state := state
end

module type Full64Init30 = sig
  type state
  include State64 with type state := state
  include Init30 with type state := state
end
