(** {1 Xoshiro}

   This library includes OCaml implementation of some pseudorandom number
   generators (PRNGs) designed by David Blackman and Sebastiano Vigna behind an
   interface that mimmics that of the {!Random} module of the standard library.

   The Xoshiro generators (for XOR/shift/rotate) are all-purpose generators ({b
   not cryptographically secure}). Compared to the standard library, they:

   - {b have a bigger state space}: [xoshiro256++]/[xoshiro256**] generators
     have a period of 2²⁵⁶-1.

   - {b pass more tests}: [xoshiro256++]/[xoshiro256**] pass the whole
     {{: http://simul.iro.umontreal.ca/testu01/tu01.html}BigCrush} test suite
     while the {!Random} module of the standard library systematically fails
     some of the tests.

   - {b run similarly fast} for the bindings and twice slower for the pure
     implementation.

   This module and all the variants (see below) are drop-in replacements of the
   {!Random} module of the standard library. This means you can use {!Xoshiro}
   everywhere where you would use {!Random}. For instance:

   - use [Xoshiro.bits] instead of [Random.bits]
   - (same for [int], [bool], etc. and also for the [State] submodule)
   - use [open Xoshiro] instead of [open Random]
   - or even write [module Random = Xoshiro] at the beginning of every file. *)

(** {2 Variants}

   David Blackman and Sebastiano Vigna present several variants of their
   generators depending on the state size and implementation details. *)

module Xoshiro256plusplus : sig
  include MakeRandom.Sig.Full

  (** {3 Low-level Interface}

     Direct bindings of the functions provided in the original implementation.
     *)

  module LowLevel : sig
    type t
    (** Low-level type for [xoshiro256++]. This is basically an [int64 array] of
       4 values. *)

    val of_int64_array : int64 array -> t
    (** Copy an [int64 array] to the state.

        @raise Invalid_argument if the array is not of size 4. *)

    val to_int64_array : t -> int64 array
    (** Copy the state to an [int64 array]. *)

    val next : t -> int64

    val jump : t -> unit
    (** This is the jump function for the generator. It is equivalent to 2¹²⁸
       calls to {!next}; it can be used to generate 2¹²⁸ non-overlapping
       subsequences for parallel computations. *)

    val long_jump : t -> unit
    (** This is the long-jump function for the generator. It is equivalent to
       2¹⁹² calls to {!next}; it can be used to generate 2⁶⁴ starting points,
       from each of which {!jump} will generate 2⁶⁴ non-overlapping subsequences
       for parallel distributed computations. *)
  end
end

(** {2 Default}

   The module {!Xoshiro} includes by default an implementation of
   {!Xoshiro256plusplus}. *)

include module type of Xoshiro256plusplus

(** {2 Others} *)

module Splitmix64 : MakeRandom.Sig.Full

(* Not to self [from Niols]: it is tempting, while we're at it, to provide
   bindings for a C implementation of the standard library's Mersene Twister. I
   have tried this and, as it turns out, it is pretty hard to beat the pure
   OCaml implementation of the standard library. *)
