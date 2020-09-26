open Core_kernel

module type S = sig
  module Incr : sig
    type 'a t
  end

  (** [select_one] logically constructs a set of outputs such that exactly one is selected
      as [true] (specifically the one that corresponds to the current value of the input),
      and the rest are false.  The staged function is used for creating incrementals that
      contain the appropriate output value. *)
  val select_one
    :  (module Hashable.Common with type t = 'a)
    -> 'a Incr.t
    -> ('a -> bool Incr.t) Staged.t

  (** [select_one'] is similar to [select_one], except you can decide to not make a choice
      (by choosing [None]) *)
  val select_one'
    :  (module Hashable.Common with type t = 'a)
    -> 'a option Incr.t
    -> ('a -> bool Incr.t) Staged.t

  (** [select_one_value] is similar to [select_one], except you also get to specify the
      value it will take on instead of only [true] and [false].  [select_one h input] is
      equivalent to:

      {[ select_one_value h ~default:false (let%map x = input in (x, true)) ]}
  *)
  val select_one_value
    :  (module Hashable.Common with type t = 'a)
    -> default:'b
    -> ('a * 'b) Incr.t
    -> ('a -> 'b Incr.t) Staged.t

  (** [select_one_value'] is the same as [select_one_value] except if the input is None,
      then all outputs are [default]. *)
  val select_one_value'
    :  (module Hashable.Common with type t = 'a)
    -> default:'b
    -> ('a * 'b) option Incr.t
    -> ('a -> 'b Incr.t) Staged.t

  (** [select_many] allows you to specify a group of outputs to be selected as true
      instead of just one.  [select_one h input] is equivalent to:

      {[select_many h (let%map x = input in [x])]}
  *)
  val select_many
    :  (module Hashable.Common with type t = 'a)
    -> 'a list Incr.t
    -> ('a -> bool Incr.t) Staged.t

  (** [select_many_values] allows you specify a group of outputs to be selected and their
      corresponding values.

      This is the most general of these functions; all the others can be reduced to it.
  *)
  val select_many_values
    :  (module Hashable.Common with type t = 'a)
    -> default:'b
    -> ('a * 'b) list Incr.t
    -> ('a -> 'b Incr.t) Staged.t
end

module type Incr_select = sig
  (** This module provides the ability to create a large set of incremental outputs based
      on a single incremental input, where some subset of the outputs are selected to have
      specific values, and the remainder are left with a specified default value. The
      outputs are updated in time proportional to the number of outputs that are changed,
      independent of the number of outputs that exist. Each function returns a staged
      function for allocating new output incrementals.

      Note that it's possible that no incremental output is actually instantiated for the
      node that is selected, which is not a problem. Also, one can have more than one
      incremental that reflects the same output.

      An example of where this is useful is for managing focus in a UI, where you want a
      single value to determine where the focus is, and many individual incrementals for
      determining whether a given sub-component is in focus.
  *)

  module Make (Incr : Incremental.S_gen) : S with module Incr := Incr
end
