(**
   This module contains a non-determinism monad.
*)

open Batteries;;

module type Nondeterminism_monad_sig = sig
  type 'a m
  include Monad.LazyPlus with type 'a m := 'a m
  include Jhupllib_monad_utils.Utils with type 'a m := 'a m
  val pick_enum : 'a Enum.t -> 'a m
  val enum : 'a m -> 'a Enum.t

  val stop_unless : bool -> unit m
  val empty : unit -> 'a m
  val alternative : 'a m -> 'a m -> 'a m
end;;

module Nondeterminism_monad : Nondeterminism_monad_sig
