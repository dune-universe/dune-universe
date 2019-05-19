(**
  A module containing a utility functor for monads.
*)

open Batteries;;
open Interfaces;;

module type Utils =
sig
  type 'a m
  
  val sequence : 'a m Enum.t -> 'a Enum.t m
  val mapM : ('a -> 'b m) -> 'a Enum.t -> 'b Enum.t m
end;;

module Make : functor (M : Monad) -> Utils with type 'a m = 'a M.m;;
