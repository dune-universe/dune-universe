(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)


(** Signature of polymorphic ephemeral stacks *)

module type ResizeFactorsSig =
sig
  val min_capacity : int
  val grow_factor : int
  val shrink_condition : int
  val shrink_factor : int
end

module Factors2 : ResizeFactorsSig =
struct
  let min_capacity = 4
  let grow_factor = 2
  let shrink_condition = 4
  let shrink_factor = 2
end

module Factors4 : ResizeFactorsSig =
struct
  let min_capacity = 4
  let grow_factor = 4
  let shrink_condition = 8
  let shrink_factor = 4
end


(** Stack implemented as a vector (resizable array).

    The ResizeFactors argument specifies the resizing policy.
    By default, one should use [ResizeFactorsSig.Factors2],
    to double size when full, and to half size when less than
    quarter full.

*)

(*
module Make
  (ResizeFactors : ResizeFactorsSig)
  (OverwriteEmptySlots : OVERWRITE_EMPTY_SLOTS)
= struct
*)
module ResizeFactors = Factors2
let restore_default = Shared.Cmdline.parse_or_default_bool "restore_default" true
module OverwriteEmptySlots = struct let overwrite_empty_slots = restore_default end

open OverwriteEmptySlots

(** Elements are stored in the array [data].
    The size of [data] is always equal to [Capacity.value].
    The [size] field denotes the number of elements.
    It is such that [0 <= size <= capacity]. *)

type 'a t = {
   mutable size : int;
   mutable data : 'a array;
   default : 'a; }

(** Auxiliary *)

let default s =
  s.default

let length s =
  s.size

let capacity s =
  Array.length s.data

let change_capacity s new_capacity =
  let old_data = s.data in
  let old_capacity = capacity s in
  if new_capacity < old_capacity then begin
    s.data <- Array.sub old_data 0 new_capacity;
  end else begin
    s.data <- Array.make new_capacity s.default;
    Array.blit old_data 0 s.data 0 s.size
  end

(** Stack operations *)

let create d =
 { size = 0;
   data = Array.make ResizeFactors.min_capacity d;
   default = d; }

let is_empty s =
  s.size = 0

let push x s =
  let n = s.size in
  if n = capacity s
    then change_capacity s (ResizeFactors.grow_factor * n);
    (* Note: could take min with Sys.max_array_length *)
  s.data.(n) <- x;
  s.size <- n + 1

let top s =
  assert (s.size > 0);
  s.data.(s.size - 1)

let pop s =
   assert (s.size > 0);
   let new_n = s.size - 1 in
   s.size <- new_n;
   let x = s.data.(new_n) in
   if overwrite_empty_slots
     then s.data.(new_n) <- s.default;
   (* Note: Is it more efficient to compute:
      (ResizeFactors.shrink_condition * t.size <= c)
      and would this have any overflow issue? *)
   let c = capacity s in
   if   (s.size <= c / ResizeFactors.shrink_condition)
     && (c > ResizeFactors.min_capacity)
      then change_capacity s (c / ResizeFactors.shrink_factor);
   x

   (* Note that we could add: if s.size <= 0 then raise Not_found; *)

(** Random access *)

let get s i =
  assert (i >= 0 && i < s.size);
  s.data.(i)

let set s i v =
  assert (i >= 0 && i < s.size);
  s.data.(i) <- v

(** Iteration *)

let iter f s =
  let n = length s in
  for i = 0 to n-1 do
    f (s.data.(i))
  done

let fold_left f x s =
  let r = ref x in
  let n = length s in
  for i = 0 to n-1 do
    r := f !r (s.data.(i) )
  done;
  !r

let fold_right f s x =
  let r = ref x in
  let n = length s in
  for i = n-1 downto 0 do
    r := f (s.data.(i)) !r
  done;
  !r

let to_list s =
  fold_left (fun a x -> x::a) [] s

(*
let to_list s =
  fold_right (fun x a -> x::a) s []
*)

(* end *)
