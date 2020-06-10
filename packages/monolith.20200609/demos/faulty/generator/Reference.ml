(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(* A reference implementation of a stateful nondeterministic number generator,
   whose specification states that the next number must be strictly greater
   than the previous number. *)

type 'a t =
  'a ref

let create () =
  ref 0

(* Whereas the function [next] on the candidate side has type ['a t -> int], the
   function [next] on the reference side has type ['a t -> int postcondition].
   Its arguments are the generator [g] and the result produced by the function
   [next] in the candidate implementation. It returns a value of type [int
   diagnostic] that indicates whether the candidate result is valid or invalid.
   In the latter case, the diagnostic includes an OCaml assertion that allows
   reproducing the problem. *)

open Monolith
open Monolith.Print
open PPrint

let next (g : 'a t) : int postcondition =
  fun (candidate : int) ->
    (* According to the specification, the candidate result [candidate] is
       acceptable if and only if it is strictly greater than [!g]. *)
    if !g < candidate then begin
      (* It is acceptable. Update our internal state. *)
      g := candidate;
      Valid
    end
    else begin
      (* It is not acceptable: it should have been strictly greater than [!g].
         We produce an OCaml assertion that says so. *)
      Invalid (fun (x : document) ->
        assert_ (int !g ^^ string " < " ^^ x) ^^ string ";;" ^^
        candidate_finds (int candidate)
      )
    end
