(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: prolog.ml,v 1.8 2001/06/15 10:27:27 barnier Exp $ *)

(* FaCiLe as a Prolog interpreter: the family tree

   In this example, we write the classic goals parent, grandparent and ancestor
   for the following family tree:

   sam
    |
   jim + lucy
       |
     fred + lynn
          |
         ann

   Then we search all the solutions for various questions, using a "findall"
   goal that builds a list of all the possible values for a given variable such
   that a given goal succeeds.

   This example was inspired by Mattias Waldau and translated from the
   following Prolog source:

   father(ann, fred).
   father(fred, jim).
   father(jim, sam).

   mother(ann, lynn).
   mother(fred, lucy).

   parent(X,Y) :- mother(X,Y).
   parent(X,Y) :- father(X,Y).

   grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

   ancestor(X,Y) :- parent(X,Y).
   ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
*)

open Facile
open Easy

let ann = 0 and fred = 1 and jim = 2
and sam = 3 and lynn = 4 and lucy = 5

let name_of = function
    0 -> "Ann" | 1 -> "Fred" | 2 -> "Jim"
  | 3 -> "Sam" | 4 -> "Lynn" | 5 -> "Lucy" | _ -> invalid_arg "name_of"

let family_dom = Domain.create [ann; fred; jim; sam; lynn; lucy]

(* child first, parent second *)
let fathers = [(ann, fred); (fred, jim); (jim, sam)]
let mothers = [(ann, lynn); (fred, lucy)]

(* [father x y] [x]: child; [y]: father*)
let father x y =
  Goals.List.exists (fun (c, f) -> Goals.unify x c &&~ Goals.unify y f) fathers

(* [mother x y] [x]: child; [y]: mother] *)
let mother x y =
  Goals.List.exists (fun (c, m) -> Goals.unify x c &&~ Goals.unify y m) mothers

let parent x y = father x y ||~ mother x y

(* We use the existential quantifier [Goals.sigma] to hide the creation of
   an intermediate variable. However, we provide its domain [family_dom] which
   is an optional argument (if omitted, the domain is the largest one). *)
let grandparent x y =
  Goals.sigma ~domain:family_dom (fun z -> parent x z &&~ parent z y)

(* Recursive goal implemented from a recursive function using Goals.create *)
let rec ancestor x y =
  Goals.create
    (fun () ->
      parent x y ||~
      (Goals.sigma ~domain:family_dom (fun z -> parent x z &&~ ancestor z y)))
    ()

(* [val findall : (Fd.t -> Goals.t) -> int list]
   [findall g] returns all the solutions of variables [y] such that the goal [g y]
   succeeds. *)
let findall g =
  (* The solutions are stored  in a list. *)
  let sol = ref [] in
  let store v = Goals.atomic (fun () -> sol := Fd.int_value v :: !sol) in
  let goal =
    Goals.sigma ~domain:family_dom (fun y -> g y &&~ store y &&~ Goals.fail)
     ||~
    Goals.success in
  if Goals.solve goal then
    !sol
  else
    failwith "Unexpected failure"

let all_ancestors x =
  findall (fun y -> ancestor x y)

let all_parents x =
  findall (fun y -> parent x y)

let all_grandchildren y =
  findall (fun x -> grandparent x y)

let _ =
  let print_list l =
    List.iter (fun a -> Printf.printf "%s " (name_of a)) l;
    print_newline () in
  (* All the ancestors of Ann *)
  let ancestors = all_ancestors (Fd.int ann) in
  print_list ancestors;

  (* All the parents of Jim and Ann *)
  let parents = all_parents (Fd.create (Domain.create [jim; ann])) in
  print_list parents;

  (* All the grandchildren of Sam *)
  let grandchildren = all_grandchildren (Fd.int sam) in
  print_list grandchildren

