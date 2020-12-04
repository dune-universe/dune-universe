(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2020 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(*${*)

open Containers

(*$}*)

(*$
  ;;
  inject

  open Test
*)

type t = Atom.t Array.t

(* Constructors *)

let of_list1 xs =
  assert (not @@ List.is_empty xs);
  Array.of_list xs


let tuple1 at = of_list1 [ at ]

(* accessor *)
let arity tuple =
  let ar = Array.length tuple in
  assert (ar > 0);
  ar


let hash tuple = Hash.array Atom.hash tuple

(* printing *)
let pp out atoms =
  let open Fmtc in
  (array ~sep:sp Atom.pp |> if arity atoms > 1 then parens else fun x -> x)
    out
    atoms


module P = Intf.Print.Mixin (struct
  type nonrec t = t

  let pp = pp
end)

include P

(* other accessors *)

let to_list t = Array.to_list t

let compare t1 t2 = Array.compare Atom.compare t1 t2

let equal t1 t2 = Array.equal Atom.equal t1 t2

(* transpose involutive *)
(*$Q transpose
  any_tuple2 (fun tuple -> \
  transpose (transpose tuple) = tuple)
*)
let transpose tuple =
  assert (arity tuple = 2);
  Array.rev tuple


let ith i tuple =
  assert (i >= 0 && i < arity tuple);
  tuple.(i)


let ( @@@ ) t1 t2 = Array.append t1 t2

let concat = function
  | [] ->
      invalid_arg "Tuple.concat: empty list of tuples"
  | hd :: tl ->
      List.fold_left ( @@@ ) hd tl


(* join iden right neutral *)
(*$Q join
  Q.(pair any_tuple any_tuple2) (fun (t1, iden) -> \
  Q.assume (Atom.equal (ith (arity t1 - 1) t1) (ith 0 iden));\
  Q.assume (Atom.equal (ith 0 iden) (ith 1 iden));\
  equal (join t1 iden) t1 \
  )
*)
(* join iden left neutral *)
(*$Q join
  Q.(pair any_tuple2 any_tuple) (fun (iden, t1) -> \
  Q.assume (Atom.equal (ith 1 iden) (ith 0 t1));\
  Q.assume (Atom.equal (ith 0 iden) (ith 1 iden));\
  equal (join iden t1) t1 \
  )
*)
let join tuple1 tuple2 =
  let t1 = tuple1 in
  let t2 = tuple2 in
  let lg1 = Array.length t1 in
  let lg2 = Array.length t2 in
  assert (Atom.equal (ith (arity tuple1 - 1) tuple1) (ith 0 tuple2));
  (* imperative but safe: first we create a fresh array and fill it
     imperatively; and only then do we make a [t] out of it *)
  let res = Array.make (lg1 + lg2 - 2) t1.(0) in
  Array.blit t1 0 res 0 (lg1 - 1);
  Array.blit t2 1 res (lg1 - 1) (lg2 - 1);
  res


let is_in_join tup t1 t2 =
  let lg1 = Array.length t1 in
  assert (lg1 > 0);
  assert (Array.length t2 > 0);
  Atom.equal t1.(lg1 - 1) t2.(0) && (equal tup @@ join t1 t2)


(* |> Fun.tap (fun res -> Msg.debug (fun m -> *)
(*       m "is_in_join: %a =? %a.%a --> %B" *)
(*         pp tup *)
(*         pp t1 *)
(*         pp t2 *)
(*         res)) *)

(* split "inverse" to (@@@) *)
(*$Q split
  Q.(pair any_tuple (int_range 1 9)) (fun (tuple, len) -> \
  Q.assume (len > 0 && len < arity tuple);\
  equal tuple (Fun.uncurry (@@@) @@ split tuple len)\
  )
*)
(* other direction *)
(*$Q split
  Q.(pair any_tuple any_tuple) (fun (t1, t2) -> \
  let t = (t1 @@@ t2) in \
  let l1 = arity t1 in \
  let (a, b) = (split t l1) in \
  equal a t1 && equal b t2 \
  )
*)
let split tuple len =
  (* Msg.debug (fun m -> m "split <-- %a %d" pp tuple len); *)
  let t = tuple in
  let full_len = Array.length t in
  assert (len > 0 && len < full_len);
  let t1 = Array.sub t 0 len in
  let t2 = Array.sub t len (full_len - len) in
  (t1, t2)


(* |> Fun.tap (fun res -> *)
(*     Msg.debug (fun m -> m "split --> %a@." Fmtc.(parens @@ Pair.pp ~sep:"," pp pp) res)) *)

let all_different t =
  let sorted = Array.sorted Atom.compare t in
  let lg = Array.length t in
  assert (lg > 0);
  let i = ref 1 in
  let yes = ref true in
  while !yes && !i < lg do
    yes := Atom.compare sorted.(!i - 1) sorted.(!i) <> 0
  done;
  !yes


let to_1tuples t =
  assert (Array.length t > 0);
  Array.fold_right (fun at acc -> of_list1 [ at ] :: acc) t []


let to_ntuples n t =
  let lg = Array.length t in
  assert (lg > 0);
  if lg mod n <> 0
  then
    invalid_arg
    @@ Fmt.strf "Tuple.to_ntuples %d %a: length not a multiple of %d" n pp t n;
  Array.to_list t |> List.sublists_of_len n |> List.map of_list1


let rename atom_renaming t =
  Array.map (fun at -> List.assoc ~eq:Atom.equal at atom_renaming) t


module Set = CCSet.Make (struct
  type nonrec t = t

  let compare = compare
end)
