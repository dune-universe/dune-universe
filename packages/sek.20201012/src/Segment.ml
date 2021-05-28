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

open PrivateSignatures (* [Front], [Back] *)

let is_valid (a, i, k) =
  0 <= k &&
  0 <= i && i + k <= Array.length a

let is_empty (_, _, k) =
  k = 0

let[@inline] iter pov ((a, i, k) as seg) yield =
  assert (is_valid seg);
  match pov with
  | Front ->
      for j = i to i + k - 1 do
        yield a.(j)
      done
  | Back ->
      for j = i + k - 1 downto i do
        yield a.(j)
      done

let[@inline] iter2 pov ((a1, i1, k1) as seg1) ((a2, i2, k2) as seg2) yield =
  assert (is_valid seg1);
  assert (is_valid seg2);
  assert (k1 = k2);
  let k = k1 in
  match pov with
  | Front ->
      for j = 0 to k - 1 do
        yield a1.(i1 + j) a2.(i2 + j)
      done
  | Back ->
      for j = k - 1 downto 0 do
        yield a1.(i1 + j) a2.(i2 + j)
      done
