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

let may_raise_empty f x =
  match f x with
  | exception Stack.Empty ->
      None
  | y ->
      Some y

let harness_iter iter s =
  let accu = ref [] in
  iter (fun x -> accu := x :: !accu) s;
  !accu

let harness_fold fold s =
  fold (fun accu x -> x :: accu) [] s
