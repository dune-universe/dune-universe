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

let defined = function
  | None ->
      false
  | Some _ ->
      true

let iter f = function
  | None ->
      ()
  | Some x ->
      f x

(* The composition [length . filter defined] counts the number of defined
   entries in a list of options. *)

let rec ndefined accu (os : _ option list) =
  match os with
  | [] ->
      accu
  | o :: os ->
      ndefined (if defined o then accu + 1 else accu) os

let ndefined os =
  ndefined 0 os

(* [index i os] computes the absolute index of the [i]-th defined entry
   in a list of options. [i] must be less than [ndefined os]. *)

let rec index i j (os : _ option list) =
  match os with
  | [] ->
      (* This should not happen if [i] is in range. *)
      assert false
  | o :: os ->
      if defined o then
        if i = 0 then j
        else index (i - 1) (j + 1) os
      else
        index i (j + 1) os

let index i os =
  index i 0 os
