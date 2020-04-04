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

type ('a, 'c) iter =
  ('a -> unit) -> 'c -> unit

type ('a, 'c) iteri =
  (int -> 'a -> unit) -> 'c -> unit

type ('a, 'b, 'c) fold_left =
  ('b -> 'a -> 'b) -> 'b -> 'c -> 'b

type ('a, 'b, 'c) fold_right =
  ('a -> 'b -> 'b) -> 'c -> 'b -> 'b

let[@inline] fold_left iter_left f seed c =
  let accu = ref seed in
  let f x = accu := f !accu x in
  iter_left f c;
  !accu

let[@inline] fold_right iter_right f c seed =
  let accu = ref seed in
  let f x = accu := f x !accu in
  iter_right f c;
  !accu

let[@inline] iteri_left iter_left f s =
  let i = ref (-1) in
  let f a = (i := !i + 1; f !i a) in
  iter_left f s

let[@inline] iteri_right length iter_right f s =
  let i = ref (length s) in
  let f a = (i := !i - 1; f !i a) in
  iter_right f s

type ('a, 'c) to_list =
  'c -> 'a list

let[@inline] to_list fold_right c =
  fold_right (fun x xs -> x :: xs) c []

let[@inline] to_list iter_right c =
  to_list (fold_right iter_right) c

type ('a, 'c) for_all =
  ('a -> bool) -> 'c -> bool

exception Invalid

let[@inline] for_all iter valid c =
  try
    iter (fun x ->
      if not (valid x) then
        raise Invalid
    ) c;
    true
  with Invalid ->
    false
