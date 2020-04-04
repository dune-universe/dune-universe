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

type side = bool
let front = true
let back = false

type direction = bool
let forward = true
let backward = false

module Persistent = struct

  (* Replace OCaml's default implementation of list concatenation with
     a tail recursive implementation. This should decrease our chances
     of blowing the stack. *)

  let (@) xs ys =
    List.rev_append (List.rev xs) ys

  (* Persistent sequences are implemented by lists. *)

  type 'a t = 'a list

  let create _default = []

  let rec repeat n x xs =
    if n = 0 then
      xs
    else
      repeat (n - 1) x (x :: xs)

  let repeat n x =
    repeat n x []

  let make _ n x =
    repeat n x

  let rec init i j f xs =
    if i < j then
      init (i + 1) j f (f i :: xs)
    else
      xs

  let init n f =
    List.rev (init 0 n f [])

  let init _ n f =
    init n f

  let length = List.length

  let is_empty xs =
    xs = []

  let push_front s x = x :: s

  let push_back s x = s @ [x]

  let push side s x =
    if side then push_front s x else push_back s x

  let pop_front_opt s =
    match s with
    | [] ->
        None, []
    | x :: xs ->
        Some x, xs

  let pop_back_opt s =
    let ox, s = pop_front_opt (List.rev s) in
    ox, List.rev s

  let pop_opt side s =
    if side then pop_front_opt s else pop_back_opt s

  let peek_front_opt s =
    match s with
    | [] ->
        None
    | x :: _ ->
        Some x

  let peek_back_opt =
    ListExtra.last

  let peek_opt side s =
    if side then peek_front_opt s else peek_back_opt s

  let get =
    List.nth

  let rec set xs i x' =
    match xs, i with
    | [], _ ->
        assert false (* invalid argument *)
    | _x :: xs, 0 ->
        x' :: xs
    | x :: xs, _ ->
        x :: set xs (i - 1) x'

  let concat = (@)

  let rec split xs i =
    if i = 0 then
      [], xs
    else
      match xs with
      | [] ->
          assert false (* invalid argument *)
      | x :: xs ->
          let xs, ys = split xs (i - 1) in
          x :: xs, ys

  let iter direction f xs =
    if direction then
      List.iter f xs
    else
      List.iter f (List.rev xs)

  let iteri direction f xs =
    if direction then
      List.iteri f xs
    else
      let n = List.length xs in
      let opposite i = n - 1 - i in
      List.iteri (fun i x -> f (opposite i) x) (List.rev xs)

  let of_array _ a =
    Array.to_list a

  let of_array_segment d a i k =
    of_array d (Array.sub a i k)

  let to_array =
    Array.of_list

  let check _s = ()

end

module Ephemeral = struct

  module P = Persistent

  type 'a t =
    'a P.t ref

  let create default =
    ref (P.create default)

  let make d n x =
    ref (P.make d n x)

  let init d n f =
    ref (P.init d n f)

  let length s =
    P.length !s

  let is_empty s =
    P.is_empty !s

  let clear s =
    s := []

  let copy s =
    ref !s

  let assign s1 s2 =
    if s1 != s2 then begin
      s1 := !s2;
      clear s2
    end

  let push side s x =
    s := P.push side !s x

  let pop_opt side s =
    let ox, xs = P.pop_opt side !s in
    s := xs;
    ox

  let peek_opt side s =
    P.peek_opt side !s

  let get s =
    P.get !s

  let set s i x =
    s := P.set !s i x

  let concat s1 s2 =
    let s = ref (P.concat !s1 !s2) in
    clear s1;
    clear s2;
    s

  let append_back s1 s2 =
    (* Move data from [s2] into the back of [s1]. Clear [s2]. *)
    s1 := P.concat !s1 !s2;
    clear s2

  let append_front s1 s2 =
    (* Move data from [s2] into the front of [s1]. Clear [s2]. *)
    s1 := P.concat !s2 !s1;
    clear s2

  let append side s1 s2 =
    if side then
      append_front s1 s2
    else
      append_back s1 s2

  let split s i =
    let xs, ys = P.split !s i in
    clear s;
    ref xs, ref ys

  let carve_front s i =
    (* Extract the [i] front elements in a new sequence. *)
    let xs, ys = P.split !s i in
    s := ys;
    ref xs

  let carve_back s i =
    (* Keep the [i] front elements and extract the rest in a new sequence. *)
    let xs, ys = P.split !s i in
    s := xs;
    ref ys

  let carve side s i =
    if side then
      carve_front s i
    else
      carve_back s i

  let iter direction f s =
    P.iter direction f !s

  let iteri direction f s =
    P.iteri direction f !s

  let of_array d a =
    ref (P.of_array d a)

  let of_array_segment d a i k =
    ref (P.of_array_segment d a i k)

  let to_array s =
    P.to_array !s

  let check _s = ()

end

let snapshot s =
  !s

let edit xs =
  ref xs

let snapshot_and_clear s =
  let xs = !s in
  s := [];
  xs

module E = Ephemeral
module P = Persistent
