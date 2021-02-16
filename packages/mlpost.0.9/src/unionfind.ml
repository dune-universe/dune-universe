(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* This is code which has been taken from
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2008                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
 and has been modified since then by the Mlpost authors *)

module M = struct
  type t = int * int

  let equal = Stdlib.( = )

  (* let compare = Stdlib.compare *)
  let hash = Hashtbl.hash
end

type elt = float * float

type inputelt = M.t

module H = Hashtbl.Make (M)

type cell = { mutable c : int; mutable data : elt; mutable father : cell }

type t = cell H.t (* a forest *)

let init l =
  let h = H.create 997 in
  List.iter
    (fun ((a, b) as x) ->
      let t = (float_of_int a, float_of_int b) in
      let rec cell = { c = 1; data = t; father = cell } in
      H.add h x cell)
    l;
  h

let rec find_aux cell =
  if cell.father == cell then cell
  else
    let r = find_aux cell.father in
    cell.father <- r;
    r

let find x h = (find_aux (H.find h x)).data

let avg ra rb =
  let ax, ay = ra.data and bx, by = rb.data in
  let ac = float_of_int ra.c and bc = float_of_int rb.c in
  let z = ac +. bc in
  (((ac *. ax) +. (bc *. bx)) /. z, ((ac *. ay) +. (bc *. by)) /. z)

let union x y h =
  let rx = find_aux (H.find h x) in
  let ry = find_aux (H.find h y) in
  if rx != ry then
    if rx.c > ry.c then (
      ry.father <- rx;
      rx.data <- avg rx ry;
      rx.c <- rx.c + ry.c )
    else if rx.c < ry.c then (
      rx.father <- ry;
      ry.data <- avg rx ry;
      ry.c <- rx.c + ry.c )
    else (
      ry.father <- rx;
      rx.data <- avg rx ry;
      rx.c <- rx.c + ry.c )

let fold_classes f acc h =
  let seen = Hashtbl.create 127 in
  H.fold
    (fun _ v acc ->
      let r = find_aux v in
      let d = r.data in
      if Hashtbl.mem seen r then acc
      else (
        Hashtbl.add seen r ();
        f d acc ))
    h acc
