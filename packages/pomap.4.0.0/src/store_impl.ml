(*
   POMAP - Library for manipulating partially ordered maps

   Copyright (C) 2001-2002  Markus Mottl  (OEFAI)
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

open Store_intf

module IntIx = struct
  type t = int
  type gen = int

  module Set = Ptset
  module Map = Map.Make (struct type t = int let compare x y = x - y end)

  let start = 0
  let next_ix gen = gen
  let next n = n, n + 1
  let remove_ix gen n = max gen (n + 1)
  let int_of_ix n = n
end

module Make (Ix : INDEX) = struct
  module Ix = Ix

  type 'a t = Ix.gen * 'a Ix.Map.t

  let empty = Ix.start, Ix.Map.empty

  let is_empty (_, m) = Ix.Map.is_empty m
  let count _ _ n = n + 1
  let cardinal (_, m) = Ix.Map.fold count m 0
  let next_ix (gen, _) = Ix.next_ix gen

  let singleton el =
    let ix, gen = Ix.next Ix.start in
    ix, (gen, Ix.Map.add ix el Ix.Map.empty)

  let add el (gen, m) =
    let ix, new_gen = Ix.next gen in
    ix, (new_gen, Ix.Map.add ix el m)

  let find el (_, m) = Ix.Map.find el m

  let update ix el (gen, m) =
    Ix.remove_ix gen ix, Ix.Map.add ix el m

  let remove ix (gen, m) = gen, Ix.Map.remove ix m

  let iter f (_, m) = Ix.Map.iter (fun _ el -> f el) m
  let iteri f (_, m) = Ix.Map.iter f m
  let map f (gen, m) = gen, Ix.Map.map f m
  let mapi f (gen, m) = gen, Ix.Map.mapi f m
  let fold f (_, m) acc = Ix.Map.fold (fun _ el acc -> f el acc) m acc
  let foldi f (_, m) acc = Ix.Map.fold f m acc

  let cons_tpl ix el acc = (ix, el) :: acc
  let to_list s = foldi cons_tpl s []

  let choose (_, m) =
    let x_ref = ref (Obj.magic 0) in
    let act ix el = x_ref := (ix, el); raise Exit in
    try Ix.Map.iter act m; raise Not_found with Exit -> !x_ref

  let filter p (gen, m) =
    let coll ix el acc = if p ix el then acc else Ix.Map.remove ix acc in
    gen, Ix.Map.fold coll m m

  let partition p (gen, m) =
    let coll ix el (yes, no) =
      if p ix el then yes, Ix.Map.remove ix no else Ix.Map.remove ix yes, no in
    let yes, no = Ix.Map.fold coll m (m, m) in
    (gen, yes), (gen, no)

  let eq_classes eq (gen, m) =
    let rec coll acc ix el = function
      | [] -> (el, Ix.Map.add ix el Ix.Map.empty) :: acc
      | (eq_el, eq_m) as ec :: ecs ->
          if eq el eq_el then
            (eq_el, Ix.Map.add ix el eq_m) :: List.rev_append acc ecs
          else coll (ec :: acc) ix el ecs in
    let ecs = Ix.Map.fold (coll []) m [] in
    List.rev_map (fun (eq_el, eq_m) -> eq_el, (gen, eq_m)) ecs

  let get_ix_map (_, m) = m
end

module IntStore = Make (IntIx)
