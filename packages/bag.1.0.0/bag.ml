(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
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

module Make(X: sig
  type t
  val compare: t -> t -> int
end) = struct

  module M = Map.Make(X)

  type elt = X.t

  type t = int M.t
  (** invariant: multiplicities are all > 0 *)

  let empty =
    M.empty

  let is_empty =
    M.is_empty

  let mem =
    M.mem

  let occ x b =
    try M.find x b with Not_found -> 0

  let add x ?(mult=1) b =
    if mult < 0 then invalid_arg "add";
    if mult = 0 then b else
    try let m = M.find x b in M.add x (m + mult) b
    with Not_found -> M.add x mult b

  let update x f b =
    let f o =
      let m = f (match o with None -> 0 | Some m -> m) in
      if m < 0 then invalid_arg "update";
      if m = 0 then None else Some m in
    M.update x f b

  let singleton x =
    M.add x 1 M.empty

  let remove x ?(mult=1) b =
    if mult < 0 then invalid_arg "remove";
    if mult = 0 then b else
    M.update x
      (function | None | Some 1 -> None
                | Some m when m <= mult -> None
                | Some m -> Some (m - mult)) b

  let remove_all =
    M.remove

  let merge f b1 b2 =
    let f x o1 o2 =
      let m1 = match o1 with None -> 0 | Some m -> m in
      let m2 = match o2 with None -> 0 | Some m -> m in
      let m = f x m1 m2 in
      if m < 0 then invalid_arg "merge";
      if m = 0 then None else Some m in
    M.merge f b1 b2

  let cardinal b =
    M.fold (fun _ m c -> m + c) b 0

  let elements =
    M.bindings

  let min_elt =
    M.min_binding

  let min_elt_opt =
    M.min_binding_opt

  let max_elt =
    M.max_binding

  let max_elt_opt =
    M.max_binding_opt

  let choose =
    M.choose

  let choose_opt =
    M.choose_opt

  let union b1 b2 =
    M.merge (fun _ o1 o2 -> match o1, o2 with
                            | None, None -> None
                            | None, Some m | Some m, None -> Some m
                            | Some m1, Some m2 -> Some (max m1 m2)) b1 b2

  let sum b1 b2 =
    M.merge (fun _ o1 o2 -> match o1, o2 with
                            | None, None -> None
                            | None, Some m | Some m, None -> Some m
                            | Some m1, Some m2 -> Some (m1 + m2)) b1 b2

  let inter b1 b2 =
    M.merge (fun _ o1 o2 -> match o1, o2 with
                            | None, None
                            | None, Some _ | Some _, None -> None
                            | Some m1, Some m2 -> Some (min m1 m2)) b1 b2

  let diff b1 b2 =
    M.merge (fun _ o1 o2 -> match o1, o2 with
                            | None, _ -> None
                            | Some m, None -> Some m
                            | Some m1, Some m2 when m1 <= m2 -> None
                            | Some m1, Some m2 -> Some (m1 - m2)) b1 b2

  let disjoint b1 b2 =
    M.for_all (fun x1 _ -> not (mem x1 b2)) b1

  let included b1 b2 =
    M.for_all (fun x1 m1 -> m1 <= occ x1 b2) b1

  let iter =
    M.iter

  let fold =
    M.fold

  let for_all =
    M.for_all

  let exists =
    M.exists

  let filter =
    M.filter

  let partition =
    M.partition

  let split x b =
    let l, m, r = M.split x b in
    l, (match m with None -> 0 | Some m -> m), r

  let find_first =
    M.find_first

  let find_first_opt =
    M.find_first_opt

  let find_last =
    M.find_last

  let find_last_opt =
    M.find_last_opt

  let map f =
    let f m = let m = f m in if m < 0 then invalid_arg "map"; m in
    M.map f

  let mapi f =
    let f x m = let m = f x m in if m < 0 then invalid_arg "mapi"; m in
    M.mapi f

  let compare =
    M.compare Stdlib.compare

  let equal =
    M.equal (==)

  let to_seq =
    M.to_seq

  let to_seq_from =
    M.to_seq_from

  let add_seq s b =
    Seq.fold_left (fun b (x, mult) -> add x ~mult b) b s

  let of_seq s =
    add_seq s empty

end
