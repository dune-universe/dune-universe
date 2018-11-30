(* File: curve_sampling_pq.ml

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* Maximum priority queue.  Implemented as a Pairing heap
   (http://en.wikipedia.org/wiki/Pairing_heap) following the paper:

   Fredman, Michael L.; Sedgewick, Robert; Sleator, Daniel D.; Tarjan,
   Robert E. (1986). "The pairing heap: a new form of self-adjusting
   heap" (PDF). Algorithmica. 1 (1): 111–129. doi:10.1007/BF01840439.  *)

let is_nan x = (x: float) <> x [@@inline]

type 'a node = {
    mutable priority: float;
    data: 'a;
    mutable child: 'a node;   (* points to oneself if no child *)
    mutable sibling: 'a node; (* next older sibling (or parent if last) *)
    mutable parent: 'a node;  (* points to oneself if root node *)
  }
(* Remark: because of mutability, a node can only belong to a single tree. *)

let has_children n = n.child != n
let not_last_sibling n = n.sibling != n.parent
let is_root n = n.parent == n

(* Since we will need to update the nodes, we need the tree to be
   mutable in case the root changes. *)
type 'a t = 'a node option ref


let make() = ref None

let is_empty q = (!q = None) [@@inline]

let max q = match !q with
  | None -> failwith "Curve_Sampling.PQ.max: empty"
  | Some node -> node.data

let max_priority q = match !q with
  | None -> neg_infinity
  | Some node -> node.priority

(* Will modify [n1] and [n2].  The one that is returned keeps its
   parent and siblings. *)
let merge_pair n1 n2 =
  if n1.priority > n2.priority then (
    let c1 = n1.child in
    n1.child <- n2;
    (* Because of the convention that the sibling = parent if last, we
       do not have to make a special case for the 1st child. *)
    n2.sibling <- c1;  n2.parent <- n1;
    n1)
  else (
    let c2 = n2.child in
    n2.child <- n1;
    n1.sibling <- c2;  n1.parent <- n2;
    n2)
  [@@inline]

(* Beware that [n] may become the new root and that then its parent
   and sibling need to have been set correctly. *)
let add_node q n =
  q := Some(match !q with
            | None -> n
            | Some root -> merge_pair n root)

let add q p x =
  if is_nan p then
    invalid_arg "Curve_Sampling.PQ.add: NaN priority not allowed";
  let rec n = { priority = p;  data = x;
                child = n;  sibling = n;  parent = n } in
  (* Whichever [n] or the root of [q] becomes the new root, parent and
     sibling are fine. *)
  add_node q n

type 'a witness = {
    queue: 'a t; (* To make sure the witness is for the right queue *)
    node: 'a node;
  }

let witness_add q p x =
  if is_nan p then
    invalid_arg "Curve_Sampling.PQ.witness_add: NaN priority not allowed";
  let rec n = { priority = p;  data = x;
                child = n;  sibling = n;  parent = n } in
  add_node q n;
  { queue = q;  node = n }

let priority w = w.node.priority

(* All the parents of [n0] and its siblings are replaced except for
   the node that is returned (which keeps the values it had). *)
let rec merge_pairs n0 =
  if not_last_sibling n0 then (
    let n1 = n0.sibling in
    if not_last_sibling n1 then
      merge_pair (merge_pair n0 n1) (merge_pairs n1.sibling)
    else
      merge_pair n0 n1
  )
  else n0

let delete_max q = match !q with
  | None -> failwith "Curve_Sampling.PQ.delete_max: empty"
  | Some root ->
     (if has_children root then
        let root' = merge_pairs root.child in
        (* Update the parent of the selected child (important to
           release the reference to [root]). *)
        root'.parent <- root';
        root'.sibling <- root';
        q := Some root'
      else q := None);
     root.data

(* REMARK: To be removed a node must become root.  Thus the state of
   removed nodes is necessarily root and using [increase_priority] on
   a removed node will not change the queue it used to belong to. *)
let increase_priority p witness =
  if is_nan p then
    invalid_arg "Curve_Sampling.PQ.increase_priority: NaN priority not allowed";
  let n = witness.node in
  if n.priority < p then
    if is_root n then
      n.priority <- p
    else (
      (* Cut [n] (and its children) from the tree and re-insert it
         with the new priority. *)
      let parent = n.parent in
      if parent.child == n then
        parent.child <- n.sibling (* fine if it is the only child. *)
      else (
        let n_prev = ref parent.child (* first child *) in
        while !n_prev.sibling != n do n_prev := !n_prev.sibling done;
        !n_prev.sibling <- n.sibling; (* OK even if [n] is last *)
      );
      n.priority <- p;
      n.sibling <- n;
      n.parent <- n;
      add_node witness.queue n;
    )

let rec iter_nodes n f =
  f n.data;
  if has_children n then iter_nodes n.child f;
  if not_last_sibling n then iter_nodes n.sibling f

let iter q ~f = match !q with
  | None -> ()
  | Some root -> iter_nodes root f

let rec iteri_nodes n f =
  f n.priority n.data;
  if has_children n then iteri_nodes n.child f;
  if not_last_sibling n then iteri_nodes n.sibling f

let iteri q ~f = match !q with
  | None -> ()
  | Some root -> iteri_nodes root f


let rec fold_nodes n init f =
  let init = f init n.data in
  let init = if has_children n then fold_nodes n.child init f
             else init in
  if not_last_sibling n then fold_nodes n.sibling init f
  else init

let fold q ~init ~f = match !q with
  | None -> init
  | Some root -> fold_nodes root init f

let rec foldi_nodes n init f =
  let init = f init n.priority n.data in
  let init = if has_children n then foldi_nodes n.child init f
             else init in
  if not_last_sibling n then foldi_nodes n.sibling init f
  else init

let foldi q ~init ~f = match !q with
  | None -> init
  | Some root -> foldi_nodes root init f

(* Since the nodes are mutable, we need to duplicate them. *)
let rec map_nodes n ~new_parent f =
  let rec n' = { priority = n.priority;  data = f n.data;
                 child = n';  sibling = new_parent;  parent = new_parent } in
  if has_children n then
    n'.child <- map_nodes n.child ~new_parent:n' f;
  if not_last_sibling n then
    n'.sibling <- map_nodes n.sibling ~new_parent f;
  n'

let map q ~f = match !q with
  | None -> ref None
  | Some root ->
     let rec root' = { priority = root.priority;  data = f root.data;
                       child = root';  sibling = root';  parent = root' } in
     if has_children root then
       root'.child <- map_nodes root.child ~new_parent:root' f;
     ref(Some root')


let rec filter_map_nodes n ~new_parent f =
  match f n.data with
  | Some y ->
     let rec n' = { priority = n.priority;  data = y;
                    child = n';  sibling = n';  parent = n' } in
     (* If [new_parent] is not known, set it to the node itself.
        Either the node will (eventually) be merged with [merge_pairs]
        or it will be returned in which case it will be the new root.  *)
     (match new_parent with
      | Some p -> n'.sibling <- p;  n'.parent <- p
      | None -> ());
     if has_children n then (
       match filter_map_nodes n.child ~new_parent:(Some n') f with
       | Some child -> n'.child <- child
       | None -> () (* all children removed *)
     );
     if not_last_sibling n then (
       match filter_map_nodes n.sibling ~new_parent f with
       | Some sibling -> n'.sibling <- sibling
       | None -> ()
     );
     Some n'
  | None ->
     (* Remove the node. Similar to [increase_priority] except that we
        do not know the new root yet so we will only move the children
        one level up. *)
     let child =
       if has_children n then
         (match filter_map_nodes n.child ~new_parent f with
          | Some n ->
             (* We merge all updated children [n] to make sure the
                heap property is preserved. *)
             let n = merge_pairs n in
             (* [n.parent] already set by above rec call *)
             n.sibling <- n; (* in case it becomes root *)
             Some n
          | None -> None)
       else None in
     let sibling = if not_last_sibling n then
                     filter_map_nodes n.sibling ~new_parent f
                   else None in
     match child, sibling with
     | Some n1, Some n2 -> n1.sibling <- n2;
                           Some n1
     | (Some _ as n), None | None, (Some _ as n) -> n
     | None, None -> None

let filter_map q ~f = match !q with
  | None -> ref None
  | Some root -> ref(filter_map_nodes root ~new_parent:None f)
