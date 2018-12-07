(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

open Sigs

module Make
  (M : IMPERATIVE_MAPS)
  (G : GRAPH with type t = M.key)
= struct

  type t = G.t

  let push frontier x =
    Stack.push x frontier

  let rec visit gensym table frontier =
    match Stack.pop frontier with
    | exception Stack.Empty ->
        (* The stack is empty: we are done. *)
        ()
    | x ->
        match M.find x table with
        | _ ->
            (* [x] is known already: ignore it. *)
            visit gensym table frontier
        | exception Not_found ->
            (* Assign the number [i] to [x]. *)
            let i = gensym() in
            M.add x i table;
            G.foreach_successor x (push frontier);
            visit gensym table frontier

  let n, encode, decode =
    (* Perform a depth-first traversal of the graph. Assign a unique number [i]
       to every newly-discovered vertex [x]. *)
    let gensym = Gensym.make() in
    let table = M.create() in
    let frontier = Stack.create() in
    G.foreach_root (push frontier);
    visit gensym table frontier;
    let n = gensym() in
    (* We have discovered [n] graph vertices. [table] now contains a mapping
       of these vertices to integers in the range [0..n). We now build the
       reverse mapping in an array. This may seem a little clumsy (an array of
       options is allocated in an intermediate step), but requires only linear
       time. *)
    let vertex : G.t option array = Array.make n None in
    M.iter (fun x i ->
      vertex.(i) <- Some x
    ) table;
    let force = function Some x -> x | None -> assert false in
    let vertex : G.t array = Array.map force vertex in
    (* Return two conversions functions. *)
    let encode x =
      try
        M.find x table
      with Not_found ->
        let msg = Printf.sprintf "\n  Fix.Number says: \
          please check the argument passed to \"encode\".\n  %s\n" __LOC__ in
        raise (Invalid_argument msg)
    and decode i = vertex.(i) in
    n, encode, decode

end

module ForOrderedType (T : OrderedType) =
  Make(Glue.PersistentMapsToImperativeMaps(Map.Make(T)))

module ForHashedType (T : HashedType) =
  Make(Glue.HashTablesAsImperativeMaps(T))

module ForType (T : TYPE) =
  ForHashedType(Glue.TrivialHashedType(T))
