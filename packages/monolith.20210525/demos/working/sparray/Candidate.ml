(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(* These are Conchon and Filliâtre's semi-persistent arrays (JFLA 2007). *)

(* Whereas Conchon and Filliâtre introduce a data constructor [Invalid],
   which (if desired) might allow testing at runtime whether an array is
   invalid, we introduce no such data constructor. Therefore, it is impossible
   to determine at runtime whether an array is invalid; it is up to the user
   to never attempt to use an invalid array. *)

type 'a t = 'a data ref

and 'a data =
  | Array of 'a array
  | Diff of int * 'a * 'a t

let make n v = ref (Array (Array.make n v))

let rec rerootk t k =
  match !t with
  | Array _ ->
      k ()
  | Diff (i, v, t') ->
      rerootk t' (fun () ->
        begin match !t' with
	| Array a as n ->
            (* let v' = a.(i) in *)
            a.(i) <- v;
	    t := n
	    (* t' := Diff (i, v', t) *)
        | Diff _ ->
            assert false
        end;
        k()
      )

let reroot t = rerootk t (fun () -> ())

let get t i = match !t with
  | Array a ->
      a.(i)
  | Diff _ ->
      reroot t;
      begin match !t with Array a -> a.(i) | Diff _ -> assert false end

let set t i v =
  reroot t;
  match !t with
  | Array a as n ->
      let old = a.(i) in
      if old == v then
	t
      else begin
	a.(i) <- v;
	let res = ref n in
	t := Diff (i, old, res);
	res
      end
  | Diff _ ->
      assert false

let rec length t =
  match !t with
  | Array a ->
      Array.length a
  | Diff (_, _, t) ->
      length t

(* wrappers to apply an impure function from Array to a persistent array *)
let impure f t =
  reroot t;
  match !t with Array a -> f a | Diff _ -> assert false

let to_list t = impure Array.to_list t
