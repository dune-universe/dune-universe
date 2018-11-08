(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

module A = Array
module L = BatList
module IntMap = BatMap.Int

(* tani(A,B) = |inter(A,B)| / |union(A,B)| *)

let tanimoto_et_al (xs: float array) (ys: float array): float * float * float =
  let xys = ref 0.0 in
  let x2s = ref 0.0 in
  let y2s = ref 0.0 in
  A.iter2 (fun x y ->
      let xy = x *. y in
      let x2 = x *. x in
      let y2 = y *. y in
      xys := !xys +. xy;
      x2s := !x2s +. x2;
      y2s := !y2s +. y2
    ) xs ys;
  (!xys, !x2s, !y2s)

let array_tanimoto (xs: float array) (ys: float array): float =
  if xs = ys then
    (* needed _before_ NaN protection *)
    1.0
  else
    let xys, x2s, y2s = tanimoto_et_al xs ys in
    if xys = 0.0 then 0.0 (* avoid NaN *)
    else xys /. (x2s +. y2s -. xys) (* regular formula *)

let array_tanimoto_dist xs ys =
  1.0 -. (array_tanimoto xs ys)

(* a SFP encoded molecule is an intmap: atom_env (feature index) to count
   of this feature *)
let intmap_tanimoto m1 m2 =
  let int_equal (i: int) (j: int): bool =
    i = j in
  (* like x2s in tanimoto_et_al *)
  let intmap_sum_of_squared_values m =
    IntMap.fold (fun _k v acc ->
        acc + v * v
      ) m 0 in
  (* like xys in tanimoto_et_al *)
  let intmap_sum_of_value_products m1 m2 =
    IntMap.fold (fun k1 v1 acc ->
        let v2 = IntMap.find_default 0 k1 m2 in
        acc + v1 * v2
      ) m1 0 in
  let xys = float (intmap_sum_of_value_products m1 m2) in
  let x2s = float (intmap_sum_of_squared_values m1) in
  let y2s = float (intmap_sum_of_squared_values m2) in
  if IntMap.equal int_equal m1 m2
  then 1.0 (* needed _before_ NaN protection *)
  else if xys = 0.0 then 0.0 (* avoid NaN *)
  else xys /. (x2s +. y2s -. xys) (* regular formula *)
