(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

module A = Array
module L = MyList
module Ht = BatHashtbl

(* scan d for all actives at the same time *)
let global_dscan ds res train test =
  let actives_train = L.filter FpMol.is_active train in
  let actives_bst = Bstree.(create 1 Two_bands (A.of_list actives_train)) in
  (* compute distance to nearest active for each molecule in test set *)
  let unsorted =
    L.map (fun test_mol ->
        let _nearest_act, nearest_dist =
          Bstree.nearest_neighbor test_mol actives_bst in
        (nearest_dist, test_mol)
      ) test in
  (* increasing sort *)
  let sorted' =
    L.sort (fun (d1, _m1) (d2, _m2) -> BatFloat.compare d1 d2) unsorted in
  let sorted = ref sorted' in
  L.iter (fun d ->
      sorted := L.filter (fun (d1, _m1) -> d1 <= d) !sorted;
      let actives, decoys =
        L.partition (fun (_d, m) -> FpMol.is_active m) !sorted in
      let act_card, dec_card = L.length actives, L.length decoys in
      let act_card', dec_card' = Ht.find_default res d (0, 0) in
      Ht.replace res d (act_card' + act_card, dec_card' + dec_card)
    ) ds

(* a molecule is inside the AD if it is nearer than [d] to any of
   the known actives *)
let mol_is_inside_global_AD mol d actives_bst =
  let _nearest_active, dist = Bstree.nearest_neighbor mol actives_bst in
  dist <= d
