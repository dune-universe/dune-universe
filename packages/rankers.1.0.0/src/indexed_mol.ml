(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

(* a molecule from the validation set, for which we know the distance to
 * all actives and inactives from the training set *)

module A = MyArray
module Fn = Filename
module FpMol = Molenc.FpMol
module L = MyList

module SL = struct
  type t = string * float (* (name, pred_score) *)
  let create name score =
    (name, score)
  let get_score (_, s) = s
  let get_label (n, _) = FpMol.mol_is_active n
  let get_name (n, _) = n
  (* to do a decreasing sort of a score labels list *)
  let high_score_first_cmp (_, s1) (_, s2) =
    BatFloat.compare s2 s1
  let to_string (name, score) =
    Printf.sprintf "%s %f" name score
end

type t = (* regular use case *)
  { valid_mol: FpMol.t;
    act_dists: float array; (* incr. sorted *)
    dec_dists: float array (* incr. sorted *) }

let dists_to_mol valid_mol n_mols mols =
  let dists = A.create_float n_mols in
  L.iteri (fun i mol ->
      let d = FpMol.dist valid_mol mol in
      A.unsafe_set dists i d
    ) mols;
  A.sort BatFloat.compare dists;
  dists

let create valid_mol actives n_acts decoys n_decs =
  let act_dists = dists_to_mol valid_mol n_acts actives in
  let dec_dists = dists_to_mol valid_mol n_decs decoys in
  { valid_mol; act_dists; dec_dists }

(* score one validation-set molecule using given bwidth (and kernel) *)
let score kernel bwidth indexed =
  let act_contribs, _i =
    A.fold_while (fun _acc d -> d < bwidth) (fun acc d ->
        acc +. (Kernel.eval kernel bwidth d)
      ) 0.0 indexed.act_dists in
  let dec_contribs, _i =
    A.fold_while (fun _acc d -> d < bwidth) (fun acc d ->
        acc +. (Kernel.eval kernel bwidth d)
      ) 0.0 indexed.dec_dists in
  let n_acts = float (A.length indexed.act_dists) in
  let n_decs = float (A.length indexed.dec_dists) in
  let act_contrib = act_contribs /. n_acts in
  let dec_contrib = dec_contribs /. n_decs in
  let score = act_contrib -. dec_contrib in
  let name = FpMol.get_name indexed.valid_mol in
  SL.create name score
