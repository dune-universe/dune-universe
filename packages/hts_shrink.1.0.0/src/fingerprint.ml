(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

module Ht = BatHashtbl
module IntSet = BatSet.Int
module IntMap = MyIntMap
module L = MyList

type t = MACCS of Bitv.t
       | ECFP4 of Bitv.t
       | PUBCH of Bitv.t
       | MOP2D of IntSet.t (* an unfolded-uncounted FP *)
       | SFP of int IntMap.t (* an unfolded-counted FP *)

type flavor = MACCSf
            | ECFP4f
            | PUBCHf
            | MOP2Df
            | SFPf

let to_string: t -> string = function
  | MOP2D _ -> failwith "not implemented yet"
  | SFP map -> MyIntMap.to_string string_of_int map
  | MACCS bits
  | PUBCH bits
  | ECFP4 bits -> Bitv.M.to_string bits

let identify: t -> string = function
  | MOP2D _ -> "mop2d"
  | MACCS _ -> "maccs"
  | ECFP4 _ -> "ecfp4"
  | PUBCH _ -> "pubch"
  | SFP _ -> "sfp"

let get_bits: t -> Bitv.t = function
  | MOP2D _ -> failwith "Fp.get_bits: MOP2D"
  | SFP _ -> failwith "Fp.get_bits: SFP"
  | MACCS bits
  | PUBCH bits
  | ECFP4 bits -> bits

let get_ints: t -> IntSet.t = function
  | MOP2D ints -> ints
  | SFP _ -> failwith "Fp.get_ints: SFP"
  | MACCS _ -> failwith "Fp.get_ints: MACCS"
  | PUBCH _ -> failwith "Fp.get_ints: PUBCH"
  | ECFP4 _ -> failwith "Fp.get_ints: ECFP4"

let get_intmap: t -> int IntMap.t = function
  | SFP map -> map
  | MOP2D _ -> failwith "Fp.get_intmap: MOP2D"
  | MACCS _ -> failwith "Fp.get_intmap: MACCS"
  | PUBCH _ -> failwith "Fp.get_intmap: PUBCH"
  | ECFP4 _ -> failwith "Fp.get_intmap: ECFP4"

let count_set_bits: t -> int = function
  | MOP2D ints -> IntSet.cardinal ints
  | SFP _ -> failwith "Fp.count_set_bits: SFP"
  | MACCS bits
  | PUBCH bits
  | ECFP4 bits -> Bitv.pop bits

let size: t -> int = function
  | MOP2D _ -> failwith "Fp.size: MOP2D"
  | SFP _ -> failwith "Fp.size: SFP"
  | MACCS bits
  | PUBCH bits
  | ECFP4 bits -> Bitv.length bits

let union fp1 fp2 = match fp1, fp2 with
  | MOP2D m1, MOP2D m2 -> MOP2D (IntSet.union m1 m2)
  | MACCS m1, MACCS m2 -> MACCS (Bitv.bw_or m1 m2)
  | ECFP4 m1, ECFP4 m2 -> ECFP4 (Bitv.bw_or m1 m2)
  | SFP _, SFP _ -> failwith "Fp.union: SFP"
  | _, _ -> failwith "Fp.union: incompatible FPs"

exception Failed

let of_string s size =
  if Utls.string_contains_only_zeros_or_ones s &&
     String.length s = size then
    Bitv.M.of_string s
  else
    raise Failed

let of_maccs_string (s: string): t =
  try MACCS (of_string s Flags.maccs_length)
  with Failed -> failwith ("Fp.of_maccs_string: non MACCS string: " ^ s)

let of_ecfp4_string (s: string): t =
  try ECFP4 (of_string s Flags.ecfp4_length)
  with Failed -> failwith ("Fp.of_ecfp4_string: non ECFP4 string: " ^ s)

let of_pubch_string (s: string): t =
  try PUBCH (of_string s Flags.pubch_length)
  with Failed -> failwith ("Fp.of_pubch_string: non PUBCH string: " ^ s)

let of_sfp_string (s: string): t =
  try SFP (IntMap.of_string s)
  with Failed -> failwith ("Fp.of_sfp_string: non SFP string: " ^ s)

let tanimoto (query: t) (cand: t): float =
  let count_set_bits (bitv: Bitv.t): float =
    (* specialists call it "population count" *)
    float (Bitv.pop bitv) in
  (* Tanimoto over bitvectors *)
  let bitv_tanimoto (fpA: Bitv.t) (fpB: Bitv.t): float =
    if fpA = fpB then 1.0 (* needed _before_ NaN protection *)
    else
      let numerator = count_set_bits (Bitv.bw_and fpA fpB) in
      if numerator = 0.0 then 0.0 (* avoid NaN *)
      else
        let denominator = count_set_bits (Bitv.bw_or fpA fpB) in
        numerator /. denominator in (* regular formula *)
  match query with
  | MOP2D _ -> failwith "not implemented yet"
  | MACCS _ | PUBCH _ | ECFP4 _ ->
    bitv_tanimoto (get_bits query) (get_bits cand)
  | SFP _ -> Score.intmap_tanimoto (get_intmap query) (get_intmap cand)

(* tanimoto distance (this _is_ a metric) *)
let distance x y =
  1.0 -. (tanimoto x y)

(* compute the centroid of a list of unfolded-counted-FPs *)
let centroid fps =
  match fps with
  | [] -> failwith "Fingerprint.centroid: empty list"
  | _ ->
    let accum = Ht.create 11 in
    L.iter (fun fp ->
        IntMap.iter (fun k v ->
            try
              let curr = Ht.find accum k in
              Ht.replace accum k (v + curr)
            with Not_found ->
              Ht.add accum k v
          ) (get_intmap fp)
      ) fps;
    (* divide *)
    let n = float (L.length fps) in
    Ht.fold (fun k v acc ->
        IntMap.add k (float v /. n) acc
      ) accum IntMap.empty

(* compute the bounding box for a list of unfolded-counted-FPs *)
let bounding_box fps =
  match fps with
  | [] -> failwith "Fingerprint.bounding_box: empty list"
  | _ ->
    let max_ht = Ht.create 11 in
    L.iter (fun fp ->
        IntMap.iter (fun k v ->
            let curr_max = Ht.find_default max_ht k 0 in
            Ht.replace max_ht k (max curr_max v)
          ) (get_intmap fp)
      ) fps;
    max_ht

(* min_bindings are all zeroes since we are using a sparse format *)
let is_inside_bounding_box max_bindings fp =
  IntMap.for_all (fun k v ->
      let maxi = Ht.find_default max_bindings k 0 in
      (v >= 0 && v <= maxi)
    ) (get_intmap fp)
