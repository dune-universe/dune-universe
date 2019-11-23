open Printf

module Log = Dolog.Log.Make(struct let section = "Cons" end)

module Fp = Fingerprint
module IntMap = BatMap.Int
module IntSet = MyIntSet
module StringSet = BatSet.Make(BatString)
module L = BatList
module Mol = Molecule
module Pol = Policy

type t =
  (* control *)
  | Single              of Mol.t list * Fp.t list
  | Opportunist         of Mol.t list * Fp.t list
  (* MACCS *)
  | Optimist_maccs      of Mol.t list * Fp.t
  | Realist_maccs       of Mol.t list * float array
  | Knowledgeable_maccs of Mol.t list * float array
  (* MOP2D *)
  | Optimist_mop2d      of Mol.t list * Fp.t
  | Realist_mop2d       of Mol.t list * float IntMap.t
  | Knowledgeable_mop2d of Mol.t list * float IntMap.t
  (* ECFP4 *)
  | Optimist_ecfp4      of Mol.t list * Fp.t
  | Realist_ecfp4       of Mol.t list * float array
  | Knowledgeable_ecfp4 of Mol.t list * float array

(* number of times we will have to screen *)
let length: t -> int = function
  | Single (_queries, fprints)
  | Opportunist (_queries, fprints) -> List.length fprints
  | Optimist_mop2d (_, _)
  | Realist_mop2d (_, _)
  | Knowledgeable_mop2d (_, _)
  | Optimist_maccs (_, _)
  | Optimist_ecfp4 (_, _)
  | Realist_maccs (_, _)
  | Realist_ecfp4 (_, _)
  | Knowledgeable_maccs (_, _)
  | Knowledgeable_ecfp4 (_, _) -> 1

let is_single: t -> bool = function
  | Single _ -> true
  | _ -> false

let to_policy: t -> Pol.t = function
  | Single _ -> Pol.Single
  | Opportunist _ -> Pol.Opportunist
  | Optimist_maccs _
  | Optimist_mop2d _
  | Optimist_ecfp4 _ -> Pol.Optimist
  | Realist_maccs _
  | Realist_mop2d _
  | Realist_ecfp4 _ -> Pol.Realist
  | Knowledgeable_maccs _
  | Knowledgeable_mop2d _
  | Knowledgeable_ecfp4 _ -> Pol.Knowledgeable

let to_string (cons: t): string =
  let pol_str = Pol.to_string (to_policy cons) in
  let fp_str = match cons with
    | Single (_mols, fprints)
    | Opportunist (_mols, fprints) -> Fp.identify (L.hd fprints)
    | Optimist_ecfp4 _
    | Realist_ecfp4 _
    | Knowledgeable_ecfp4 _ -> "ecfp4"
    | Optimist_maccs _
    | Realist_maccs _
    | Knowledgeable_maccs _ -> "maccs"
    | Optimist_mop2d _
    | Realist_mop2d _
    | Knowledgeable_mop2d _ -> "mop2d"
  in
  sprintf "%s_%s" pol_str fp_str

let get_queries: t -> Mol.t list = function
  | Single (queries, _)
  | Opportunist (queries, _)
  | Optimist_mop2d (queries, _)
  | Realist_mop2d (queries, _)
  | Knowledgeable_mop2d (queries, _)
  | Optimist_maccs (queries, _)
  | Optimist_ecfp4 (queries, _)
  | Realist_maccs (queries, _)
  | Realist_ecfp4 (queries, _)
  | Knowledgeable_maccs (queries, _)
  | Knowledgeable_ecfp4 (queries, _) -> queries

let get_fprints: t -> Fp.t list = function
  | Single (_queries, fprints)
  | Opportunist (_queries, fprints) -> fprints
  | Optimist_mop2d (_, _)
  | Realist_mop2d (_, _)
  | Knowledgeable_mop2d (_, _)
  | Optimist_maccs (_, _)
  | Optimist_ecfp4 (_, _)
  | Realist_maccs (_, _)
  | Realist_ecfp4 (_, _)
  | Knowledgeable_maccs (_, _)
  | Knowledgeable_ecfp4 (_, _) -> assert(false)

let get_fprint: t -> Fp.t = function
  | Single (_, _)
  | Opportunist (_, _) -> assert(false)
  | Optimist_mop2d (_, fp)
  | Optimist_maccs (_, fp)
  | Optimist_ecfp4 (_, fp) -> fp
  | Realist_mop2d (_, _)
  | Realist_maccs (_, _)
  | Realist_ecfp4 (_, _)
  | Knowledgeable_mop2d (_, _)
  | Knowledgeable_maccs (_, _)
  | Knowledgeable_ecfp4 (_, _) -> assert(false)

(* get the name of all query molecules used in the consensus *)
let get_query_names (cons: t): StringSet.t =
  let queries = get_queries cons in
  let names = L.map Mol.get_name queries in
  StringSet.of_list names

(* score using given consensus query; we just need a candidate to output its score *)
let cons_score (cons_q: t): (Fp.t -> float) =
  match cons_q with
  | Single _ | Opportunist _ -> assert(false)
  | Optimist_maccs (_, cons)
  | Optimist_mop2d (_, cons)
  | Optimist_ecfp4 (_, cons) -> Score.get_fp_score !Flags.curr_score cons
  | Realist_maccs (_, cons)
  | Realist_ecfp4 (_, cons)
  | Knowledgeable_maccs (_, cons)
  | Knowledgeable_ecfp4 (_, cons) -> Score.get_score !Flags.curr_score cons
  | Realist_mop2d (_, cons)
  | Knowledgeable_mop2d (_, cons) ->
    assert(!Flags.curr_score = Flags.Tanimoto);
    Score.tanimoto_intmap cons

(* how many actives were used to create the consensus *)
let size cons =
  L.length (get_queries cons)

(* compute the probability of being set for each bit *)
let bit_frequency (queries: Mol.t list): float array =
  assert(queries <> []);
  let size = Fp.size (Mol.get_fp (List.hd queries)) in
  let frequencies = Array.make size 0.0 in
  let nb_queries = float (L.length queries) in
  (* compute nb times each bit was set *)
  L.iter (fun query ->
      let query_bits = Mol.get_bits query in
      Bitv.iteri (fun i bit ->
          if bit then
            let prev = Array.get frequencies i in
            Array.set frequencies i (prev +. 1.0)
        ) query_bits
    ) queries;
  (* convert to a frequency *)
  Array.iteri (fun i count ->
      Array.set frequencies i (count /. nb_queries)
    ) frequencies;
  frequencies

(* for intset, unfolded FPs *)
let int_frequency (queries: Mol.t list): float IntMap.t =
  assert(queries <> []);
  let nb_queries = float (L.length queries) in
  (* accumulate *)
  let counts =
    L.fold_left (fun acc0 mol ->
        let indexes = Fp.get_ints (Mol.get_fp mol) in
        IntSet.fold (fun index acc1 ->
            try
              let prev_count = IntMap.find index acc1 in
              IntMap.update index index (prev_count + 1) acc1
            with Not_found ->
              IntMap.add index 1 acc1
          ) indexes acc0
      ) IntMap.empty queries
  in
  (* convert to frequencies *)
  IntMap.map (fun count -> (float count) /. nb_queries) counts

let get_pot_scale name_to_weight name =
  try Hashtbl.find name_to_weight name
  with Not_found -> failwith ("get_pot_scale: no IC50 for " ^ name)

(* for intset, unfolded FPs *)
let pot_scaled_int_frequency
    (queries: Mol.t list)
    (name_to_weight: (string, float) Hashtbl.t): float IntMap.t =
  assert(queries <> []);
  let sum_weights = ref 0.0 in
  (* accumulate *)
  let counts =
    L.fold_left (fun acc0 mol ->
        let indexes = Fp.get_ints (Mol.get_fp mol) in
        let name = Mol.get_name mol in
        let weight = get_pot_scale name_to_weight name in
        sum_weights := !sum_weights +. weight;
        IntSet.fold (fun index acc1 ->
            try
              let prev_count = IntMap.find index acc1 in
              IntMap.update index index (prev_count +. weight) acc1
            with Not_found ->
              IntMap.add index weight acc1
          ) indexes acc0
      ) IntMap.empty queries
  in
  (* convert to frequencies *)
  IntMap.map (fun sum -> sum /. !sum_weights) counts

(* same as bit_frequency except that each molecule's contribution
   is potency-scaled *)
let pot_scaled_bit_frequency
    (queries: Mol.t list)
    (name_to_weight: (string, float) Hashtbl.t): float array =
  assert(queries <> []);
  let size = Fp.size (Mol.get_fp (List.hd queries)) in
  let frequencies = Array.make size 0.0 in
  let sum_weights = ref 0.0 in
  (* compute nb times each bit was set and scale it *)
  L.iter (fun query ->
      let query_bits = Mol.get_bits query in
      let query_name = Mol.get_name query in
      let weight = get_pot_scale name_to_weight query_name in
      sum_weights := !sum_weights +. weight;
      Bitv.iteri (fun i bit ->
          if bit then
            let prev = Array.get frequencies i in
            Array.set frequencies i (prev +. weight)
        ) query_bits
    ) queries;
  (* convert to a frequency *)
  Array.iteri (fun i count ->
      Array.set frequencies i (count /. !sum_weights)
    ) frequencies;
  frequencies

let debug_bit_cons (cons: Bitv.t): unit =
  Printf.printf "%s\n" (Bitv.M.to_string cons)

let debug_freq_cons (freqs: float array): unit =
  let n = (Array.length freqs) - 1 in
  (* Bitv.iteri iterates from least to most significant bits:
     i.e. bit at index 0 is the least significant one.
     So, we walk the array from its end to output most
     significant bits first (on the left of the display, like
     regular numbers written by humans). *)
  for i = n downto 0 do
    Printf.printf
      (if i = n then "%.2f" else " %.2f")
      freqs.(i)
  done;
  Printf.printf "\n"

let logical_consensus strat queries =
  match queries with
  | [] -> failwith "logical_consensus: no queries"
  | q :: _ ->
    let fp = Mol.get_fp q in
    let fprints = L.map Mol.get_fp queries in
    let opti_cons = L.reduce Fp.union fprints in
    match strat, fp with
    | Pol.Optimist, Fp.MACCS _ -> Optimist_maccs (queries, opti_cons)
    | Pol.Optimist, Fp.ECFP4 _ -> Optimist_ecfp4 (queries, opti_cons)
    | Pol.Optimist, Fp.MOP2D _ -> Optimist_mop2d (queries, opti_cons)
    | _ -> failwith "Cons.logical_consensus: unsupported strat"

let create
    ?verbose:(verbose = false)
    (strat: Pol.t)
    (queries: Mol.t list): t =
  if verbose then
    (L.iter (fun m ->
         let fp = Mol.get_fp m in
         Printf.printf "%s\n" (Fp.to_string fp)
       ) queries;
     Printf.printf "---\n");
  match strat with
  | Pol.Single
  | Pol.Opportunist ->
    (* use query FP as is *)
    begin match queries with
      | [] -> assert(false) (* we need at least one molecule *)
      | _ ->
        let name_fprints =
          L.map (fun mol ->
              Mol.(get_name mol, get_fp mol)
            ) queries
        in
        let fprints = L.map snd name_fprints in
        match strat with
        | Pol.Single -> Single (queries, fprints)
        | Pol.Opportunist -> Opportunist (queries, fprints)
        | _ -> assert(false)
    end
  | Pol.Optimist -> (* logical OR *)
    let cons = logical_consensus strat queries in
    if verbose then debug_bit_cons (Fp.get_bits (get_fprint cons));
    cons
  | Pol.Realist -> (* probability per bit *)
    (match Mol.get_fp (List.hd queries) with
     | Fp.MACCS _ ->
       let a = bit_frequency queries in
       if verbose then debug_freq_cons a;
       Realist_maccs (queries, a)
     | Fp.PUBCH _ ->
       failwith "Consensus.create: realist not supported for PUBCH"
     | Fp.MOP2D _ ->
       let a = int_frequency queries in
       Realist_mop2d (queries, a)
     | Fp.ECFP4 _ ->
       let a = bit_frequency queries in
       if verbose then debug_freq_cons a;
       Realist_ecfp4 (queries, a))
  | Pol.Knowledgeable ->
    (* <=> realist + activity scaling of each molecule *)
    let _, weights = Mol.potency_scale queries in
    match Mol.get_fp (List.hd queries) with
    | Fp.MOP2D _ ->
      let a = pot_scaled_int_frequency queries weights in
      Knowledgeable_mop2d (queries, a)
    | _ ->
      let a = pot_scaled_bit_frequency queries weights in
      match Mol.get_fp (List.hd queries) with
      | Fp.MOP2D _ -> assert(false)
      | Fp.MACCS _ -> Knowledgeable_maccs (queries, a)
      | Fp.ECFP4 _ -> Knowledgeable_ecfp4 (queries, a)
      | Fp.PUBCH _ ->
        failwith "Consensus.create: knowledgeable not supported for PUBCH"
