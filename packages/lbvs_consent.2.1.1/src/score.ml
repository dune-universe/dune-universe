
module Log = Dolog.Log.Make(struct let section = "Scor" end)

module A = BatArray
module Fp = Fingerprint
module L = BatList

module IntMap = BatMap.Int
module IntSet = MyIntSet

let count_set_bits (bitv: Bitv.t): float =
  (* specialists call it "population count" *)
  float (Bitv.pop bitv)

let tversky_f alpha a b c =
  let beta = 1.0 -. alpha in
  c /. ((alpha *. a) +. (beta *. b) +. c)

let tversky_i alpha a b c =
  tversky_f alpha (float a) (float b) (float c)

let tanimoto_intset (query: Fp.t) (cand: Fp.t): float =
  IntSet.tanimoto (Fp.get_ints query) (Fp.get_ints cand)

let fp_tanimoto_score (query: Fp.t) (cand: Fp.t): float =
  (* Tanimoto over bitvectors *)
  let bitv_tanimoto (fpA: Bitv.t) (fpB: Bitv.t): float =
    count_set_bits (Bitv.bw_and fpA fpB) /.
    count_set_bits (Bitv.bw_or  fpA fpB) in
  match query with
  | Fp.MACCS _
  | Fp.PUBCH _
  | Fp.ECFP4 _ -> bitv_tanimoto (Fp.get_bits query) (Fp.get_bits cand)
  | Fp.MOP2D _ -> tanimoto_intset query cand

let fp_tversky_score (alpha: float) (query: Fp.t) (cand: Fp.t): float =
  let bitv_tversky (fpA: Bitv.t) (fpB: Bitv.t): float =
    let c = Bitv.pop (Bitv.bw_and fpA fpB) in
    let a = (Bitv.pop fpA) - c in
    let b = (Bitv.pop fpB) - c in
    tversky_i alpha a b c in
  bitv_tversky (Fp.get_bits query) (Fp.get_bits cand)

let fp_tanimoto_dist (query: Fp.t) (cand: Fp.t): float =
  1.0 -. (fp_tanimoto_score query cand)

let get_fp_score: Flags.score -> Fp.t -> Fp.t -> float = function
  | Flags.Tanimoto -> fp_tanimoto_score
  | Flags.Tversky alpha -> fp_tversky_score alpha

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
  let xys, x2s, y2s = tanimoto_et_al xs ys in
  xys /. (x2s +. y2s -. xys)

let array_tversky (alpha: float) (xs: float array) (ys: float array): float =
  let c, x2s, y2s = tanimoto_et_al xs ys in
  let a = x2s -. c in
  let b = y2s -. c in
  tversky_f alpha a b c

let tanimoto (cons: float array) (cand: Fp.t): float =
  let tanimoto_bits (cons: float array) (cand: Bitv.t): float =
    let res = MyUtils.bitv_to_floats cand in
    assert(A.length cons = A.length res);
    array_tanimoto cons res in
  tanimoto_bits cons (Fp.get_bits cand)

let tanimoto_intmap (cons: float IntMap.t) (cand': Fp.t): float =
  let cand = Fp.get_ints cand' in
  let key_values = IntMap.bindings cons in
  let x2s = L.fold_left (fun acc (_k, v) -> acc +. v *. v) 0.0 key_values in
  let y2s = float (IntSet.sum cand) in
  let xys =
    L.fold_left (fun acc (k, v) ->
        if IntSet.mem k cand then
          acc +. v
        else
          acc
      ) 0.0 key_values
  in
  xys /. (x2s +. y2s -. xys)

let tversky (alpha: float) (cons: float array) (cand: Fp.t): float =
  let tversky_bits (cons: float array) (cand: Bitv.t): float =
    let res = MyUtils.bitv_to_floats cand in
    assert(A.length cons = A.length res);
    array_tversky alpha cons res in
  tversky_bits cons (Fp.get_bits cand)

let get_score (flag: Flags.score): (float array -> Fp.t -> float) =
  match flag with
  | Flags.Tanimoto -> tanimoto
  | Flags.Tversky alpha -> tversky alpha

(* (\* how much a query molecule agrees with the probabilistic consensus;
 *    the larger the agreement, the better
 *    WARNING: there is no data backing that this score is better (even for
 *    Knowledgeable consensus) than Tanimoto *\)
 * let agreement (cons: float array) (cand: Fp.t): float =
 *   let agreement_bits (cons: float array) (cand: Bitv.t): float =
 *     assert(A.length cons = Bitv.length cand);
 *     Bitv.foldi_left
 *       (fun acc i bit ->
 *          let p = A.unsafe_get cons i in
 *          let delta = if bit then p else 1.0 -. p in
 *          acc +. delta
 *       ) 0.0 cand in
 *   agreement_bits cons (Fp.get_bits cand) *)
