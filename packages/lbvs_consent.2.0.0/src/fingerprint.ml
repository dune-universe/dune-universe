
module Log = Log.Make(struct let section = "Fprt" end)

module S = BatString
module IntSet = MyIntSet

type t = MACCS of Bitv.t
       | ECFP4 of Bitv.t
       | PUBCH of Bitv.t
       | MOP2D of IntSet.t

let to_string: t -> string = function
  | MOP2D ints -> IntSet.to_string ints
  | MACCS bits
  | PUBCH bits
  | ECFP4 bits -> Bitv.M.to_string bits

let identify: t -> string = function
  | MOP2D _ -> "mop2d"
  | MACCS _ -> "maccs"
  | ECFP4 _ -> "ecfp4"
  | PUBCH _ -> "pubch"

let get_bits: t -> Bitv.t = function
  | MOP2D _ -> failwith "Fp.get_bits: MOP2D"
  | MACCS bits
  | PUBCH bits
  | ECFP4 bits -> bits

let get_ints: t -> IntSet.t = function
  | MOP2D ints -> ints
  | MACCS bits -> failwith "Fp.get_ints: MACCS"
  | PUBCH bits -> failwith "Fp.get_ints: PUBCH"
  | ECFP4 bits -> failwith "Fp.get_ints: ECFP4"

let count_set_bits: t -> int = function
  | MOP2D ints -> IntSet.cardinal ints
  | MACCS bits
  | PUBCH bits
  | ECFP4 bits -> Bitv.pop bits

let size: t -> int = function
  | MOP2D _ -> failwith "Fp.size: MOP2D"
  | MACCS bits
  | PUBCH bits
  | ECFP4 bits -> Bitv.length bits

let union fp1 fp2 = match fp1, fp2 with
  | MOP2D m1, MOP2D m2 -> MOP2D (IntSet.union m1 m2)
  | MACCS m1, MACCS m2 -> MACCS (Bitv.bw_or m1 m2)
  | ECFP4 m1, ECFP4 m2 -> ECFP4 (Bitv.bw_or m1 m2)
  | _, _ -> failwith "Fp.union: incompatible FPs"

exception Failed

let of_string s size =
  if MyUtils.string_contains_only_zeros_or_ones s &&
     String.length s = size then
    Bitv.M.of_string s
  else
    raise Failed

let maccs_length = 166
let ecfp4_length = 2048
let pubch_length = 881

let of_maccs_string (s: string): t =
  try MACCS (of_string s maccs_length)
  with Failed ->
    failwith ("of_maccs_string: non MACCS string: " ^ s)

let of_ecfp4_string (s: string): t =
  try ECFP4 (of_string s ecfp4_length)
  with Failed ->
    failwith ("of_ecfp4_string: non ECFP4 string: " ^ s)

let of_pubch_string (s: string): t =
  try PUBCH (of_string s pubch_length)
  with Failed ->
    failwith ("of_pubch_string: non PUBCH string: " ^ s)

(* this one has dataset-dependant variable length *)
let of_mop2d_string (s: string): t =
  let _size_str, set_bit_indexes_str = S.split s ~by:":" in
  let set_bit_indexes = IntSet.of_string set_bit_indexes_str in
  MOP2D set_bit_indexes
