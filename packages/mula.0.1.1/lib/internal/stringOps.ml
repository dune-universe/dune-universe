module type S = sig
  type ch
  type t

  val length : t -> int
  val get : t -> int -> ch
  val equal : ch -> ch -> bool
end

module BitVecOps (St : S) = struct

  (* note that St.get is zero-indexed, so we define a one-indexed version of St.get *)
  let get xs n = St.get xs (n-1)

  (* assumes 1 <= index <= str_len + k *)
  let sizes k index str_len =
    let prefix =
      if index > k then 0 else (k + 1 - index)
    in
    let start = max 1 (index - k) in
    let end_ = min (index + k) str_len in
    let suffix =
      if index + k <= str_len then 0 else index + k - str_len
    in
    (prefix,start,end_,suffix)

  (* assumes 1 <= index <= str_len + k *)
  let bit_vec_of ch str ~index ~k =
    let (_pre_size,start,end_,suf_size) =
      sizes k index (St.length str)
    in
    let prefix_bv = BitVec.zero in
    let index_bv_len = (end_ + 1) - start in (* argh one indexing! *)
    (* 2 - 4. start = 2, end = 4, len = 3, n = 3,2,1... (end + 1)-n = 2,3,4 *)
    let index_bv =
      BitVec.pos_fold
      ~f:(fun n bv -> if St.equal (get str (end_ + 1 - n)) ch then BitVec.snoc_one bv else BitVec.snoc_zero bv )
      ~init:prefix_bv
      index_bv_len
    in
    let suffix_bv =
      BitVec.snoc_zeros index_bv ~m:suf_size
    in
    suffix_bv

  (* assumes str_len < index <= str_len + k *)
  let bit_vec_of_sentinel ~str_len ~index ~k =
    let (pre_size,start,end_,suf_size) =
      sizes k index str_len
    in
    let prefix_bv = BitVec.snoc_ones BitVec.zero ~m:pre_size in
    let index_bv_len = (end_ + 1) - start in (* argh one indexing! *)
    (* 2 - 4. start = 2, end = 4, len = 3, n = 3,2,1... (end + 1)-n = 2,3,4 *)
    let index_bv = BitVec.snoc_zeros prefix_bv ~m:index_bv_len in
    let suffix_bv =
      BitVec.snoc_ones index_bv ~m:suf_size
    in
    suffix_bv
end
