open Internal

module type S = sig
  type ch
  type t

  val length : t -> int
  val get : t -> int -> ch
  val equal : ch -> ch -> bool
end

module type NFA_t = sig
  module StateSet : sig
    type t

    val start : t
    val err : t

    val min_cost_opt : t -> int option
    val is_err : t -> bool
  end
  module Transitions : sig
    val all_transitions : StateSet.t -> BitVec.t -> k:int -> StateSet.t
  end
end

module MakeMatcher (St : S) (NFA : NFA_t) = struct

  module GBV = StringOps.BitVecOps (St)

  type nfa_state = {nfa : NFA.StateSet.t; k : int; str: St.t; str_len: int; fed_so_far: int}

  let start ~k ~str =
    if (k < 0) then
      failwith "the limit k cannot be negative"
    else if (k > ((Sys.int_size - 1) / 2)) then
      failwith "the limit k cannot be larger than ((int_size - 1) / 2)"
    else
      {nfa = NFA.StateSet.start; k; str; str_len = St.length str; fed_so_far = 0 }

  let feed {nfa;k;str;str_len;fed_so_far} ~ch =
    let index = fed_so_far + 1 in
    if NFA.StateSet.is_err nfa
      || index > str_len + k then
      {nfa = NFA.StateSet.err; k; str; str_len; fed_so_far = index}
    else
      let bv = GBV.bit_vec_of ch str ~index ~k in
      let nfa = NFA.Transitions.all_transitions nfa bv ~k in
      {nfa;k;str;str_len;fed_so_far = index}

  let current_error {nfa;_} : int option =
    if NFA.StateSet.is_err nfa then
      None
    else
      NFA.StateSet.min_cost_opt nfa

  let end_input {nfa;k;str=_;str_len;fed_so_far} : int option =
    let size_diff = str_len - fed_so_far in
    if NFA.StateSet.is_err nfa (* handles over feeding *)
      || size_diff > k then (* handle under feeding *)
      None
    else
      (* add (str_len - fed_so_far + k) many sentinels *)
      let sentinels = str_len - fed_so_far + k in
      let nfa =
        BitVec.pos_fold
        ~f:(fun n nfa ->
          let bv = GBV.bit_vec_of_sentinel ~str_len ~index:(fed_so_far + 1 + sentinels - n) ~k in
          NFA.Transitions.all_transitions nfa bv ~k)
          ~init:nfa
          sentinels
      in
      NFA.StateSet.min_cost_opt nfa

  let feed_str nfa_state ~str =
    (* TODO early exit *)
    let len = St.length str in
    BitVec.pos_fold
    ~f:(fun n nfa -> feed nfa ~ch:(St.get str (len - n)))
    ~init:nfa_state
    len

  let get_distance ~k str1 str2 =
    let len_diff =
      let len1 = St.length str1 in
      let len2 = St.length str2 in
      Int.abs (len1 - len2)
    in
    if len_diff > k then
      None
    else
      let start = start ~k ~str:str1 in
      let end_ = feed_str start ~str:str2 in
      let cost = end_input end_ in
      cost
end

module Make (St : S) = struct
  module Lev = MakeMatcher (St) (NFA)
  module Dem = MakeMatcher (St) (DemarauNFA)
end
