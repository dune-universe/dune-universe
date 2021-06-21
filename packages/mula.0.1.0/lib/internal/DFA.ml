open NFA

module NFAStateSetSet = Set.Make(NFA.StateSet)
module NFAStateSetMap = Map.Make(NFA.StateSet)

module Transitions = Map.Make(BitVec)

module DFA = struct

  type dfa = (NFA.StateSet.t Transitions.t) NFAStateSetMap.t

  let add_transition ~from:(from : NFA.StateSet.t) (bv : BitVec.t) ~to_:(to_ : NFA.StateSet.t) ~dfa:(dfa : dfa) : dfa =
    match NFAStateSetMap.find_opt from dfa with
    | None -> NFAStateSetMap.add from (Transitions.singleton bv to_) dfa
    | Some trans -> NFAStateSetMap.add from (Transitions.add bv to_ trans) dfa

  type dula_build =
  { marked : NFAStateSetSet.t
  ; unmarked : NFAStateSetSet.t
  ; k : int
  ; dfa : dfa
  }

  let build_transitions (dula : dula_build) ~t =
    let rec build_transitions ({marked; unmarked; k; dfa} as dula) t n max =
      let from = t in
      let bv = (BitVec.Bits n) in
      let transition : NFA.StateSet.t = NFA.Transitions.all_transitions t bv ~k in
      let dfa = add_transition ~from bv ~to_:transition ~dfa in
      let unmarked =
        if NFAStateSetSet.mem transition marked || NFAStateSetSet.mem transition unmarked then
          unmarked
        else
          NFAStateSetSet.add transition unmarked
      in
      let dula = { dula with dfa; unmarked } in
      if n = max then
        dula
      else
        build_transitions dula t (n + 1) max
    in
    let BitVec.(Bits max) =
      let open BitVec in
      pos_fold ~f:(fun _n acc -> snoc_one acc) ~init:(Bits 0) (dula.k * 2 + 1)
    in
    build_transitions dula t 0 max

  let build_dfa ({marked; unmarked; k = _; dfa = _} as dula) =
    match NFAStateSetSet.max_elt_opt unmarked with
    | None -> dula
    | Some t ->
      let marked = NFAStateSetSet.add t marked in
      let unmarked = NFAStateSetSet.remove t unmarked in
      build_transitions { dula with marked; unmarked } ~t

  let start = NFAStateSetSet.singleton (NFA.StateSet.singleton (Lane 0, Err 0))

  let build_dula ~k =
    if (k < 1) || k > 3 then
      failwith "build_dula can only be called with 1 <= k <= 3"
    else
      build_dfa { marked = NFAStateSetSet.empty; unmarked = start; k; dfa = NFAStateSetMap.empty }
end
