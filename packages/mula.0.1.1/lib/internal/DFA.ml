open NFA

module NFAStateSetSet = struct
  include Set.Make(NFA.StateSet)

  let pp_states_set ppf s =
    Format.fprintf ppf "{@[%a@]}"
    (Format.pp_print_list ~pp_sep:StateSet.pp_comma StateSet.pp_states) (to_seq s |> List.of_seq)
end

module NFAStateSetMap = struct
  include Map.Make(NFA.StateSet)

  let pp_kv_pair ~pp_val ppf (k,v) =
    Format.fprintf ppf "@[%a ->@ %a@]" StateSet.pp_states k pp_val v

  let pp_map ~pp_val ppf map =
    Format.fprintf ppf "{@[%a@]}"
    (Format.pp_print_list ~pp_sep:StateSet.pp_comma (pp_kv_pair ~pp_val)) (to_seq map |> List.of_seq)
end

module Transitions = struct
  include Map.Make(BitVec)

  let pp_kv_pair ~pp_val ppf (k,v) =
    Format.fprintf ppf "@[%a ->@ %a@]" BitVec.pp_bv k pp_val v

  let pp_map ~pp_val ppf map =
    Format.fprintf ppf "[@[%a@]]"
    (Format.pp_print_list ~pp_sep:StateSet.pp_comma (pp_kv_pair ~pp_val)) (to_seq map |> List.of_seq)
end

module DFA = struct

  type dfa = (NFA.StateSet.t Transitions.t) NFAStateSetMap.t

  let add_key ~from:(from : NFA.StateSet.t) ~dfa:(dfa : dfa) : dfa =
    match NFAStateSetMap.find_opt from dfa with
    | None -> NFAStateSetMap.add from (Transitions.empty) dfa
    | Some _ -> dfa

  let add_transition ~from:(from : NFA.StateSet.t) (bv : BitVec.t) ~to_:(to_ : NFA.StateSet.t) ~dfa:(dfa : dfa) : dfa =
    let dfa =
      match NFAStateSetMap.find_opt from dfa with
      | None -> NFAStateSetMap.add from (Transitions.singleton bv to_) dfa
      | Some trans -> NFAStateSetMap.add from (Transitions.add bv to_ trans) dfa
    in
    (* make sure the to_ is in the set of keys*)
    add_key ~from:to_ ~dfa

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
      BitVec.ones ~m:(dula.k * 2 + 1)
    in
    build_transitions dula t 0 max

  let rec build_dfa ({marked; unmarked; k = _; dfa = _} as dula) =
    match NFAStateSetSet.max_elt_opt unmarked with
    | None -> dula
    | Some t ->
      let marked = NFAStateSetSet.add t marked in
      let unmarked = NFAStateSetSet.remove t unmarked in
      let dula =
        build_transitions { dula with marked; unmarked } ~t
      in
      build_dfa dula

  let start = NFAStateSetSet.of_list [NFA.StateSet.singleton (Lane 0, Err 0);NFA.StateSet.empty]

  let build_dula ~k =
    if (k < 1) || k > 3 then
      failwith "build_dula can only be called with 1 <= k <= 3"
    else
      build_dfa { marked = NFAStateSetSet.empty; unmarked = start; k; dfa = NFAStateSetMap.empty }

  let build_and_print_dula ~k =
    let {dfa;_} = build_dula ~k in
    NFAStateSetMap.pp_map ~pp_val:(Transitions.pp_map ~pp_val:NFA.StateSet.pp_states) Format.std_formatter dfa;
    Format.pp_print_newline Format.std_formatter ()
end
