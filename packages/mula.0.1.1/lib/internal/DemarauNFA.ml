type cost = Cost of int [@@unboxed]

module State = struct
  type t =
    | Std of {lane: int; error: int}
    | Trans of {lane: int; error: int}
  let compare (s1 : t) s2 =
    Stdlib.compare s1 s2

  let pp_state ppf = function
    | Std {lane; error} -> Format.fprintf ppf "(@[%d, %d@])" lane error
    | Trans {lane; error} -> Format.fprintf ppf "(t@[%d, %d@])" lane error
end

module StateSet = struct
  include Set.Make(State)

  let[@inline] subsumed_by s1 s2 =
  let open State in
  match s1, s2 with
  | Std {lane = l1; error = e1}, Std {lane = l2; error = e2} ->
    (e1 < e2) && (l1 + e1 - e2 <= l2 && l2 <= l1 + e2 - e1)
  | Std {lane = l1; error = e1}, Trans {lane = l2; error = e2} ->
    l2 = l1 + 1 && e2 = e1 + 1
  | _ -> false

  let not_subsumed_by x y =
    not (subsumed_by x y)

  let reduce (states : t) : t =
    fold (fun elt acc -> filter (not_subsumed_by elt) acc) states states

  let min_cost (states : t) : int =
    fold (fun (Std {lane = _; error = e} | Trans {lane = _; error = e}) acc -> min e acc) states Int.max_int

  let min_cost_opt (states : t) : int option =
    let min_cost = min_cost states in
    if (Int.equal min_cost Int.max_int) then
      None
    else
      Some min_cost

  let start : t = singleton (State.Std {lane = 0; error = 0})

  let err : t = empty

  let is_err = is_empty

  let pp_comma ppf () =
    Format.fprintf ppf ",@ "

  let pp_states ppf s =
    Format.fprintf ppf "{@[%a@]}"
    (Format.pp_print_list ~pp_sep:pp_comma State.pp_state) (to_seq s |> List.of_seq)

end

module Transitions = struct

  let get_delete_trans s bv ~k : State.t list =
    match s with
    | State.Std {lane = l; error = e} ->
      let err_left = k - e in
      let rec delete_state err_left err_visit =
        if Int.equal err_left 0 then
          []
        else if BitVec.get_right_of_lane ~lane:l ~k ~m:err_visit bv then
          let del = (State.Std {lane = (l + err_visit); error = (e + err_visit)}) in
          if err_visit = 1 then
            (State.Trans {lane = (l - err_visit); error = (e + err_visit)}) :: [del]
          else
            [del]
        else
          delete_state (err_left - 1) (err_visit + 1)
      in
      delete_state err_left 1
    | State.Trans _ -> []

  let get_sub_ins s ~k : State.t list =
    match s with
    | State.Std {lane = l; error = e} ->
      let err_left = k - e in
      if Int.equal err_left 0 then
        []
      else
        [ State.Std {lane = l; error = (e+1)}
        ; State.Std {lane = (l-1); error = (e+1)}
        ]
    | State.Trans _ ->
      []

  let transitions bv ~k s : StateSet.t =
    match s with
    | State.Std {lane = l; error = _} as x ->
      if BitVec.get_lane ~lane:l ~k bv then
        StateSet.singleton x
      else
        StateSet.of_list (List.concat [get_delete_trans s bv ~k;get_sub_ins s ~k])
    | State.Trans {lane; error} ->
      if BitVec.get_lane ~lane ~k bv then
        StateSet.singleton (State.Std {lane = lane +1; error})
      else
        StateSet.empty

  let all_transitions (xs : StateSet.t) bv ~k : StateSet.t =
    StateSet.fold (fun elt acc -> StateSet.union (transitions bv ~k elt) acc) xs StateSet.empty
    |> StateSet.reduce

end
