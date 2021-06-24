type lane = Lane of int [@@unboxed]
type errors = Err of int [@@unboxed]
type cost = Cost of int [@@unboxed]

module State = struct
  type t = lane * errors
  let compare ((Lane l1, Err e1) : t) ((Lane l2, Err e2) : t) =
    match Int.compare l1 l2 with
    | 0 -> Int.compare e1 e2
    | diff -> diff

  let pp_state ppf (Lane l, Err e) =
    Format.fprintf ppf "(@[%d, %d@])" l e
end

module StateSet = struct
  include Set.Make(State)

  let[@inline] subsumed_by ((Lane l1, Err e1) : elt) ((Lane l2, Err e2) : elt) =
    if e1 < e2 then
      (l1 + e1 - e2 <= l2 && l2 <= l1 + e2 - e1)
    else
      false

  let not_subsumed_by x y =
    not (subsumed_by x y)

  let reduce (states : t) : t =
    fold (fun elt acc -> filter (not_subsumed_by elt) acc) states states

  let min_cost (states : t) : int =
    fold (fun (_,Err e) acc -> min e acc) states Int.max_int

  let min_cost_opt (states : t) : int option =
    let min_cost = min_cost states in
    if (Int.equal min_cost Int.max_int) then
      None
    else
      Some min_cost

  let start : t = singleton (Lane 0, Err 0)

  let err : t = empty

  let is_err = is_empty

  let print_states s =
    to_seq s
    |> Seq.iter (fun (Lane l, Err e) -> print_int l; print_string " "; print_int e; print_newline ())

  let pp_comma ppf () =
    Format.fprintf ppf ",@ "

  let pp_states ppf s =
    Format.fprintf ppf "{@[%a@]}"
    (Format.pp_print_list ~pp_sep:pp_comma State.pp_state) (to_seq s |> List.of_seq)

end

module Transitions = struct

  let get_delete ((Lane l, Err e) : State.t) bv ~k : State.t option =
    let err_left = k - e in
    let rec delete_state err_left err_visit =
      if Int.equal err_left 0 then
        None
      else if BitVec.get_right_of_lane ~lane:l ~k ~m:err_visit bv then
        Some (Lane (l + err_visit), Err (e + err_visit))
      else
        delete_state (err_left - 1) (err_visit + 1)
    in
    delete_state err_left 1

  let get_sub_ins ((Lane l, Err e) : State.t) ~k : (State.t * State.t) option =
    let err_left = k - e in
    if Int.equal err_left 0 then
      None
    else
      Some (((Lane l, Err (e+1)), (Lane (l-1), Err (e+1))))

  let transitions bv ~k ((Lane l, _) as x : State.t) : StateSet.t =
    if BitVec.get_lane ~lane:l ~k bv then
      StateSet.singleton x
    else
      match get_delete x bv ~k, get_sub_ins x ~k with
      | None, None -> StateSet.empty
      | Some d, None -> StateSet.singleton d
      | Some d, Some (s,i) -> StateSet.of_list [d;s;i]
      | None, Some (s,i) -> StateSet.of_list [s;i]

  let all_transitions (xs : StateSet.t) bv ~k : StateSet.t =
    StateSet.fold (fun elt acc -> StateSet.union (transitions bv ~k elt) acc) xs StateSet.empty
    |> StateSet.reduce

end
