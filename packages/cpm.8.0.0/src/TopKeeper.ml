
(* keep only the N top scoring elements in memory *)

(* WARNING: we will have several molecules with equal scores when working
            on a huge database *)

module FMap = BatMap.Float

type 'a t = {
  (* elements sorted by increasing scores; elements for a given
     score are in reverse order of entry (LIFO);
     until high_scores_first is called *)
  queue: 'a list FMap.t;
  count: int;
  max: int (* max nb. of molecules to keep *) }

let create_priv queue count max: 'a t =
  { queue; count; max }

let create (n: int): 'a t =
  create_priv FMap.empty 0 n

let insert_priv (score: float) (element: 'a) (map: 'a list FMap.t)
  : 'a list FMap.t =
  try (* score already seen *)
    let previous = FMap.find score map in
    let current = element :: previous in
    FMap.add score current map
  with Not_found -> (* new score *)
    FMap.add score [element] map

let add (element: 'a) (score: float) (acc: 'a t): 'a t =
  if acc.count < acc.max then (* not enough molecules yet *)
    let new_map = insert_priv score element acc.queue in
    create_priv new_map (acc.count + 1) acc.max
  else (* enough molecules already *)
    let (min_score, min_elements), rest = FMap.pop_min_binding acc.queue in
    if score > min_score then
      match min_elements with
      | [] -> assert(false) (* not supposed to happen *)
      | [_one] -> (* forget it and add new *)
        let new_map = insert_priv score element rest in
        create_priv new_map acc.count acc.max
      | _one :: others -> (* forget one and add new *)
        let new_map = FMap.add min_score others rest in
        let new_map = insert_priv score element new_map in
        create_priv new_map acc.count acc.max
    else
      acc

let high_scores_first (acc: 'a t): (float * 'a) list =
  (* put back scores in decreasing order *)
  FMap.fold (fun score elements acc1 ->
      (* put back elements in order of encounter *)
      List.fold_left (fun acc2 element ->
          (score, element) :: acc2
        ) acc1 elements
    ) acc.queue []
