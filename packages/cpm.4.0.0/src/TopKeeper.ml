(* keep only the top N scoring molecules in memory *)

(* FBR: add .mli file *)

(* WARNING: we will have several molecules with equal scores when working
            on a huge database *)

module FMap = BatMap.Make(BatFloat)

type t = { queue: string list FMap.t; (* molecule names sorted by
                                         increasing scores; names for a given
                                         score are in reverse order
                                         of encounter (LIFO) until high_scores_first is called *)
           count: int;
           max: int } (* max nb. of molecules to keep *)

let create_priv queue count max: t =
  { queue; count; max }

let create (n: int): t =
  create_priv FMap.empty 0 n

let insert_priv (score: float) (name: string) (map: string list FMap.t)
  : string list FMap.t =
  try (* score already seen *)
    let previous = FMap.find score map in
    let current = name :: previous in
    FMap.add score current map
  with Not_found -> (* new score *)
    FMap.add score [name] map

let add (name: string) (score: float) (acc: t): t =
  if acc.count < acc.max then (* not enough molecules yet *)
    let new_map = insert_priv score name acc.queue in
    create_priv new_map (acc.count + 1) acc.max
  else (* enough molecules already *)
    let (min_score, min_names), rest = FMap.pop_min_binding acc.queue in
    if score > min_score then
      match min_names with
      | [] -> assert(false) (* not supposed to happen *)
      | [_one] -> (* forget it and add new *)
        let new_map = insert_priv score name rest in
        create_priv new_map acc.count acc.max
      | _one :: others -> (* forget one and add new *)
        let new_map = FMap.add min_score others rest in
        let new_map = insert_priv score name new_map in
        create_priv new_map acc.count acc.max
    else
      acc

let high_scores_first (acc: t): (float * string) list =
  (* put back scores in decreasing order *)
  FMap.fold (fun score names acc1 ->
      (* put back names in encounter order *)
      List.fold_left (fun acc2 name ->
          (score, name) :: acc2
        ) acc1 names
    ) acc.queue []
