open Core

module S = Set.Make(Group);;

let rec cartesian_product = function
  | [] -> [[]]
  | h :: t ->
    let rest = cartesian_product t in
    List.concat
      (List.map h ~f:(fun i -> List.map rest ~f:(fun r -> i :: r)))

let product gs =
  let c = cartesian_product gs in
  let c = List.map c ~f:Group.sort in
  S.to_list (S.of_list c) 
