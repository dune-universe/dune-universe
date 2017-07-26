open! Core_kernel

include Array
let empty () = [||]
let make1 a = [|a|]
let make2 a b = [|a ; b|]
let make3 a b c = [|a ; b ; c|]
let make4 a b c d = [|a ; b ; c ; d|]
let make5 a b c d e = [|a ; b ; c ; d ; e|]
let sort a ~cmp =
  let a = Array.copy a in
  stable_sort ~cmp a;
  a
let map_stable = Traverse_map.array
let map a ~f = map a ~f
let mapi a ~f = mapi a ~f
let iteri a ~f = iteri a ~f
let of_array = mapi
let to_array = mapi

let of_list_map xs ~f =
  match xs with
  | [] -> [||]
  | hd::tl ->
    let a = create ~len:(1 + List.length tl) (f hd) in
    let rec fill a i = function
      | [] -> a
      | hd::tl -> unsafe_set a i (f hd); fill a (i+1) tl in
    fill a 1 tl
;;

(* It can be written in a more simple (but less efficient) way with [fold_right] *)
let to_list_map arr ~f =
  let rec aux_to_list_map arr f acc index =
    if index < 0 then acc
    else
      let elt = f (unsafe_get arr index) in
      aux_to_list_map arr f (elt::acc) (pred index)
  in
  aux_to_list_map arr f [] (pred (length arr))
;;
