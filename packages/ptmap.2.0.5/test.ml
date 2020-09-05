
open Ptmap

let of_list l =
  List.fold_left (fun acc (k, v) -> add k v acc) empty l

let list_to_map lst =
  let rec loop l map = match l with
  | [] -> map
  | item :: ls -> loop ls (add item true map)
  in
  loop lst empty

(* basic add/mem test *)
let () =
  let max_int = 1 lsl 30 - 1 in
  let seed = Random.int max_int in
  Random.init seed;
  let s =
    let rec loop s i =
      if i = 1000 then s else loop (add (Random.int max_int) true s) (succ i)
    in
    loop empty 0
  in
  Random.init seed;
  for _i = 0 to 999 do assert (mem (Random.int max_int) s) done

(* the bug from "QuickChecking Patricia Trees" is fixed *)
let () =
  let m1 = add min_int true (add 0 true empty) in
  let m2 = add min_int true (add 1 true empty) in
  let m = union (fun _ _ _ -> Some true) m1 m2 in
  assert (cardinal m = 3)

let interval lo hi =
  let rec build k m = if k > hi then m else build (k+1) (add k true m) in
  build lo empty

(* cardinal *)
let () =
  assert (cardinal empty = 0);
  assert (cardinal (of_list [(-1,false); (5,true); (0,false)]) = 3);
  assert (cardinal (interval (-10) 10) = 21)

(* choose *)
let () =
  assert (try let _ = choose empty in false with Not_found -> true);
  assert (choose (add 1 true empty) = (1, true))

(* min/max_binding *)
let () =
  assert (try let _ = min_binding empty in false with Not_found -> true);
  assert (min_binding (of_list [(-1,false); (5,true); (0,false)]) = (-1,false));
  assert (try let _ = max_binding empty in false with Not_found -> true);
  assert (max_binding (of_list [(-1,false); (5,true); (0,false)]) = (5,true))

(* bindings *)
let () =
  assert (bindings empty = []);
  let l = bindings (of_list [(-1,false); (5,true); (0,false)]) in
  assert (List.sort Stdlib.compare l = [(-1,false); (0,false); (5,true)]);
  let itv = bindings (interval (-10) 10) in
  assert (List.length itv = 21)

(* merge *)
let () =
  let l1 = [(-1,-1); (0,0); (5,4)] in
  let l2 = [(5,5)] in
  let l3 = [(-1,-1); (0,0); (5,5)] in
  assert (equal (=) (of_list l3)
            (merge (fun _k x y -> max x y) (of_list l1) (of_list l2)))

(* union *)
let () =
  let l1 = [(-1,false); (0,false); (5,true)] in
  let l2 = [(0,true); (6,true)] in
  let l3 = [(-1,false); (5,true); (6,true)] in
  let m1 = of_list l1 in
  let m2 = of_list l2 in
  let m3 = of_list l3 in
  assert (equal (=) m3 (union (fun _ _ _ -> None) m1 m2))

(* find_first *)
let () =
  let k,_ = find_first (fun i -> i > 3) (list_to_map [3; 1; 2; 4; 6; 5]) in
  assert (k = 4)

(* find_first_opt *)
let () =
  assert (find_first_opt (fun _ -> true) empty = None);
  match find_first_opt (fun i -> i > 3) (list_to_map [3; 1; 2; 4; 6; 5]) with
  | Some (4, _) -> assert true
  | _ -> assert false

(* find_last *)
let () =
  let k,_ = find_last (fun i -> i < 4) (list_to_map [3; 1; 2; 4; 6; 5]) in
  assert (k = 3)

(* find_last_opt *)
let () =
  assert (find_last_opt (fun _ -> true) empty = None);
  match find_last_opt (fun i -> i < 4) (list_to_map [3; 1; 2; 4; 6; 5]) with
  | Some (3, _) -> assert true
  | _ -> assert false

(* update_remove *)
let () =
  let m = update 2 (fun _ -> None) (list_to_map [3; 1; 2; 4; 6; 5]) in
  match find_opt 2 m with
  | None -> assert true
  | _ -> assert false

(* update_add *)
let () =
  let m = update 2 (fun _ -> Some true) (list_to_map [3; 1; 4; 6; 5]) in
  match find_opt 2 m with
  | Some true -> assert true
  | _ -> assert true

(* update_update *)
let () =
  let m = update 2 (fun _ -> Some false) (list_to_map [3; 1; 2; 4; 6; 5]) in
  match find_opt 2 m with
  | Some false -> assert true
  | _ -> assert true

let list_of_seq s = Seq.fold_left (fun l b -> b :: l) [] s
let list_to_seq l =
  let rec aux l () = match l with
    | [] -> Seq.Nil | x :: tail -> Seq.Cons (x, aux tail) in
  aux l

(* to_seq *)
let () =
  let o = [3; 1; 2; 4; 6; 5] in
  let l = list_of_seq (to_seq (list_to_map o)) in
  assert (List.length l = List.length o);
  assert (List.for_all (fun (k,_) -> List.exists ((=) k) o) l)

(* to_seq_from *)
let () =
  let o = [3; 1; 2; 4; 6; 5] in
  let r = [3; 4; 5; 6] in
  let l = list_of_seq (to_seq_from 3 (list_to_map o)) in
  assert (List.length l = List.length r);
  assert (List.for_all (fun (k,_) -> List.exists ((=) k) r) l)

(* of_seq *)
let () =
  let o = [3; 1; 2; 4; 6; 5] in
  let m = of_seq (list_to_seq (List.map (fun v -> (v, true)) o)) in
  assert (cardinal m = List.length o);
  assert (for_all (fun k _ -> List.exists ((=) k) o) m)

(* add_seq *)
let () =
  let o = [3; 1; 6; 5] in
  let a = [2; 4] in
  let r = [3; 1; 2; 4; 6; 5] in
  let m = add_seq (list_to_seq (List.map (fun v -> (v, true)) a)) (list_to_map o) in
  assert (cardinal m = List.length r);
  assert (for_all (fun k _ -> List.exists ((=) k) r) m)
