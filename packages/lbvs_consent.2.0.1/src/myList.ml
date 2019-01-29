
include BatList

module Ht = BatHashtbl

let avg (l: int list): float =
  (float (sum l)) /. (float (length l))

(* compute the average list of a list of list of ints *)
let naverage (l: int list list): float list =
  match l with
  | [] -> assert(false)
  | l1 :: _ ->
    let n = float (length l) in
    let array_len = length l1 in
    let arr = Array.make array_len 0.0 in
    (* accumulate *)
    iter (fun li ->
        assert(length li = array_len);
        iteri (fun i x ->
            arr.(i) <- arr.(i) +. (float x)
          ) li
      ) l;
    (* average the accumulated *)
    let res = Array.map (fun x -> x /. n) arr in
    Array.to_list res

let nfaverage (l: float list list): float list =
  match l with
  | [] -> assert(false)
  | l1 :: _ ->
    let n = float (length l) in
    let array_len = length l1 in
    let arr = Array.make array_len 0.0 in
    (* accumulate *)
    iter (fun li ->
        assert(length li = array_len);
        iteri (fun i x ->
            arr.(i) <- arr.(i) +. x
          ) li
      ) l;
    (* average the accumulated *)
    let res = Array.map (fun x -> x /. n) arr in
    Array.to_list res

let singleton x =
  [x]

let medianf (l: float list): float =
  let xs = Array.of_list l in
  Array.sort Pervasives.compare xs;
  let n = Array.length xs in
  if n mod 2 = 1 then
    xs.(n/2)
  else
    0.5 *. (xs.(n/2) +. xs.(n/2 - 1))
(*$T medianf
   medianf [1.;2.;3.;4.;5.] = 3.0
   medianf [1.;2.;3.;4.] = 2.5
*)

let median (l: int list): float =
  let xs = Array.of_list l in
  Array.sort Pervasives.compare xs;
  let n = Array.length xs in
  if n mod 2 = 1 then
    float xs.(n/2)
  else
    0.5 *. float (xs.(n/2) + xs.(n/2 - 1))
(*$T median
   median [1;2;3;4;5] = 3.0
   median [1;2;3;4] = 2.5
   median [1] = 1.0
*)

let nmedian (l: int list list): float list =
  match l with
  | [] -> assert(false)
  | _ ->
    let arrays = map Array.of_list l in
    let n = Array.length (hd arrays) in
    assert(for_all (fun a -> Array.length a = n) arrays);
    let res = Array.make n 0.0 in
    for i = 0 to n - 1 do
      res.(i) <- median (map (fun a -> Array.get a i) arrays)
    done;
    Array.to_list res
(*$T nmedian
  nmedian [[1;2]; [3;4]] = [2.; 3.]
*)

exception Empty

let hd_tl = function
  | x :: xs -> (x, xs)
  | [] -> raise Empty

let nfmedian (l: float list list): float list =
  let rec loop acc xs =
    try
      let head_tails = map hd_tl xs in
      let heads, tails = split head_tails in
      loop (medianf heads :: acc) tails
    with
    | Empty -> rev acc
  in
  match l with
  | [] -> assert(false)
  | x :: _ ->
    let n = length x in
    assert(for_all (fun y -> n = length y) l);
    loop [] l
(*$T nfmedian
  nfmedian [[1.;2.]; [3.;4.]] = [2.; 3.]
*)

module Internal = struct
  let robust_xmedian med_fun l =
    let maybe_head = function
      | x :: xs -> Some (x, xs)
      | [] -> None in
    let rec loop acc l =
      let heads = map maybe_head l in
      let heads = filter BatOption.is_some heads in
      if heads = [] then rev acc
      else
        let heads = map BatOption.get heads in
        let med = med_fun (map fst heads) in
        loop (med :: acc) (map snd heads) in
    loop [] l
end

(* nfmedian that works even when lists don't all have the same size *)
let robust_nfmedian (l: float list list): float list =
  Internal.robust_xmedian medianf l
(*$T robust_nfmedian
  robust_nfmedian [[0.;2.;5.;7.]; [1.;3.]; [2.;4.;6.]] = [1.;3.;5.5;7.]
*)

(* nmedian that works even when lists don't all have the same size *)
let robust_nmedian (l: int list list): float list =
  Internal.robust_xmedian median l
(*$T robust_nmedian
  robust_nmedian [[0;2;5;7]; [1;3]; [2;4;6]] = [1.;3.;5.5;7.]
*)

let variance (l: float list): float =
  let square x = x *. x in
  let sum = fsum l in
  let n = float (length l) in
  let avg = sum /. n in
  let x =
    fold_left (fun acc x ->
        acc +. square (x -. avg)
      ) 0.0 l
  in
  x /. n
(*$T variance
   variance (map float [2;4;4;4;5;5;7;9]) = 4.0
*)

(* standard deviation *)
let sigma l =
  sqrt (variance l)
(*$T sigma
   sigma (map float [2;4;4;4;5;5;7;9]) = 2.0
*)

let stddev = sigma

(* median absolute deviation (useful to detect outliers)
   returns (med, MAD) *)
let mad l =
  let med = medianf l in
  let abs_devs = map (fun x -> abs_float (x -. med)) l in
  (med, medianf abs_devs)
(*$T mad
  mad (map float [1;1;2;2;4;6;9]) = (2., 1.)
*)

let to_string to_str l =
  let buff = Buffer.create 80 in
  Buffer.add_char buff '[';
  iteri (fun i x ->
      if i > 0 then Buffer.add_char buff ';';
      Buffer.add_string buff (to_str x);
    ) l;
  Buffer.add_char buff ']';
  Buffer.contents buff

let of_string of_str s =
  let s' = BatString.chop ~l:1 ~r:1 s in
  map of_str (BatString.nsplit s' ~by:";")

(* shuffle list in non reproducible way (except luck) *)
let real_shuffle l =
  shuffle ~state:(BatRandom.State.make_self_init ()) l

(* pad the end of 'l' with 'n' times value 'v' *)
let pad l n v =
  l @ (make n v)
