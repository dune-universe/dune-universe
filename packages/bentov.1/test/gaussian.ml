open Seq

(** Knuth algorithm for producing a sample from N(0,1) *)
let random_normal : float Seq.t =
  let rec gen phase () =
    let u1 = Random.float 1.0 in
    let u2 = Random.float 1.0 in
    let v1 = 2. *. u1 -. 1.0 in
    let v2 = 2. *. u2 -. 1.0 in
    let s = v1 *. v1 +. v2 *. v2 in
    if s >= 1.0 || s = 0.0 then
      gen phase ()
    else
      let v = if phase then v1 else v2 in
      let x = v *. sqrt ((-2. *. log s) /. s) in
      Cons (x, gen (not phase))
  in
  gen true

(* [random_normal ~mu ~sigma] produces a sequence whose elements
   are N([mean],[stddev]) *)
let seq ~mu ~sigma =
  if mu = 0. && sigma = 1.0 then
    random_normal
  else
    Seq.map (fun x -> mu +. x *. sigma) random_normal

(*
let mean_std x =
  let n = List.length x in
  let sum = List.fold_left ( +. ) 0. x in
  let mean = sum /. (float n) in
  let rss = List.fold_left (fun rss x -> rss +. (x -. mean)**2.) 0. x in
  let std = sqrt (rss /. (float (n-1))) in
  mean, std


let _ =
  let x = take_rev 100_000 (seq ~mu:0. ~stddev:1.) in
  let qs = quantiles x 10 in
  List.iter (fun (i, q) -> Printf.printf "%d %f\n" i q) qs
*)
