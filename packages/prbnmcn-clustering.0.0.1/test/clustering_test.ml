open Clustering

type t = { x : float; y : float }

let compare (a : t) (b : t) =
  if a.x < b.x then -1
  else if a.x > b.x then 1
  else if a.y < b.y then -1
  else if a.y > b.y then 1
  else 0

module R2 : K_means.Element with type t = t = struct
  type nonrec t = t

  let dist (a : t) (b : t) =
    sqrt @@ (((a.x -. b.x) ** 2.) +. ((a.y -. b.y) ** 2.))

  let mean (arr : t array) =
    let acc_x = ref 0.0 in
    let acc_y = ref 0.0 in
    for i = 0 to Array.length arr - 1 do
      let p = arr.(i) in
      acc_x := !acc_x +. p.x ;
      acc_y := !acc_y +. p.y
    done ;
    { x = !acc_x; y = !acc_y }
end

let normal =
  let pi = 4. *. atan 1. in
  let saved = ref None in
  fun st ->
    match !saved with
    | Some (r, t) ->
        saved := None ;
        r *. sin t
    | None ->
        let u1 = Random.State.float st 1. in
        let u2 = Random.State.float st 1. in
        let r = sqrt (-2. *. log u1) in
        let t = 2. *. pi *. u2 in
        saved := Some (r, t) ;
        r *. cos t

let normal2 st mu = { x = normal st +. mu.x; y = normal st +. mu.y }

let cloud1 st = Array.init 100 (fun _ -> normal2 st { x = 5.0; y = 3.0 })

let cloud2 st = Array.init 100 (fun _ -> normal2 st { x = 15.0; y = 18.0 })

module K = K_means.Make (R2)

let result st =
  let cloud1 = cloud1 st in
  let cloud2 = cloud2 st in
  let outcome =
    K.k_means
      ~k:2
      ~init:K_means.KmeansPP
      ~elements:(Array.concat [cloud1; cloud2])
      ~termination:(K_means.Threshold 0.01)
      st
  in
  (cloud1, cloud2, outcome)

let rng_state = Random.State.make [| 0x1337; 0x533D |]

let (cloud1, cloud2, inferred) = result rng_state

(* We expect k-means to perfectly separate these two clouds of points
   with very high probability. *)

let class1 = inferred.(0)

let () = Array.sort compare class1

let class2 = inferred.(1)

let () = Array.sort compare class2

let () = Array.sort compare cloud1

let () = Array.sort compare cloud2

let () =
  assert (
    (cloud1 = class1 && cloud2 = class2) || (cloud1 = class2 && cloud2 = class1))
