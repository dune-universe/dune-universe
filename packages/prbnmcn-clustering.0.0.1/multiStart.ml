let multi_start ~f ~nstarts =
  let results = Array.init nstarts (fun _ -> f ()) in
  let (cost, result) =
    Array.fold_left
      (fun (min_cost, min_cost_sol) (cost, sol) ->
        if cost < min_cost then (cost, sol) else (min_cost, min_cost_sol))
      results.(0)
      results
  in
  ((cost : float), result)

(* Once Multicore is out, implement this using domainslib *)
(* let multi_start_parallel ~f ~nstarts ~ncores = *)
(*   let results = *)
(*     Parmap.array_parmap *)
(*       ~ncores *)
(*       (fun _ -> *)
(*          Random.set_state (Random.State.make_self_init ()); *)
(*          f () *)
(*       ) (Array.create nstarts ()) *)
(*   in *)
(*   let cost, result = *)
(*     Array.fold_left (fun (min_cost, min_cost_sol) (cost, sol) -> *)
(*         if cost < min_cost then *)
(*           (cost, sol) *)
(*         else *)
(*           (min_cost, min_cost_sol) *)
(*       ) results.(0) results *)
(*   in *)
(*   (cost : float), result *)
