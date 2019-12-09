open Idds
open Base

module A : Boolean.Algebra = Bdd.Make()
module Boolean_tests = Boolean_test.Make(A)

module Tests = struct
  (* set up *)
  let mgr = Bdd.manager ()
  let vars = 3
  let envs = List.init (2**vars) ~f:(fun k var ->
    (k lsr (Var.index var)) % 2 = 1
  )
  let rec mk_all_trees n : (Bdd.t * ((Var.t -> bool) -> bool)) list =
    if n <= 0 then
      [(Bdd.ctrue, fun _ -> true); (Bdd.cfalse, fun _ -> false)]
    else
      let ts = mk_all_trees (n-1) in
      List.cartesian_product ts ts
      |> List.map ~f:(fun ((hi, f_hi), (lo, f_lo)) ->
          Bdd.ite mgr (Var.inp n) hi lo,
          fun env -> if env (Var.inp n) then f_hi env else f_lo env
        )
  let all_trees = mk_all_trees vars
  let all_two_trees = List.cartesian_product all_trees all_trees

  let%test "binary operators correct (3 variables, exhaustive)" =
    [Bdd.conj mgr, (&&); Bdd.disj mgr, (||)]
    |> List.for_all ~f:(fun (bdd_op, bool_op) ->
      List.for_all all_two_trees ~f:(fun ((bdd1, bool1), (bdd2, bool2)) ->
        let bdd = bdd_op bdd1 bdd2 in
        let bool env = bool_op (bool1 env) (bool2 env) in
        List.for_all envs ~f:(fun env ->
          Bool.equal (Bdd.eval bdd ~env) (bool env)
        )
      )
    )

  let%test "negation correct" =
    List.for_all all_trees ~f:(fun (bdd, _) ->
      let bdd' = Bdd.neg mgr bdd in
      List.for_all envs ~f:(fun env ->
        not @@ Bool.equal (Bdd.eval bdd ~env) (Bdd.eval bdd' ~env)
      )
    )

  let%test "ite correct" =
    List.for_all all_two_trees ~f:(fun ((bdd1, _), (bdd2, _)) ->
      List.init vars ~f:(fun i -> i)
      |> List.for_all ~f:(fun var ->
        let bdd = Bdd.ite mgr (Var.inp var) bdd1 bdd2 in
        List.for_all envs ~f:(fun env ->
          if env (Var.inp var) then
            Bool.equal (Bdd.eval bdd ~env) (Bdd.eval bdd1 ~env)
          else
            Bool.equal (Bdd.eval bdd ~env) (Bdd.eval bdd2 ~env)
        )
      )
    )

end
