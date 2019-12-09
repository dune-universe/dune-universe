open Idds
open Base

let index (idd : Idd.t) = Dd.index (idd :> Dd.t)

module Basic = struct
  let mgr = Idd.manager ()
  let vars = 2
  let x0 = Var.inp 0
  let y0 = Var.out 0
  let x1 = Var.inp 1
  let y1 = Var.out 1
  let x2 = Var.inp 2
  let y2 = Var.out 2
  let x99 = Var.inp 99
  let y99 = Var.out 99

  let rec mk_all_trees n : Idd.t list =
    if n <= 0 then
      [Idd.ident; Idd.empty]
    else
      let ts = mk_all_trees (n-1) in
      let var = if n%2 = 0 then Var.out (n/2) else Var.inp (n/2) in
      List.cartesian_product ts ts
      |> List.map ~f:(fun (hi, lo) ->
          Idd.branch mgr var hi lo
        )
  let trees = mk_all_trees (2*vars)

  let%test "branch reduction: xi ? u : u -> u" =
    List.for_all trees ~f:(fun t ->
      Idd.equal (Idd.branch mgr x99 t t) t
    )

  let%test "branch reduction: yi ? u : u -> u iff u=false" =
    Idd.(equal (branch mgr y99 empty empty) empty) &&
    List.for_all trees ~f:(fun t ->
      Idd.(equal t empty) || not Idd.(equal (branch mgr y99 t t) t)
    )

  let small_trees = mk_all_trees 3

  let%test "iia" =
    List.cartesian_product small_trees small_trees |>
    List.for_all ~f:(fun (u,v) -> Idd.(equal
    (branch mgr (Var.inp 10) (branch mgr (Var.out 10) u empty) v)
    (branch mgr (Var.inp 10) u v)))

  let%test "iib" =
    List.cartesian_product small_trees small_trees |>
    List.for_all ~f:(fun (u,v) -> Idd.(equal
    (branch mgr (Var.inp 10) u (branch mgr (Var.out 10) empty v))
    (branch mgr (Var.inp 10) u v)))

  let%test "iiia" =
    List.for_all trees ~f:(fun u -> Idd.(equal
    (branch mgr (Var.inp 10) (branch mgr (Var.out 10) empty u) u)
    (branch mgr (Var.out 10) empty u)))

  let%test "iiib" =
    List.for_all trees ~f:(fun u -> Idd.(equal
    (branch mgr (Var.inp 10) u (branch mgr (Var.out 10) u empty))
    (branch mgr (Var.out 10) u empty)))

  let%test "eval" =
    Idd.(
      not (eval empty (fun _ -> true) 10) &&
      eval ident (fun _ -> true) 1  &&
      not (eval ident (fun x -> Var.equal x (Var.inp 0)) 1) &&
      let branchtree = Idd.(branch mgr x2 empty ident) in
      not (eval branchtree (fun _ -> true) 3) &&
      eval branchtree (fun x -> not ((Var.index x) = 2)) 3 &&
      not (eval branchtree (fun x -> not (Var.equal x x2)) 3) &&
      let outvartree = Idd.(branch mgr y2 empty ident) in
      eval outvartree (fun x -> not (Var.equal x y2)) 3
    )

  (* Apply tests *)
  let%test "(x = 1) conj (x <- 0 + x <- 1)" =
    Idd.(equal (apply mgr (&&) (branch mgr (Var.inp 0) ident empty)
                  (branch mgr (Var.out 0) ident ident))
           (branch mgr (Var.inp 0) ident empty))

  let%test "(x = 1) disj (x <- 0 + x <- 1)" =
    Idd.(equal (apply mgr (||) (branch mgr (Var.inp 0) ident empty)
                  (branch mgr (Var.out 0) ident ident))
           (branch mgr (Var.out 0) ident ident))

  (* Helper function to create a random environment where variable indices
    range from 0 to [n-1] *)
  let random_env n : Var.t -> bool =
    List.range 0 n |> List.concat_map ~f:(fun i -> [Var.inp i; Var.out i]) |>
    List.fold ~init:(fun _ -> failwith "unbound in environment")
      ~f:(fun acc var -> let value = Random.bool () in
           fun v -> if Var.equal v var then value else acc v)

  let%test "apply-eval compatibility" =
    List.cartesian_product small_trees small_trees |>
    List.cartesian_product [(||); (&&)] |>
    List.for_all ~f:Idd.(fun (op, (u,v)) ->
        let maxu = (index u) + 1 in
        let maxv = (index v) + 1 in
        let env = random_env (max maxu maxv) in
        let ur = (op (eval u env (max maxu maxv)) (eval v env (max maxu maxv))) in
        let app = apply mgr op u v in
        let vr = (eval app env (max maxu maxv)) in
        Bool.equal ur vr)

    let%test "apply and union agree" =
    List.cartesian_product small_trees small_trees
    |> List.for_all ~f:(fun (u,v) ->
      Idd.(equal (apply mgr (||) u v) (union mgr u v))
    )

  (* Seq tests *)

  (** returns list of all 2^n possible boolean lists *)
  let all_lsts n =
    let rec help n lsts =
      if n = 0 then lsts
      else
        let f = List.map ~f:(fun lst -> false::lst) lsts in
        let t = List.map ~f:(fun lst -> true::lst) lsts in
        help (n-1) (List.append f t)
    in
    help n [[]]

  (** requires: argument lists have same length *)
  let env_from_lsts in_f_lst out_f_lst =
    let pairs = List.zip_exn in_f_lst out_f_lst in
    List.foldi pairs ~init:(fun _ -> failwith "Unbound in environment")
    ~f:(fun i acc (b1, b2) -> let inp, out = Var.inp i, Var.out i in
       fun v -> if Var.equal v inp then b1 else if Var.equal v out then b2
         else acc v)

  let%test "seq-eval compatibility" =
   List.cartesian_product small_trees small_trees |>
   List.for_all ~f:(fun (t1, t2) ->
       Idd.(
         let max1, max2 = index t1 + 1, index t2 + 1 in
         let max_idx = max max1 max2 in
         let alllsts = all_lsts max_idx in
         List.cartesian_product alllsts alllsts |>
         List.for_all ~f:(fun (lst1, lst3) ->
             let expected = List.exists alllsts ~f:(fun lst2 ->
                 eval t1 (env_from_lsts lst1 lst2) max_idx &&
                 eval t2 (env_from_lsts lst2 lst3) max_idx) in
             Bool.equal expected
               (eval (seq mgr t1 t2) (env_from_lsts lst1 lst3) max_idx)
          )
       )
     )

  (* Star tests *)

  let is_transitive d0 max_idx =
    Idd.(
      let alllsts = all_lsts max_idx in
      List.cartesian_product alllsts alllsts |>
      List.cartesian_product alllsts |>
      List.for_all ~f:(fun (lst1, (lst2, lst3)) ->
          if eval d0 (env_from_lsts lst1 lst2) max_idx &&
             eval d0 (env_from_lsts lst2 lst3) max_idx then
            eval d0 (env_from_lsts lst1 lst3) max_idx
          else true
        )
    )

  let%test "[star d0] contains d0 and is reflexive and transitive" =
    List.for_all trees ~f:(fun d0 ->
        Idd.(
          let starred = star mgr d0 in
          let max1, max2 = index d0 + 1, index starred + 1 in
          let max_idx = max max1 max2 in
          (* contains d0 check *)
          subseteq mgr d0 starred &&
          (* reflexive check *)
          subseteq mgr ident starred &&
          (* transitive check *)
          is_transitive starred max_idx
        )
      )

  let%test "[star d0] is the smallest" =
    List.for_all small_trees ~f:(fun d0 ->
        Idd.(
          let starred = star mgr d0 in
          let max1 = index d0 + 1 in
          List.for_all small_trees ~f:(fun d1 ->
            let max2 = index d1 + 1 in
            let max_idx = max max1 max2 in
            if subseteq mgr ident d1 && subseteq mgr d0 d1 &&
               is_transitive d1 max_idx
            then subseteq mgr starred d1 else true
          )
        )
    )

end
