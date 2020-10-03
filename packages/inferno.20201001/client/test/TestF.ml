open Client

(* ∀a b. ({} -> (a * (a -> b))) -> b
   Λa b. fun (p : {} -> (a * (a -> b))) ->
     let (x, f) = p () in f x
*)
let let_prod =
  let open F in
  let alpha, beta = 0, 1 in
  let p, f, x = "p", "f", "x" in
  TyAbs (alpha, TyAbs (beta,
    Abs (p, TyArrow (TyProduct [],
                     TyProduct [TyVar alpha; TyArrow (TyVar alpha, TyVar beta)]),
      LetProd ([x; f], App (Var p, Tuple []),
        App (Var f, Var x)))))


let () =
  Test.(Log.with_log CheckF.test let_prod);
  print_endline "TestF: all tests passed."
