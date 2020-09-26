open Core_kernel
open Import

module S = Incr.Select

let%expect_test "multiple nodes for the same key" =
  let var = Incr.Var.create 0 in
  let gen_incr = Staged.unstage (S.select_one (module Int) (Incr.Var.watch var)) in

  let incr0 = gen_incr 0 in
  let incr0' = gen_incr 0 in
  let incr1 = gen_incr 1 in

  let o0 = Incr.observe incr0 in
  let o0' = Incr.observe incr0' in
  let o1 = Incr.observe incr1 in

  let get = Incr.Observer.value_exn in

  Incr.Var.set var 0;
  Incr.stabilize ();

  printf "%B %B %B" (get o0) (get o0') (get o1);
  [%expect {| true true false |}];

  Incr.Var.set var 1;
  Incr.stabilize ();

  printf "%B %B %B" (get o0) (get o0') (get o1);
  [%expect {| false false true |}];

  Incr.Var.set var 0;
  Incr.stabilize ();

  printf "%B %B %B" (get o0) (get o0') (get o1);
  [%expect {| true true false |}]

