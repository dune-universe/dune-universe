open Core_kernel
open Import

module S = Incr.Select

let%expect_test "unused nodes are collected" [@tags "no-js"] =
  let var = Incr.Var.create 1 in
  let gen_incr = Staged.unstage (S.select_one (module Int) (Incr.Var.watch var)) in

  let incr0 = gen_incr 0 in
  Caml.Gc.finalise (fun _ -> printf "incr0 collected") incr0;
  let incr1 = gen_incr 1 in

  let o0 = Incr.observe incr0 in
  let o1 = Incr.observe incr1 in
  Incr.stabilize ();
  let get = Incr.Observer.value_exn in

  printf "%B %B" (get o0) (get o1);
  [%expect {| false true |}];

  Incr.Var.set var 0;
  Incr.stabilize ();

  printf "%B %B" (get o0) (get o1);
  [%expect {| true false |}];

  Incr.Observer.disallow_future_use o0;

  let incr0 = gen_incr 0 in
  let o0' = Incr.observe incr0 in

  Incr.stabilize ();

  printf "%B %B" (get o0') (get o1);
  [%expect {| true false |}];

  Gc.full_major ();
  [%expect {| incr0 collected |}]

