open Core
open Bigint

let%bench_fun "random" =
  let state = Random.State.make [| 1 ; 2 ; 3 |] in
  let range = shift_left one 10_000 in
  fun () -> random ~state range

let%bench_module "vs. Big_int" =
  (module struct
    let elt_self i = pow (of_int_exn 1_000_000_000) (of_int_exn (Int.succ i))
    let elt_other i = Big_int.power_int_positive_int 1_000_000_000 (Int.succ i)

    let count = 4

    let array_self = Array.init count ~f:elt_self
    let array_other = Array.init count ~f:elt_other

    let%bench "plus_self" =
      for i = 0 to Int.pred count do
        for j = 0 to Int.pred count do
          ignore (array_self.(i) + array_self.(j) : t);
        done;
      done

    let%bench "plus_other" =
      for i = 0 to Int.pred count do
        for j = 0 to Int.pred count do
          ignore (Big_int.add_big_int array_other.(i) array_other.(j) : Big_int.big_int);
        done;
      done

    let%bench "mult_self" =
      for i = 0 to Int.pred count do
        for j = 0 to Int.pred count do
          ignore (array_self.(i) * array_self.(j) : t);
        done;
      done

    let%bench "mult_other" =
      for i = 0 to Int.pred count do
        for j = 0 to Int.pred count do
          ignore (Big_int.mult_big_int array_other.(i) array_other.(j) : Big_int.big_int);
        done;
      done

  end)
