open Core
open Core_bench
module Seq = Sequence
open Combinat

let rec combinations_naive k list =
  if k <= 0 then [[]]
  else
    match list with
    | [] -> []
    | h :: tl ->
        let with_h =
          List.map ~f:(fun l -> h :: l) (combinations_naive (k - 1) tl)
        in
        let without_h = combinations_naive k tl in
        with_h @ without_h

let () =
  Command.group ~summary:"combinat benchmarks"
    [ ( "combinations"
      , Bench.make_command
          [ Bench.Test.create ~name:"combinations_naive_small" (fun () ->
                combinations_naive 6 (List.init 10 ~f:(fun i -> i)) )
          ; Bench.Test.create ~name:"combinations_internal_small" (fun () ->
                Combination.iter ~f:(fun _ -> ()) (6, 10) )
          ; Bench.Test.create ~name:"combinations_naive_med" (fun () ->
                combinations_naive 5 (List.init 25 ~f:(fun i -> i)) )
          ; Bench.Test.create ~name:"combinations_internal_med" (fun () ->
                Combination.iter ~f:(fun _ -> ()) (5, 25) )
          ; Bench.Test.create ~name:"combinations_internal_ba_large" (fun () ->
                Combination.iter ~f:(fun _ -> ()) (11, 33) ) ] )
    ; ( "permutations"
      , Bench.make_command
          [ Bench.Test.create ~name:"permutations_internal_med" (fun () ->
                Permutation.iter
                  ~f:(fun _ -> ())
                  (Array.init 8 ~f:(fun i -> i)) ) ] )
    ; ( "partitions"
      , Bench.make_command
          [ Bench.Test.create ~name:"partitions_internal_med" (fun () ->
                Combinat.Partition.iter (65, 20) ~f:(fun _ -> ()) ) ] ) ]
  |> Command.run
