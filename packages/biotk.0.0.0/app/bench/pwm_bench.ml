open Core_bench
open Core
open Biotk

let scan pwm seq theta f =
  f pwm seq theta
  |> ignore

let random_nucleotide () =
  match Random.bool (), Random.bool () with
  | true, true -> 'A'
  | true, false -> 'C'
  | false, true -> 'G'
  | false, false -> 'T'

let random_sequence n = String.init n ~f:(fun _ -> random_nucleotide ())

let tests =
  let n = 100_000 in
  let seq = random_sequence n in
  let pwm = Pwm.random ~len:10 (Pwm.flat_background ()) in
  let theta = Pwm_stats.TFM_pvalue.score_of_pvalue pwm (Pwm.flat_background ()) 1e-4 in
  [
    Bench.Test.create ~name:"naive" (fun () -> scan pwm seq theta Pwm.scan) ;
    Bench.Test.create ~name:"look_ahead" (fun () -> scan pwm seq theta Pwm.opt_scan) ;
    Bench.Test.create ~name:"naive_c" (fun () -> scan pwm seq theta Pwm.fast_scan) ;
    Bench.Test.create ~name:"look_ahead_c" (fun () -> scan pwm seq theta Pwm.opt_fast_scan) ;
  ]

let () = Command.run (Bench.make_command tests)
