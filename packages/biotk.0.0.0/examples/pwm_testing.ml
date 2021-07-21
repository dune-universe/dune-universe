open Core
open Gzt

let f () =
  let bg = Pwm.random_background () in
  let pwm = Pwm.random 8 bg in
  let s = Pwm_stats.random_sequence 1_000_000 bg in
  let theoretical_threshold =
    Pwm_stats.TFM_pvalue.score_of_pvalue pwm bg 1e-3 in
  let occ = Pwm.fast_scan pwm s (theoretical_threshold /. 2.) in
  List.count occ ~f:(fun (_, s) -> s > theoretical_threshold)
