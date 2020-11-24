
let standard_deviation l = 
  let calc_sum acc e = acc +. e in
  let sum = List.fold_left calc_sum 0. l in
  let mean = sum /. (float_of_int (List.length l)) in
  let calc_sub acc e = (e -. mean) :: acc in
  let ldev = List.fold_left calc_sub [] l in
  let calc_square acc e = (e *. e) :: acc in
  let ldev = List.fold_left calc_square [] ldev in
  let sum_ldev = List.fold_left calc_sum 0. ldev in
  let r = sum_ldev /. (float_of_int ((List.length l) - 1)) in
  sqrt r

let bench f n = 
  let total_time = ref 0.0 in
  let min = ref max_float in
  let max = ref 0.0 in
  let avg = ref 0.0 in
  let () = 
    for _ = 1 to n do
      let start = Unix.gettimeofday () in
      let () = f () in
      let stop = Unix.gettimeofday () in
      let time = stop -. start in
      let () = 
        if (time < !min) then min := time
      in
      let () = 
        if (time > !max) then max := time
      in
      total_time := !total_time +. time
    done
  in
  let () = 
    avg := !total_time /. (float_of_int n)
  in
  (!total_time, !min, !max, !avg)

let () = 

  let time_mp = ref 0.0 in
  let min_mp = ref max_float in
  let max_mp = ref 0.0 in
  let avg_mp = ref 0.0 in

  let time_mc = ref 0.0 in
  let min_mc = ref max_float in
  let max_mc = ref 0.0 in
  let avg_mc = ref 0.0 in

  let nb_run = 5 in
  let nb_iter = 500 in

  let list_times_mp = ref [] in
  let list_times_mc = ref [] in

  let () = 
    for i = 1 to nb_run do
      let () = print_newline () in
      let () = print_endline (Printf.sprintf "Run %u (one run = %u iter):" i nb_iter) in

      let (time, min, max, avg) = bench Bench_mysql_protocol.run nb_iter in
      let () = if (time < !min_mp) then min_mp := time in
      let () = if (time > !max_mp) then max_mp := time in
      let () = time_mp := !time_mp +. time in
      let () = print_endline (Printf.sprintf "  MySQL Protocol  (sec.): Total=%f  Avg (total time / %u)=%f  Min (1 iter)=%f  Max (1 iter)=%f" time nb_iter avg min max) in
      let () = list_times_mp := time :: !list_times_mp in

      let (time, min, max, avg) = bench Bench_mysql_libc.run nb_iter in
      let () = if (time < !min_mc) then min_mc := time in
      let () = if (time > !max_mc) then max_mc := time in
      let () = time_mc := !time_mc +. time in
      let () = print_endline (Printf.sprintf "  MySQL C library (sec.): Total=%f  Avg (total time / %u)=%f  Min (1 iter)=%f  Max (1 iter)=%f" time nb_iter avg min max) in
      let () = list_times_mc := time :: !list_times_mc in

      let () = print_newline () in
      ()
    done
  in
  let () = avg_mp := !time_mp /. (float_of_int nb_run) in
  let () = avg_mc := !time_mc /. (float_of_int nb_run) in

  let sd_mp = standard_deviation !list_times_mp in
  let sd_mc = standard_deviation !list_times_mc in

  let () = print_newline () in
  let () = print_endline (Printf.sprintf "Results for %u runs of %u iter:" nb_run nb_iter) in
  let () = print_endline (Printf.sprintf "  MySQL Protocol  (seconds): Total=%f  Avg (total time / %u)=%f  Std deviation=%f  Min (1 run)=%f  Max (1 run)=%f" !time_mp nb_run !avg_mp sd_mp !min_mp !max_mp) in
  let () = print_endline (Printf.sprintf "  MySQL C library (seconds): Total=%f  Avg (total time / %u)=%f  Std deviation=%f  Min (1 run)=%f  Max (1 run)=%f" !time_mc nb_run !avg_mc sd_mc !min_mc !max_mc) in
  ()
