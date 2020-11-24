let count_sql = ref 0
let acc_time = ref 0.0

let reset_stats () = 
  let () = count_sql := 0 in
  let () = acc_time := 0.0 in
  ()

let stats_to_string () = 
  let avg = !acc_time /. (float_of_int !count_sql) in
  let s = Printf.sprintf "%d sql requests in %f seconds (average : one request in %f seconds)" !count_sql !acc_time avg in
  s

let time f _ (* msg *) = 
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  let time = stop -. start in
  let () = incr count_sql in
  let () = acc_time := !acc_time +. time in
  (* let () = print_endline (Printf.sprintf "%s : \n  Query sent (%f seconds)" msg time) in *)
  result
