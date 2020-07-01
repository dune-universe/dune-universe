
open Printf

module A = Array
module L = BatList
module Ht = Hashtbl
module Log = Dolog.Log
module S = BatString

let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let with_out_file fn f =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

(* population standard deviation *)
let stddev (l: float list) =
  let n, sx, sx2 =
    List.fold_left (fun (n, sx, sx2) x ->
        (n +. 1., sx +. x, sx2 +. (x *.x))
      ) (0., 0., 0.) l
  in
  sqrt ((sx2 -. (sx *. sx) /. n) /. n)
(* stddev [2.; 4.; 4.; 4.; 5.; 5.; 7.; 9.] = 2.0 *)

let lines_of_file fn =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> input_line input) in
      if exn <> End_of_file then
        raise exn
      else res
    )

let lines_to_file fn lines =
  with_out_file fn (fun out ->
      L.iter (fprintf out "%s\n") lines
    )

let filter_lines_of_file fn p =
  L.filter p (lines_of_file fn)

(* call f on lines of file *)
let iter_on_lines_of_file fn f =
  let input = open_in_bin fn in
  try
    while true do
      f (input_line input)
    done
  with End_of_file -> close_in input

let iteri_on_lines_of_file fn f =
  let input = open_in_bin fn in
  let count = ref 0 in
  try
    while true do
      f !count (input_line input);
      incr count
    done
  with End_of_file -> close_in input

(* get the first line (stripped) output by given command *)
let get_command_output (verbose: bool) (cmd: string): string =
  if verbose then Log.info "get_command_output: %s" cmd;
  let _stat, output = BatUnix.run_and_read cmd in
  match BatString.split_on_char '\n' output with
  | first_line :: _others -> first_line
  | [] -> (Log.fatal "Utls.get_command_output: no output for: %s" cmd; exit 1)

let run_command verbose cmd =
  if verbose then Log.info "cmd: %s" cmd;
  ignore(Sys.command cmd)

let fold_on_lines_of_file fn f acc =
  with_in_file fn (fun input ->
      let acc' = ref acc in
      try
        while true do
          acc' := f !acc' (input_line input)
        done;
        assert(false)
      with End_of_file -> !acc'
    )

let float_list_of_file fn =
  let res =
    fold_on_lines_of_file fn (fun acc line ->
        let pred =
          try Scanf.sscanf line "%f" (fun x -> x)
          with Scanf.Scan_failure msg ->
            (* percolate a NaN rather than crashing *)
            (Log.error "%s: %s" msg line;
             nan) in
        pred :: acc
      ) [] in
  L.rev res

let float_list_to_file fn l =
  with_out_file fn (fun out ->
      L.iter (fprintf out "%f\n") l
    )

(* measure time spent in f (seconds) *)
let wall_clock_time f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  let delta_t = stop -. start in
  (delta_t, res)

let train_test_split p lines =
  assert(p >= 0.0 && p <= 1.0);
  let n = float (L.length lines) in
  let for_training = BatFloat.round_to_int (p *. n) in
  let train, test = L.takedrop for_training lines in
  assert(L.length train = for_training);
  (train, test)

(* abort if condition is not met *)
let enforce (condition: bool) (err_msg: string): unit =
  if not condition then
    failwith err_msg

(* split a list into n parts (the last part might have
   a different number of elements) *)
let list_nparts n l =
  let len = L.length l in
  let res = ref [] in
  let curr = ref l in
  let m = int_of_float (BatFloat.ceil (float len /. float n)) in
  for _ = 1 to n - 1 do
    let xs, ys = L.takedrop m !curr in
    curr := ys;
    res := xs :: !res
  done;
  L.rev (!curr :: !res)

(* create folds of cross validation; each fold consists in (train, test) *)
let cv_folds n l =
  let test_sets = list_nparts n l in
  let rec loop acc prev curr =
    match curr with
    | [] -> acc
    | x :: xs ->
      let before_after = L.flatten (L.rev_append prev xs) in
      let prev' = x :: prev in
      let train_test = (before_after, x) in
      let acc' = train_test :: acc in
      loop acc' prev' xs in
  loop [] [] test_sets

(* like the head command *)
let unix_head n fn =
  let res = ref [] in
  with_in_file fn (fun input ->
      for _i = 1 to n do
        res := (input_line input) :: !res
      done
    );
  L.rev !res

let first_line fn =
  with_in_file fn (fun input ->
      input_line input
    )

let string_list_to_file fn l =
  with_out_file fn (fun out ->
      L.iter (fprintf out "%s\n") l
    )

let count_lines (fn: string): int =
  let count = ref 0 in
  iter_on_lines_of_file fn (fun _line ->
      incr count
    );
  !count

(* create a 2D float array from reading a csv file  *)
let matrix_of_csv_file fn =
  let nb_lines = count_lines fn in
  let nb_cols =
    let csv_header = first_line fn in
    let nb_separators = S.count_char csv_header ' ' in
    1 + nb_separators in
  Log.info "%s: (cols, lines): (%d, %d)" fn nb_cols nb_lines;
  let m = A.make_matrix nb_cols nb_lines 0.0 in
  iteri_on_lines_of_file fn (fun y line ->
      let nb_seps = S.count_char line ' ' in
      assert(nb_seps = nb_cols - 1);
      let col_strs = S.split_on_char ' ' line in
      L.iteri (fun x col_str ->
          (* counted FPs are very sparse and the matrix was 0-initialized *)
          if col_str <> "0" then
            try m.(x).(y) <- Scanf.sscanf col_str "%f" (fun x -> x)
            with exn ->
              (Log.fatal "Utls.matrix_of_csv_file: cannot parse %s in %s"
                 col_str line;
               raise exn)
        ) col_strs
    );
  m

let matrix_to_csv_file (fn: string) (m: float array array)
    (drop_cols: (int, unit) Ht.t): unit =
  with_out_file fn (fun out ->
      let dimx = A.length m in
      let dimy = A.length m.(0) in
      for y = 0 to dimy - 1 do
        for x = 0 to dimx - 1 do
          if not (Ht.mem drop_cols x) then
            let z = m.(x).(y) in
            if y = 0 then
              (* the 1st line should be converted back to integers;
                 this is the csv header with column names *)
              fprintf out (if x = 0 then "%d" else " %d") (int_of_float z)
            else
              (* %g: skip trailing zeroes *)
              fprintf out (if x = 0 then "%g" else " %g") z
        done;
        fprintf out "\n"
      done
    )

let list_really_take n l =
  let took = L.take n l in
  assert(L.length took = n);
  took

let exponential_scan n =
  let rec loop acc x =
    if x > n then List.rev acc
    else loop (x :: acc) (2 * x) in
  loop [] 1

let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let trd3 (_, _, c) = c

let favg = function
  | [] -> 0.0 (* protect against empty list *)
  | xs -> L.favg xs

let string_of_list ?pre:(pre = "[") ?sep:(sep = ";") ?suf:(suf = "]")
    to_str l =
  let buff = Buffer.create 80 in
  Buffer.add_string buff pre;
  L.iteri (fun i x ->
      if i > 0 then Buffer.add_string buff sep;
      Buffer.add_string buff (to_str x)
    ) l;
  Buffer.add_string buff suf;
  Buffer.contents buff
