
open Printf

module L = BatList
module Log = Dolog.Log

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
