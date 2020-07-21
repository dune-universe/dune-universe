
module L = BatList

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

(* get the first line output by given command *)
let get_command_output ?(debug = false) (cmd: string): string =
  if debug then
    Printf.printf "get_command_output: %s" cmd;
  let _stat, output = BatUnix.run_and_read cmd in
  match BatString.split_on_char '\n' output with
  | first_line :: _others -> first_line
  | [] ->
    begin
      Printf.eprintf "get_command_output: no output for: %s" cmd;
      exit 1
    end

(* filename to string list *)
let lines_of_file fn =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> input_line input) in
      if exn <> End_of_file then
        raise exn
      else res
    )

(* use fraction [p] as training set and fraction 1-p as test set *)
let train_test_split p lines =
  assert(p >= 0.0 && p <= 1.0);
  let n = float (L.length lines) in
  let for_training = BatFloat.round_to_int (p *. n) in
  let train, test = L.takedrop for_training lines in
  assert(L.length train = for_training);
  (train, test)

(* shuffle then train_test_split *)
let shuffle_then_cut seed p = function
  | [] -> failwith "Utls.shuffle_then_cut: no lines"
  | lines ->
    let rng = BatRandom.State.make [|seed|] in
    let rand_lines = L.shuffle ~state:rng lines in
    train_test_split p rand_lines

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
  let rec loop acc prev = function
    | [] -> acc
    | x :: xs ->
      let before_after = L.flatten (L.rev_append prev xs) in
      let prev' = x :: prev in
      let train_test = (before_after, x) in
      let acc' = train_test :: acc in
      loop acc' prev' xs in
  loop [] [] test_sets

(* shuffle then [n] train-test folds *)
let shuffle_then_nfolds seed n = function
  | [] -> failwith "Utls.shuffle_then_nfolds: no lines"
  | lines ->
    let rng = BatRandom.State.make [|seed|] in
    let rand_lines = L.shuffle ~state:rng lines in
    cv_folds n rand_lines
