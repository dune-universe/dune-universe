(* Copyright (C) 2021, Francois Berenger

   Tsuda laboratory, Tokyo university,
   5-1-5 Kashiwa-no-ha, Kashiwa-shi, Chiba-ken, 277-8561, Japan. *)

open Printf

module A = BatArray
module Fn = Filename
module Ht = BatHashtbl
module IS = BatSet.Int
module IntMap = BatMap.Int
module IntSet = BatSet.Int
module L = BatList
module LO = Line_oriented
module Log = Dolog.Log

(* sparse vector of integer features *)
type features = int IntMap.t

type filename = string

let tap f x =
  f x;
  x

let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let trd3 (_, _, c) = c

let mkfifo (fn: filename): unit =
  Unix.mkfifo fn 0o600

(* abort if condition is not met *)
let enforce (condition: bool) (err_msg: string): unit =
  if not condition then
    failwith err_msg

let enforce_f (condition: bool) (msg_constr: unit -> string): unit =
  if not condition then
    failwith (msg_constr ())

let temp_filename prfx sufx =
  Fn.temp_file ~temp_dir:"/tmp" prfx sufx

let with_temp_out_file (f: filename -> 'a): 'a =
  let temp_file = temp_filename "" (* no_prefix *) "" (* no_suffix *) in
  let res = f temp_file in
  Sys.remove temp_file;
  res

let with_in_file2 fn1 fn2 f =
  let in1 = open_in_bin fn1 in
  let in2 = open_in_bin fn2 in
  let res = f in1 in2 in
  close_in in1;
  close_in in2;
  res

let with_in_file3 fn1 fn2 fn3 f =
  let in1 = open_in_bin fn1 in
  let in2 = open_in_bin fn2 in
  let in3 = open_in_bin fn3 in
  let res = f in1 in2 in3 in
  close_in in1;
  close_in in2;
  close_in in3;
  res

(* read 'nlines' from 'input' in_channel *)
let read_n_lines nlines input =
  assert(nlines >= 0);
  let rec loop n acc =
    if n = 0 then L.rev acc
    else loop (n - 1) (input_line input :: acc) in
  loop nlines []

(* if the first line is a comment (starts with '#'); then we extract
   it separately from other lines; else we treat it as any other line *)
let maybe_extract_comment_header (fn: filename): string option * string list =
  let all_lines = LO.lines_of_file fn in
  match all_lines with
  | [] -> (None, [])
  | fst :: others ->
    if BatString.starts_with fst "#" then
      (Some fst, others)
    else
      (None, all_lines)

(* skip 'nb' blocks from file being read *)
let skip_blocks nb read_one input =
  if nb = 0 then ()
  else
    let () = assert(nb > 0) in
    for _ = 1 to nb do
      ignore(read_one input)
    done

(* get the first line (stripped) output by given command *)
let get_command_output (cmd: string): string =
  Log.info "get_command_output: %s" cmd;
  let _stat, output = BatUnix.run_and_read cmd in
  match BatString.split_on_char '\n' output with
  | first_line :: _others -> first_line
  | [] -> (Log.fatal "get_command_output: no output for: %s" cmd; exit 1)

let temp_dir template_str =
  let cmd = sprintf "mktemp -d /tmp/%s" template_str in
  get_command_output cmd

(* run the given command in a sub process (in parallel to the current process)
   and returns its pid so that we can wait for it later *)
let fork_out_cmd (cmd: string): int =
  Log.debug "fork_out_cmd: %s" cmd;
  match Unix.fork () with
  | 0 -> (* child process *) exit (Sys.command cmd)
  | -1 -> (* error *) (Log.fatal "fork_out_cmd: fork failed"; exit 1)
  | pid -> pid

(* return full path of command, if found in PATH, none else *)
let command_exists (cmd: string): string option =
  let where_is_cmd = "which " ^ cmd in
  if Unix.system (where_is_cmd ^ " 2>&1 > /dev/null") = Unix.WEXITED 0 then
    Some (get_command_output where_is_cmd)
  else
    None

let run_command (cmd: string): unit =
  Log.debug "run_command: %s" cmd;
  match Unix.system cmd with
  | Unix.WSIGNALED _ -> (Log.fatal "run_command: signaled: %s" cmd; exit 1)
  | Unix.WSTOPPED _ -> (Log.fatal "run_command: stopped: %s" cmd; exit 1)
  | Unix.WEXITED i when i <> 0 ->
    (Log.fatal "run_command: exit %d: %s" i cmd; exit 1)
  | Unix.WEXITED _ (* i = 0 then *) -> ()

let get_env (env_var: string): string option =
  try Some (Sys.getenv env_var)
  with Not_found -> None

(* look for exe in PATH then given env. var *)
let find_command (exe: string) (env_var: string): string option =
  match command_exists exe with
  | Some cmd -> Some cmd
  | None ->
    match get_env env_var with
    | Some cmd -> Some cmd
    | None -> (Log.warn "%s not found in PATH; \
                         put it in your PATH or setup the \
                         %s env. var. to point to it" exe env_var;
               None)

let filename_is_absolute fn =
  not (Fn.is_relative fn)

let relative_to_absolute fn =
  if Fn.is_relative fn then
    let cwd = Sys.getcwd () in
    Fn.concat cwd fn
  else
    fn

(* remove the prefix if it is there, or do nothing if it is not *)
let remove_string_prefix prfx str =
  if BatString.starts_with str prfx then
    let prfx_len = String.length prfx in
    BatString.tail str prfx_len
  else
    str

let may_apply_opt f = function
  | Some x -> Some (f x)
  | None -> None

let may_apply f = function
  | Some x -> f x
  | None -> ()

exception Enough_times

(* accumulate the result of calling 'f' 'n' times *)
let n_times n f =
  let i = ref 0 in
  BatList.unfold_exc (fun () ->
      if !i = n then raise Enough_times
      else
        let res = f () in
        incr i;
        res
    )

(* the identity function *)
let id x = x

let string_of_floats_array fv =
  let buff = Buffer.create 80 in
  A.iter (fun f ->
      Buffer.add_string buff (sprintf "%f " f)
    ) fv;
  Buffer.contents buff

(* enforce filename uses one of the allowed extensions *)
let enforce_file_extension allowed_exts fn =
  assert(L.exists (BatString.ends_with fn) allowed_exts)

(* Pi math constant *)
let m_pi = 4.0 *. atan 1.0

let prepend x xs =
  xs := x :: !xs

(* test (lb <= x <= hb) *)
let in_bounds lb x hb =
  x >= lb && x <= hb

let array_medianf (xs: float array): float =
  A.sort BatFloat.compare xs;
  let n = A.length xs in
  if n mod 2 = 1 then
    A.unsafe_get xs (n / 2)
  else
    let before = A.unsafe_get xs ((n / 2) - 1) in
    let after = A.unsafe_get xs (n / 2) in
    0.5 *. (before +. after)

let list_medianf (l: float list): float =
  let xs = A.of_list l in
  array_medianf xs
(*$T list_medianf
   list_medianf [1.;2.;3.;4.;5.] = 3.0
   list_medianf [1.;2.;3.;4.] = 2.5
*)

let string_of_array ?pre:(pre = "[|") ?sep:(sep = ";") ?suf:(suf = "|]")
    to_str a =
  let buff = Buffer.create 80 in
  Buffer.add_string buff pre;
  A.iteri (fun i x ->
      if i > 0 then Buffer.add_string buff sep;
      Buffer.add_string buff (to_str x)
    ) a;
  Buffer.add_string buff suf;
  Buffer.contents buff

let string_of_list ?pre:(pre = "[") ?sep:(sep = ";") ?suf:(suf = "]")
    to_str l =
  let buff = Buffer.create 1024 in
  Buffer.add_string buff pre;
  L.iteri (fun i x ->
      if i > 0 then Buffer.add_string buff sep;
      Buffer.add_string buff (to_str x)
    ) l;
  Buffer.add_string buff suf;
  Buffer.contents buff

(* marshal x to file *)
let save fn x =
  LO.with_out_file fn (fun out ->
      Marshal.to_channel out x [Marshal.No_sharing]
    )

(* unmarshal x from file *)
let restore fn =
  LO.with_in_file fn (fun input ->
      Marshal.from_channel input
    )

let is_odd i =
  (i mod 2) = 1

let is_even i =
  (i mod 2) = 0

let get_first_line fn =
  LO.with_in_file fn input_line

(* like the cut unix command *)
let cut d f line =
  let splitted = BatString.split_on_char d line in
  BatList.at splitted f

let get_ncores () =
  int_of_string (get_command_output "getconf _NPROCESSORS_ONLN")

let int_of_bool = function
  | true -> 1
  | false -> 0

let bool_of_int = function
  | 1 -> true
  | 0 -> false
  | _ -> assert(false)

(* test that 'y' is within 'x' +/- 'epsilon';
   i.e. y \in [x - epsilon, x + epsilon] *)
let approx_equal epsilon x y =
  (y >= x -. epsilon) &&
  (y <= x +. epsilon)

(* proper NaN/nan testing *)
let is_nan x =
  match classify_float x with
  | FP_nan -> true
  | _ -> false

(* some statistics *)

let faverage =
  BatList.favg

let fmean = faverage

(* population standard deviation *)
let stddev (l: float list): float =
  let n, sx, sx2 =
    List.fold_left (fun (n, sx, sx2) x ->
        (n +. 1., sx +. x, sx2 +. (x *.x))
      ) (0., 0., 0.) l
  in
  sqrt ((sx2 -. (sx *. sx) /. n) /. n)
(* stddev [2.; 4.; 4.; 4.; 5.; 5.; 7.; 9.] = 2.0 *)

let fincr_by (xref: float ref) (dx: float): unit =
  xref := !xref +. dx

let make_pair x y =
  (x, y)

(* sort then count duplicates *)
let list_uniq_count l =
  let sorted = L.sort compare l in
  let groups = L.group_consecutive (=) sorted in
  L.map (fun l -> L.(hd l, length l)) groups

(* coarse grain chronometer *)
let time_it f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  (stop -. start, res)

let ceili (x: float): int =
  int_of_float (ceil x)

let list_rev_sort cmp l =
  List.sort (fun x y -> cmp y x) l

let list_really_take n l =
  let took = L.take n l in
  assert(L.length took = n);
  took

let array_rand_elt rng a =
  let n = A.length a in
  assert(n > 0);
  let i = BatRandom.State.int rng n in
  Array.unsafe_get a i

(* not very efficient but... *)
let list_rand_elt rng l =
  array_rand_elt rng (A.of_list l)

let list_really_remove_one l x =
  assert(L.mem x l); (* BatList.remove doesn't enforce this *)
  L.remove l x

let parse_SMILES_line line =
  try Scanf.sscanf line "%s@\t%s" (fun smiles name -> (smiles, name))
  with exn -> (Log.fatal "Utls.parse_SMILES_line: malformed line: '%s'" line;
               raise exn)

let string_contains_regexp reg s =
  try
    let (_: int) = Str.search_forward reg s 0 in
    true
  with Not_found -> false

(* get one bootstrap sample of size 'nb_samples' using
   sampling with replacement *)
let array_bootstrap_sample rng nb_samples a =
  let n = Array.length a in
  assert(nb_samples <= n);
  A.init nb_samples (fun _ ->
      let rand = Random.State.int rng n in
      A.unsafe_get a rand
    )

(* same as array_bootstrap_sample,
   but also return the set of Out Of Bag (unused) sample indexes *)
let array_bootstrap_sample_OOB rng nb_samples a =
  let n = Array.length a in
  let flags = A.create n '0' in
  let bootstrap =
    A.init nb_samples (fun _ ->
        let rand = Random.State.int rng n in
        A.unsafe_set flags rand '1';
        A.unsafe_get a rand
      ) in
  let oob =
    A.fold_righti (fun i x acc ->
        if x = '0' then i :: acc
        else acc
      ) flags [] in
  (bootstrap, A.of_list oob)

(* only return bootstrap and OOB element _indexes_ *)
let array_bootstrapi_sample_OOB rng nb_samples a =
  let n = Array.length a in
  let flags = A.create n '0' in
  let bootstrap =
    A.init nb_samples (fun _ ->
        let rand = Random.State.int rng n in
        A.unsafe_set flags rand '1';
        rand
      ) in
  let oob =
    A.fold_righti (fun i x acc ->
        if x = '0' then i :: acc
        else acc
      ) flags [] in
  (bootstrap, A.of_list oob)

let robust_float_of_string s =
  try Scanf.sscanf s "%f" (fun x -> x)
  with _exn ->
    failwith ("robust_float_of_string: cannot parse: " ^ s)

(* force x to be in [mini, maxi] *)
let bound_between mini maxi x =
  if x < mini then mini
  else if x > maxi then maxi
  else x

exception Not_singleton

let is_singleton s =
  try
    let must_false = ref false in
    IntSet.iter (fun _x ->
        if !must_false then raise Not_singleton;
        must_false := true
      ) s;
    !must_false (* handle empty set case *)
  with Not_singleton -> false

(* tests
   assert (not (is_singleton IntSet.empty));;
   assert (is_singleton (IntSet.singleton 1));;
   assert (not (is_singleton (IntSet.of_list [1;2])));;
*)

let square x =
  x *. x

let standard_deviation a =
  let n = float (A.length a) in
  let avg = A.favg a in
  let sum_squared_errors =
    A.fold (fun acc x ->
        acc +. square (x -. avg)
      ) 0.0 a in
  sqrt (sum_squared_errors /. n)

(* test
    standard_deviation [|2.;4.;4.;4.;5.;5.;7.;9.|] = 2.0
*)

let std_dev avg a =
  let n = float (A.length a) in
  let sum_squared_errors =
    A.fold (fun acc x ->
        acc +. square (x -. avg)
      ) 0.0 a in
  sqrt (sum_squared_errors /. n)

let ht_iteri f ht =
  let i = ref 0 in
  Ht.iter (fun x ->
      let j = !i in
      incr i;
      f j x
    ) ht

let array_iter3 a1 a2 a3 f =
  let n = A.length a1 in
  assert(n = A.length a2 && n = A.length a3);
  for i = 0 to n - 1 do
    let x = A.unsafe_get a1 i in
    let y = A.unsafe_get a2 i in
    let z = A.unsafe_get a3 i in
    f x y z
  done
