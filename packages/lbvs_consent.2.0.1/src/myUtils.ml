(* not using Batteries !!! Dont! I want fast IOs.
   Or we have to prefix many things with Legacy.XXX *)

(* #require "batteries";; *)
(* #require "dolog";; *)
(* #require "parmap";; *)
(* #require "camlzip";; *)
(* #require "bitv";; *)

module L = BatList
module Log = Log.Make(struct let section = "Utls" end)

type filename = string

let tap f x =
  f x;
  x

let fst3 (a, _, _) = a

let create_tmp_filename () =
  let res = Filename.temp_file "" (* no_prefix *) "" (* no_suffix *) in
  (* tap (Log.info "create_tmp_filename: %s") res; *)
  res

let mkfifo (fn: filename): unit =
  Unix.mkfifo fn 0o600

(* abort if condition is not met *)
let enforce (condition: bool) (err_msg: string): unit =
  if not condition then
    failwith err_msg

let with_out_file (fn: filename) (f: out_channel -> 'a): 'a =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

let with_temp_out_file (f: filename -> 'a): 'a =
  let temp_file = Filename.temp_file "" (* no_prefix *) "" (* no_suffix *) in
  let res = f temp_file in
  Sys.remove temp_file;
  res

let with_in_file (fn: filename) (f: in_channel -> 'a): 'a =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let with_in_out_file (in_fn: filename) (out_fn: filename) (f: in_channel -> out_channel -> 'a): 'a =
  let input = open_in_bin in_fn in
  let output = open_out_bin out_fn in
  let res = f input output in
  close_in input;
  close_out output;
  res

let with_gzip_in_file (fn: string) (f: Gzip.in_channel -> 'a): 'a =
  let input = Gzip.open_in fn in
  let res = f input in
  Gzip.close_in input;
  res

let with_gzip_out_file
    (level: int) (fn: string) (f: Gzip.out_channel -> 'a): 'a =
  let output = Gzip.open_out ~level fn in
  let res = f output in
  Gzip.close_out output;
  res

let gzip_output_string (chan: Gzip.out_channel) (s: string): unit =
  Gzip.output_substring chan s 0 (String.length s)

(* to factorize code using parmap *)
let list_parmap (ncores: int) (f: 'a -> 'b) (l: 'a list): 'b list =
  if ncores > 1 then (* don't invoke parmap in vain *)
    Parmap.parmap ~ncores ~chunksize:1 f (Parmap.L l)
  else
    L.map f l

let list_pariter (ncores: int) (f: 'a -> unit) (l: 'a list): unit =
  if ncores > 1 then
    (* don't invoke parmap in vain *)
    Parmap.pariter ~ncores ~chunksize:1 f (Parmap.L l)
  else
    L.iter f l

let lines_of_file (fn: filename): string list =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> input_line input) in
      assert(exn = End_of_file);
      res
    )

(* call f on lines of file *)
let iter_on_lines_of_file fn f =
  let input = open_in_bin fn in
  try
    while true do
      f (input_line input)
    done
  with End_of_file -> close_in input

let map_on_file (fn: filename) (f: in_channel -> 'a): 'a list =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> f input) in
      assert(exn = End_of_file);
      res
    )

(* map f on lines of file *)
let map_on_lines_of_file (fn: filename) (f: string -> 'a): 'a list =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> f (input_line input)) in
      assert(exn = End_of_file);
      res
    )

(* skip 'nb' blocks from file being read *)
let skip_blocks nb read_one input =
  if nb = 0 then ()
  else
    let () = assert(nb > 0) in
    for _ = 1 to nb do
      ignore(read_one input)
    done

(* read file in parallel and call f on each block returned by read_one *)
let parmap_on_file
    (ncores: int)
    (fn: filename)
    (f: 'a -> 'b)
    (read_one: in_channel -> 'a): 'b list =
  (* We do interleaved reads, we might go faster with block reads;
     like 10k molecules at a time *)
  if BatString.ends_with fn ".bin" && ncores > 1 then
    (Log.fatal "parmap_on_file: cannot read in parallel from a .bin: %s"
       fn; exit 1)
  else
    (if ncores < 1 then
       (Log.fatal "parmap_on_file: (ncores = %d) < 1" ncores; exit 1)
     else if ncores = 1 then
       map_on_file fn (fun input -> f (read_one input))
     else (* ncores > 1 *)
       let fake_lines = BatList.make ncores "" in
       let values =
         list_parmap ncores
           (fun _fake_line ->
              let input = open_in_bin fn in
              let my_rank = Parmap.get_rank () in
              assert(my_rank >= 0);
              let res = ref [] in
              try
                (* start to read at correct position in file *)
                skip_blocks my_rank read_one input;
                while true do
                  (* process my block *)
                  let y = f (read_one input) in
                  res := y :: !res;
                  (* advance to next block for me *)
                  skip_blocks (ncores - 1) read_one input
                done;
                assert(false); (* for typing *)
              with End_of_file -> (close_in input; !res)
           ) fake_lines in
       L.concat values)

let read_lines = lines_of_file

let write_lines (lines: string list) (output_fn: filename): unit =
  with_out_file output_fn (fun out ->
      List.iter (Printf.fprintf out "%s\n") lines
    )

(* keep only lines that satisfy p *)
let filter_lines_of_file fn p =
  L.filter p (lines_of_file fn)

(* get the first line (stripped) output by given command *)
let get_command_output (cmd: string): string =
  Log.info "get_command_output: %s" cmd;
  let _stat, output = BatUnix.run_and_read cmd in
  match BatString.split_on_char '\n' output with
  | first_line :: _others -> first_line
  | [] -> (Log.error "get_command_output: no output for: %s" cmd; exit 1)

(* run the given command in a sub process (in parallel to the current process)
   and returns its pid so that we can wait for it later *)
let fork_out_cmd (cmd: string): int =
  Log.info "fork_out_cmd: %s" cmd;
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
  Log.info "run_command: %s" cmd;
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
  not (Filename.is_relative fn)

let relative_to_absolute fn =
  if Filename.is_relative fn then
    let cwd = Sys.getcwd () in
    Filename.concat cwd fn
  else
    fn

(* remove the prefix if it is there, or do nothing if it is not *)
let remove_string_prefix prfx str =
  if BatString.starts_with str prfx then
    let prfx_len = String.length prfx in
    BatString.tail str prfx_len
  else
    str

let string_contains_non_binary_digit = Str.regexp "[^01]"

let string_contains_only_zeros_or_ones (s: string): bool =
  not (Str.string_match string_contains_non_binary_digit s 0)

let string_contains_non_digits_non_sep = Str.regexp "[^-0123456789;]"

let string_is_a_list_of_integers (s: string): bool =
  BatString.starts_with s "[" &&
  BatString.ends_with s "]" &&
  let chopped = BatString.chop ~l:1 ~r:1 s in
  not (Str.string_match string_contains_non_digits_non_sep chopped 0)

let may_apply f = function
  | Some x -> f x
  | None -> ()

(* returns true if we could create the file; false else (already there) *)
let lock_file_for_writing (fn: filename): bool =
  try
    let fd = Unix.(openfile fn [O_CREAT; O_EXCL; O_WRONLY] 0o600) in
    Unix.close fd;
    true
  with Unix.Unix_error _ -> false

(* compute all distinct pairs ((a,b) = (b,a)) of elements in 'l' *)
let all_pairs (l: 'a list): ('a * 'a) list =
  let pair x ys =
    L.map (fun y ->
        (x, y)
      ) ys
  in
  let rec loop acc = function
    | [] -> acc
    | x :: xs ->
      loop (L.rev_append (pair x xs) acc) xs
  in
  loop [] l

(* accumulate the result of calling 'f' 'n' times *)
let n_times n f =
  let res = Array.init n (fun _i -> f ()) in
  Array.to_list res

(* turn bitv into float array *)
let bitv_to_floats (bv: Bitv.t): float array =
  let len = Bitv.length bv in
  let res = Array.make len 0.0 in
  Bitv.iteri (fun i bit ->
      if bit then Array.set res i 1.0
    ) bv;
  res

(* round to nearest integer away from 0.0 *)
let round (x: float): float =
  snd (modf (if x >= 0.0 then x +. 0.5 else x -. 0.5))

let push (x: 'a) (l: 'a list ref): unit =
  l := x :: !l

(* the identity function *)
let id x = x
