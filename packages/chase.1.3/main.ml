(* Command line processing and reading of formulas *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

let default_bound = 250
let default_limit = 2000

(* Options read from the command line *)
type options = {
    mutable bound : int;
    mutable limit : int;
    terse : bool;
    just_one : bool;
    compact : bool;
    sexpr : bool;
    margin : int;
    quant: bool;
    explicit: bool;
    flat : bool;
  }

(* Update option from the herald in the input *)
let herald opts bnd lmt =
  (match bnd with
   | None -> ()
   | Some i ->
      opts.bound <- i);
  match lmt with
   | None -> ()
   | Some i ->
      opts.limit <- i

(* Actions after processing the command line *)

let go run opts f i ofile =
  let bnd, lmt, xs =            (* Read formulas *)
    if opts.quant then
      Quant_reader.read_file f i
    else
      Reader.read_file f i in
  close_in i;
  herald opts bnd lmt;          (* Process herald *)
  Arity.arity xs;               (* Ensure valid signature *)
  List.iter Check.check xs;     (* Check formulas *)
  let o =                       (* Open output *)
    if ofile = "" then
      stdout
    else
      open_out ofile in
  run opts o xs                 (* Run the chase *)

(* Command-line processing using Getopt *)

let usage =
  Printf.sprintf
    "Usage: %s [OPTIONS] [INPUT]\n\
    Options:\n\
    \  -o FILE  --output=FILE  output to file (default is standard output)\n\
    \  -t       --terse        use terse output -- print only models\n\
    \  -j       --just-one     find just one model\n\
    \  -b INT   --bound=INT    set structure size bound (default %d)\n\
    \  -l INT   --limit=INT    set step count limit (default %d)\n\
    \  -c       --compact      print structures compactly\n\
    \  -s       --sexpr        print structures using S-expressions\n\
    \  -m INT   --margin=INT   set output margin\n\
    \  -q       --quant        read formulas using quantifier syntax\n\
    \  -e       --explicit     print formulas using quantifier syntax\n\
    \  -f       --flatten      print flattened formulas\n\
    \  -p       --proc-time    print processor time in seconds\n\
    \  -v       --version      print version number\n\
    \  -h       --help         print this message"
    Sys.argv.(0) default_bound default_limit

let output    = ref ""
let terse     = ref false
let just_one  = ref false
let bound     = ref default_bound
let limit     = ref default_limit
let compact   = ref false
let sexpr     = ref false
let margin    = ref 0
let quant     = ref false
let explicit  = ref false
let flat      = ref false
let proc_time = ref false
let version   = ref false
let help      = ref false

let getint option place string =
  match int_of_string_opt string with
  | Some i -> place := i
  | None -> failwith ("Bad value for option " ^ option)

let specs =
[
 ('o', "output",  None, Getopt.atmost_once output
                          (Getopt.Error "only one output"));
 ('t', "terse", Getopt.set terse true, None);
 ('j', "just-one", Getopt.set just_one true, None);
 ('b', "bound", None, Some (getint "bound" bound));
 ('l', "limit", None, Some (getint "limit" limit));
 ('c', "compact", Getopt.set compact true, None);
 ('s', "sexpr", Getopt.set sexpr true, None);
 ('m', "margin", None, Some (getint "margin" margin));
 ('q', "quant", Getopt.set quant true, None);
 ('e', "explicit", Getopt.set explicit true, None);
 ('f', "flatten", Getopt.set flat true, None);
 ('p', "proc-time", Getopt.set proc_time true, None);
 ('v', "version", Getopt.set version true, None);
 ('h', "help", Getopt.set help true, None);
]

let cmdline specs =
  let args = ref [] in
  let add_arg arg = args := arg :: !args in
  (try
     Getopt.parse_cmdline specs add_arg
   with
   | Getopt.Error s ->
      prerr_endline s;
      prerr_endline "";
      prerr_endline usage;
      exit 1);
  if !help then
    begin
      print_endline usage;
      exit 0
    end
  else if !version then
    begin
      print_endline Version.version;
      exit 0
    end
  else
    List.rev !args

let start run =
  let (in_name, in_ch) =
    match cmdline specs with
    | [] -> ("", stdin)
    | [f] -> (try (f, open_in f)
              with Sys_error s -> failwith s)
    | _ ->
	prerr_endline "Bad command-line argument count\n";
	prerr_endline usage;
	exit 1 in
  let opts = {
      terse = !terse;
      just_one = !just_one;
      bound = !bound;
      limit = !limit;
      compact = !compact;
      sexpr = !sexpr;
      margin = !margin;
      quant = !quant;
      explicit = !explicit;
      flat = !flat; } in
  go run opts in_name in_ch !output

let show_time () =
  if !proc_time then
    Printf.eprintf "Runtime %.1f\n" @@ Sys.time ()

let main run =
  try
    start run;
    show_time ()
  with
  | Failure s ->
     prerr_endline s;
     show_time ();
     exit 1
