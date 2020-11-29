(* Command-line processing for a filter program using Getopt *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

let usage =
  Printf.sprintf
    "Usage: %s [OPTIONS] [INPUT]\n\
    Options:\n\
    \  -o FILE  --output=FILE  output to file (default is standard output)\n\
    \  -v       --version      print version number\n\
    \  -h       --help         print this message"
    Sys.argv.(0)

let output  = ref ""
let version = ref false
let help    = ref false

let specs =
[
 ('o', "output",  None, Getopt.atmost_once output
                          (Getopt.Error "only one output"));
 ('v', "version", Getopt.set version true, None);
 ('h', "help", Getopt.set help true, None);
]

let cmdline v specs =
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
      print_endline v;
      exit 0
    end
  else
    List.rev !args

let start version go =
  let (in_name, in_ch) =
    match cmdline version specs with
    | [] -> ("", stdin)
    | [f] -> (try (f, open_in f)
              with Sys_error s -> failwith s)
    | _ ->
	prerr_endline "Bad command-line argument count\n";
	prerr_endline usage;
	exit 1 in
  go in_name in_ch !output

let filter version go =
  try
    start version go
  with
  | Failure s ->
      prerr_endline s;
      exit 1
