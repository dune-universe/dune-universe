#!/bin/env ocaml

;;
#load "str.cma"

;;
#load "unix.cma"

let max_level = ref 9
let ddate = ref true
let dtime = ref true
let ddecimal = ref false

let parse s =
  let re =
    Str.regexp
      "\\([0-9]+\\.[0-9]+\\) \\[\\([a-zA-Z._-]*\\):\\([0-9]+\\)\\] \\(.*\\)$"
  in
  if Str.string_match re s 0 then (
    let time = float_of_string (Str.matched_group 1 s) in
    let label = Str.matched_group 2 s in
    let level = int_of_string (Str.matched_group 3 s) in
    let str = Str.matched_group 4 s in
    Some (time, label, level, str) )
  else None

let rec disp file =
  let rec h f =
    try
      let l = input_line f in
      begin
        match parse l with
        | Some (time, label, level, str) ->
            let tm = Unix.localtime time in
            if level <= !max_level then begin
              if !ddate then
                Printf.printf "%04d-%02d-%02d " (tm.Unix.tm_year + 1900)
                  (tm.Unix.tm_mon + 1) tm.Unix.tm_mday;
              if !dtime then begin
                Printf.printf "%02d:%02d:%02d " tm.Unix.tm_hour tm.Unix.tm_min
                  tm.Unix.tm_sec;
                if !ddecimal then Printf.printf "%f " (time -. floor time)
              end;
              Printf.printf "[%20s:%d]\n>>> %s\n" label level str
            end
        | None -> Printf.printf "-?-\n"
      end;
      h f
    with End_of_file -> ()
  in
  let f = open_in file in
  h f;
  close_in f

let parse_args =
  Arg.parse
    [
      ("-l", Arg.String (fun l -> max_level := int_of_string l), "level");
      ("-nd", Arg.Unit (fun () -> ddate := false), "no date");
      ("-nt", Arg.Unit (fun () -> dtime := false), "no time");
      ("-d", Arg.Unit (fun () -> ddecimal := true), "decimals");
    ]
    (fun s -> disp s)
    (Printf.sprintf "usage : %s [options]" (Filename.basename Sys.argv.(0)))
