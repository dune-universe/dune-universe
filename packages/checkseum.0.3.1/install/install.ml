#!/usr/bin/env ocaml

;;
#load "unix.cma"

let freestanding =
  "freestanding_linkopts = \"-l:libcheckseum_freestanding_stubs.a\""

let meta =
  match Sys.getenv_opt "DUNE_BUILD_DIR" with
  | Some _build -> _build ^ "/default/META.checkseum"
  | None -> "_build/default/META.checkseum"

let new_line = '\n'

let output_line oc line =
  output_string oc line ;
  output_char oc new_line

let () =
  Unix.chmod meta 0o644 ;
  let oc = open_out_gen [ Open_append ] 0o644 meta in
  output_line oc freestanding ;
  close_out oc
