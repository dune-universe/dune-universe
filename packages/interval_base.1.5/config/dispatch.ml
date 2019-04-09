(* Dispatch the files and perform the substitutions to share files
   among the various sub-libraries. *)
open Printf

let read_all fn =
  let b = Buffer.create 4096 in
  let fh = open_in fn in
  Buffer.add_channel b fh (in_channel_length fh);
  close_in fh;
  Buffer.contents b

let crlibm () =
  let source = "../src-intel/generic.ml" in (* from root *)
  let s = read_all source in
  let lu_re = Str.regexp "Fpu\\.\\(Low\\|High\\)" in
  let s = Str.global_replace lu_re "Crlibm.\\1" s in
  (* Assume [mod_float x 2.] is exact — it is easy enough to implement... *)
  let s = Str.global_replace (Str.regexp "Fpu\\.fmod") "mod_float" s in
  let fh = open_out "generic.ml" in
  fprintf fh "# 1 %S\n" ("src-crlibm/" ^ source);
  output_string fh s;
  close_out fh

let () =
  crlibm()
