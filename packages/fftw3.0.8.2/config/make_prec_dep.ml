(* Generate precision dependent files.  Avoid external dependencies
   such as "sed". *)

open Printf

let debug = false

let string_of_file fname =
  let buf = Buffer.create 8192 in
  let fh = open_in fname in
  Buffer.add_channel buf fh (in_channel_length fh);
  close_in fh;
  Buffer.contents buf

let transform fin fout tr =
  let s = string_of_file fin in
  let replace s (re, sub) =
    Str.global_replace (Str.regexp re) sub s in
  let s = List.fold_left replace s tr in
  (* Output *)
  let fh = open_out fout in
  fprintf fh "(* AUTOMATICALLY GENERATED from %S. *)\n" fin;
  fprintf fh "#1 %S\n" fin;
  output_string fh s;
  close_out fh

let ocaml_major, ocaml_minor =
  Scanf.sscanf Sys.ocaml_version "%d.%d" (fun ma mi -> (ma, mi))

let () =
  let add_noalloc l =
    if ocaml_major > 4 || (ocaml_major = 4 && ocaml_minor >= 2) then l
    else ("\\[@@noalloc\\]", "\"noalloc\"") :: l in
  transform "fftw3SD.ml" "fftw3D.ml"
    (add_noalloc
       ["floatXX_elt", "Bigarray.float64_elt";
        "floatXX", "Bigarray.float64";
        "complexXX_elt", "Bigarray.complex64_elt";
        "complexXX", "Bigarray.complex64";
        "\\$FFTW", "Fftw3.D"]);
  transform "fftw3SD.ml" "fftw3S.ml"
    (add_noalloc
       ["fftw_ocaml", "fftwf_ocaml"; (* C stubs *)
        "floatXX_elt", "Bigarray.float32_elt";
        "floatXX", "Bigarray.float32";
        "complexXX_elt", "Bigarray.complex32_elt";
        "complexXX", "Bigarray.complex32";
        "\\$FFTW", "Fftw3.S"])

let () =
  let c_fortran = "C_FORTRAN{\\([^,}]*\\), *\\([^}]*\\)}" in
  let debug = "DEBUG{\\([^}]+\\)};", if debug then "\\1;" else "" in
  transform "fftw3_geomCF.ml" "fftw3_geomC.ml"
            ["\\$LAYOUT", "c_layout";
             "FIRST_INDEX", "0";
             "LAST_INDEX(\\([a-z0-9]+\\))", "\\1 - 1";
             c_fortran, "\\1";
             "\\$LT", "<";
             "\\$GE", ">=";
             debug];
  transform "fftw3_geomCF.ml" "fftw3_geomF.ml"
            ["\\$LAYOUT", "fortran_layout";
             "FIRST_INDEX", "1";
             "LAST_INDEX(\\([a-z0-9]+\\))", "\\1";
             c_fortran, "\\2";
             "\\$LT", "<=";
             "\\$GE", ">";
             debug]
