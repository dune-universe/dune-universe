open Base
open Stdio

let configure t =
  let module C = Configurator in
  let system_libtriangle =
    (* Test the presence of the header file. *)
    C.c_test t ~link_flags:["-ltriangle"]
      "#include <triangle.h>
       int main() {
         return 0;
       }" in
  let ccopt, cclib =
    if system_libtriangle then
      (* System with libtriangle *)
      ["-DTRILIBRARY"; "-DEXTERNAL_TEST"; "-DANSI_DECLARATORS";
       "-DLIBTRIANGLE"],
      ["-ltriangle"]
    else
      ["-DTRILIBRARY"; "-DEXTERNAL_TEST"; "-DANSI_DECLARATORS"], [] in
  let write_sexp file sexp =
    Out_channel.write_all file ~data:(Sexp.to_string sexp) in
  write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string ccopt);
  write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string cclib)

let () =
  Configurator.main ~name:"discover" configure
