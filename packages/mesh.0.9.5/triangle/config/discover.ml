module C = Configurator.V1

let configure t =
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
  C.Flags.write_sexp "c_flags.sexp" ccopt;
  C.Flags.write_sexp "c_library_flags.sexp" cclib

let () =
  C.main ~name:"discover" configure
