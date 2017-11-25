open Base
open Stdio

let check_fftw3 ?c_flags ?link_flags c =
  Configurator.c_test c ?c_flags ?link_flags
    "#include <fftw3.h>
     int main() {
       const int n = 10;
       fftw_complex in[10], out[10];
       fftw_plan_dft_1d(n, in, out, FFTW_FORWARD, FFTW_ESTIMATE);
       return 0;
     }"

(* Test single precision *)
let check_fftw3l ?c_flags ?link_flags c =
  Configurator.c_test c ?c_flags ?link_flags
    "#include <fftw3.h>
     int main() {
       const int n = 10;
       fftwf_complex in[10], out[10];
       fftwf_plan_dft_1d(n, in, out, FFTW_FORWARD, FFTW_ESTIMATE);
       return(0);
     }"

(* Keep the order of "type r2r_kind" in fftw3SD.ml in sync with
   fftw3.h values. *)
let check_r2r_kind ?c_flags ?link_flags c =
  Configurator.c_test c ?c_flags ?link_flags
    "#include <fftw3.h>
     int main() {
       if(FFTW_R2HC==0 && FFTW_HC2R==1 && FFTW_DHT==2 &&
          FFTW_REDFT00==3 && FFTW_REDFT01==4 && FFTW_REDFT10==5 &&
          FFTW_REDFT11==6 && FFTW_RODFT00==7 && FFTW_RODFT01==8 &&
          FFTW_RODFT10==9 && FFTW_RODFT11==10)
         return(0);
       else
         return(1);
     }"

let ocaml_version c =
  let v = Configurator.ocaml_config_var_exn c "version" in
  match String.split v ~on:'.' with
  | major :: minor :: _ -> (Caml.int_of_string major, Caml.int_of_string minor)
  | _ -> assert false

let split_ws str = String.(split str ~on:' ' |> List.filter ~f:((<>) ""))

let get_cflags ?(default=[]) conf =
  match conf with Some c -> c.Configurator.Pkg_config.cflags
                | None -> default

let get_libs ?(default=[]) conf =
  match conf with Some c -> c.Configurator.Pkg_config.libs
                | None -> default

let discover c =
  let module C = Configurator in
  let module P = C.Pkg_config in
  let fftw3 = Option.bind (P.get c) ~f:(fun p -> P.query p ~package:"fftw3") in
  let fftw3f = Option.bind (P.get c)
                 ~f:(fun p -> P.query p ~package:"fftw3f") in
  let c_flags =
    match Caml.Sys.getenv "FFTW3_CFLAGS" with
    | exception Not_found -> get_cflags fftw3 @ get_cflags fftw3f
    | alt_cflags -> split_ws alt_cflags in
  let libs =
    match Caml.Sys.getenv "FFTW3_LIBS" with
    | exception Not_found -> get_libs fftw3 ~default:["-lfftw3"]
                             @ get_libs fftw3f ~default:["-lfftw3f"]
    | alt_libs -> "-lm" :: split_ws alt_libs in

  if not(check_fftw3 c ~c_flags ~link_flags:libs) then
    C.die "Please install FFTW3 and/or set environment variables \
           FFTW3_CFLAGS and FFTW3_LIBS.";

  if not(check_fftw3l c ~c_flags ~link_flags:libs) then
    printf "warning: FFTW3 single precision not installed.  Functions of the \
            corresponding OCaml module will raise exceptions.\n";

  let major, minor = ocaml_version c in
  let c_flags =
    if major > 4 || (major = 4 &&  minor >= 6) then "-DOCAML_4_06" :: c_flags
    else c_flags in

  if not(check_r2r_kind ~c_flags c) then
    C.die "The values of the fields FFTW_R2HC,... in fftw3.h have \
           changed.  Please use a newer version of the OCaml-fftw3 \
           library or report the issue.";

  let write_sexp file sexp =
    Out_channel.write_all file ~data:(Sexp.to_string sexp) in
  write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string c_flags);
  write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string libs)


let () =
  Configurator.main ~name:"fftw3" discover
