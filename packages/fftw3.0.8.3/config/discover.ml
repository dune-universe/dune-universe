module C = Configurator.V1

(* Keep the order of "type r2r_kind" in fftw3SD.ml in sync with
   fftw3.h values. *)
let check_r2r_kind ?c_flags c =
  let v = C.C_define.(import c ?c_flags ~includes:["fftw3.h"]
                        ["FFTW_R2HC", Type.Int;
                         "FFTW_HC2R", Type.Int;
                         "FFTW_DHT", Type.Int;
                         "FFTW_REDFT00", Type.Int;
                         "FFTW_REDFT01", Type.Int;
                         "FFTW_REDFT10", Type.Int;
                         "FFTW_REDFT11", Type.Int;
                         "FFTW_RODFT00", Type.Int;
                         "FFTW_RODFT01", Type.Int;
                         "FFTW_RODFT10", Type.Int;
                         "FFTW_RODFT11", Type.Int;  ]) in
  let get name = match List.assoc name v with
    | C.C_define.Value.Int i -> i
    | exception _ -> C.die "Cannot find %S in the header file <fftw3.h>. \
                            Contact the authors." name
    | _ -> C.die "Macro %S does not have an integer value.  Contact the \
                  authors." name in
  get "FFTW_R2HC" = 0 && get "FFTW_HC2R" = 1 && get "FFTW_DHT" = 2
  && get "FFTW_REDFT00" = 3 && get "FFTW_REDFT01" = 4 && get "FFTW_REDFT10" = 5
  && get "FFTW_REDFT11" = 6 && get "FFTW_RODFT00" = 7 && get "FFTW_RODFT01" = 8
  && get "FFTW_RODFT10" = 9 && get "FFTW_RODFT11" = 10

let ocaml_version c =
  let v = C.ocaml_config_var_exn c "version" in
  let is_word_char c = '0' <= c && c <= '9' in
  match C.Flags.extract_words ~is_word_char v with
  | major :: minor :: _ -> (int_of_string major, int_of_string minor)
  | _ -> assert false

let get_cflags ?(default=[]) conf =
  match conf with Some c -> c.C.Pkg_config.cflags
                | None -> default

let get_libs ?(default=[]) conf =
  match conf with Some c -> c.C.Pkg_config.libs
                | None -> default

let discover c =
  let module P = C.Pkg_config in
  let fftw3 = match P.get c with
    | Some p -> P.query p ~package:"fftw3"
    | None -> None in
  let fftw3f = match P.get c with
    | Some p -> P.query p ~package:"fftw3f"
    | None -> None in
  let c_flags =
    match Sys.getenv "FFTW3_CFLAGS" with
    | exception Not_found -> get_cflags fftw3 @ get_cflags fftw3f
    | alt_cflags -> C.Flags.extract_blank_separated_words alt_cflags in
  let libs =
    match Sys.getenv "FFTW3_LIBS" with
    | exception Not_found -> get_libs fftw3 ~default:["-lfftw3"]
                             @ get_libs fftw3f ~default:["-lfftw3f"]
    | alt_libs -> "-lm" :: C.Flags.extract_blank_separated_words alt_libs in

  let major, minor = ocaml_version c in
  let c_flags =
    if major > 4 || (major = 4 && minor >= 6) then "-DOCAML_4_06" :: c_flags
    else c_flags in
  let c_flags =
    if major > 4 || (major = 4 && minor >= 8) then "-DOCAML_4_08" :: c_flags
    else c_flags in

  let c_flags = if fftw3f = None then c_flags
                else "-DFFTW3F_EXISTS" :: c_flags in

  if not(check_r2r_kind ~c_flags c) then
    C.die "The values of the fields FFTW_R2HC,... in fftw3.h have \
           changed.\n  Please use a newer version of the OCaml-fftw3 \
           library or report the issue.";

  C.Flags.write_sexp "c_flags.sexp" c_flags;
  C.Flags.write_sexp "c_library_flags.sexp" libs


let () =
  C.main ~name:"fftw3" discover
