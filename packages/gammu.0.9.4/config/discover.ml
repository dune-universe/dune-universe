open Base
open Stdio

(* FIXME: This may be to rough on Windows.  A more subtle splitting
   function should be added to Configurator. *)
let split_ws str = String.(split str ~on:' ' |> List.filter ~f:((<>) ""))

let configure t =
  let module P = Configurator.Pkg_config in
  let pkg = match P.get t with
    | Some pkg_config -> P.query pkg_config ~package:"gammu"
    | None -> None in
  let cflags = match Caml.Sys.getenv "OCAML_GAMMU_CFLAGS" with
    | alt_cflags -> split_ws alt_cflags
    | exception Not_found ->
       match pkg with Some p -> p.P.cflags
                    | None -> ["-I/usr/include/gammu"] in
  let libs = match Caml.Sys.getenv "OCAML_GAMMU_LIBS" with
    | alt_libs -> split_ws alt_libs
    | exception Not_found ->
       match pkg with Some p -> p.P.libs
                    | None -> ["-lGammu"; "-lm"]  in

  (* Check for debug environment variable *)
  let debug = try ignore(Caml.Sys.getenv "OCAML_GAMMU_DEBUG"); true
              with _ -> false in
  let cflags =
    if debug then
      (if Caml.Sys.win32 then "/DCAML_GAMMU_DEBUG"
       else "-DCAML_GAMMU_DEBUG") :: cflags
    else cflags in
  let write_sexp file sexp =
    Out_channel.write_all file ~data:(Sexp.to_string sexp) in
  write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string cflags);
  write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string libs)

let () =
  Configurator.main ~name:"discover" configure
