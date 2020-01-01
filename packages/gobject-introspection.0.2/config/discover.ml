open Base
open Stdio
module C = Configurator

let write_sexp fn list_of_str =
  let data = sexp_of_list sexp_of_string list_of_str |> Sexp.to_string in
  Out_channel.write_all fn ~data

let write_flags file list_of_str =
  let data = String.concat list_of_str ~sep:" " in
  Out_channel.write_all file ~data

let () =
  C.main ~name:"GObject-Introspection" (fun c ->
    let default : C.Pkg_config.package_conf =
      { libs   = ["-lgirepository-1.0"; "-lgobject-2.0"; "-lglib-2.0"; "-lffi"]
      ; cflags = ["-O2"; "-Wall"; "-Wextra"; "-Wno-unused-parameter"; "-pthread";
                  "-I/usr/include/gobject-introspection-1.0";
                  "-I/usr/lib/libffi-3.2.1/include";
                  "-I/usr/include/glib-2.0";
                  "-I/usr/lib/glib-2.0/include"]
      }
    in
    let default_ffi : C.Pkg_config.package_conf =
      { libs   = ["-lffi"] ;
        cflags = ["-O2"; "-Wall"; "-Wextra"; "-Wno-unused-parameter";
                  "-I/usr/lib/libffi-3.2.1/include";
                  "-I/usr/include/x86_64-linux-gnu"; (* default ubuntu *)
                  "-I/usr/include"] (* default ubuntu *)
      }
    in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
         let get_config package default =
           Option.value (C.Pkg_config.query pc ~package) ~default in
         let libffi = get_config "libffi" default_ffi in
         let gobject = get_config "gobject-introspection-1.0" default in
         let  module P = C.Pkg_config in
         { libs = (libffi.P.libs @ gobject.P.libs);
           cflags = (libffi.P.cflags @ gobject.P.cflags) }
    in
    let os_type = C.ocaml_config_var_exn (C.create "") "system" in
    let ccopts =
      if Base.String.(os_type = "macosx") then [""]
      else ["-Wl,-no-as-needed"]
    in
    write_sexp "c_flags.sexp"         conf.cflags;
    write_sexp "c_library_flags.sexp" conf.libs;
    write_sexp "ccopts.sexp"          ccopts;
    write_flags "c_library_flags"     conf.libs;
    write_flags "c_flags"             conf.cflags)
