module C = Configurator.V1

external is_big_endian : unit -> bool = "ocaml_mm_is_big_endian"

let () =
  C.main ~name:"flac-pkg-config" (fun c ->
      C.C_define.gen_header_file c ~fname:"flac_config.h"
        [("BIGENDIAN", Switch (is_big_endian ()))];

      let default : C.Pkg_config.package_conf =
        { libs = ["-lflac"]; cflags = [] }
      in
      let conf =
        match C.Pkg_config.get c with
          | None -> default
          | Some pc -> (
              match
                C.Pkg_config.query pc ~package:"flac"
              with
                | None -> default
                | Some deps -> deps)
      in
      C.Flags.write_sexp "flac_c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "flac_c_library_flags.sexp" conf.libs)
