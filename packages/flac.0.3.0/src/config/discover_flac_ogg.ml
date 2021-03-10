module C = Configurator.V1

let () =
  C.main ~name:"flac-ogg-pkg-config" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = ["-logg";"-lflac"]; cflags = [] }
      in
      let conf =
        match C.Pkg_config.get c with
          | None -> default
          | Some pc -> (
              match
                C.Pkg_config.query pc ~package:"ogg flac"
              with
                | None -> default
                | Some deps -> deps)
      in
      C.Flags.write_sexp "flac_ogg_c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "flac_ogg_c_library_flags.sexp" conf.libs)
