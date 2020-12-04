module C = Configurator.V1

exception Found of C.Pkg_config.package_conf

let pkg_config_names = ["soundtouch"; "libSoundTouch"]

let () =
  C.main ~name:"soundtouch-pkg-config" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = ["-lSoundTouch"]; cflags = [] }
      in
      let conf =
        match C.Pkg_config.get c with
          | None -> default
          | Some pc -> (
              try
                List.iter
                  (fun package ->
                    match C.Pkg_config.query pc ~package with
                      | Some deps -> raise (Found deps)
                      | None -> ())
                  pkg_config_names;
                default
              with Found deps -> deps )
      in
      C.Flags.write_sexp "c_flags.sexp" ("-fPIC" :: conf.cflags);
      C.Flags.write_sexp "c_library_flags.sexp" ("-lstdc++" :: conf.libs))
