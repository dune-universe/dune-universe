module C = Configurator.V1

let () =
  C.main ~name:"gstreamer-pkg-config" (fun c ->
      let default : C.Pkg_config.package_conf =
        {
          libs =
            [
              "-lgstapp-1.0 -lgstbase-1.0 -lgstreamer-1.0 -lgobject-2.0 \
               -lglib-2.0";
            ];
          cflags = [];
        }
      in
      let conf =
        match C.Pkg_config.get c with
          | None -> default
          | Some pc -> (
              match
                C.Pkg_config.query pc ~package:"gstreamer-1.0 gstreamer-app-1.0"
              with
                | None -> default
                | Some deps -> deps )
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
