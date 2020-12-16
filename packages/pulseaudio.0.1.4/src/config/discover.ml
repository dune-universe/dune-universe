module C = Configurator.V1

let () =
  C.main ~name:"pulseaudio-pkg-config" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = ["-lpulseaudio"; "-lpulse-simple"]; cflags = [] }
      in
      let conf =
        match C.Pkg_config.get c with
          | None -> default
          | Some pc -> (
              match
                C.Pkg_config.query_expr_err pc ~package:"libpulse libpulse-simple"
                  ~expr:"libpulse libpulse-simple"
              with
                | Error msg -> failwith msg
                | Ok deps -> deps )
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
