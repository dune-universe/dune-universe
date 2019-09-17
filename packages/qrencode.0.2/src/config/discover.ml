module C = Configurator.V1

let () =
  C.main ~name:"libqrencode" begin fun c ->
    let default : C.Pkg_config.package_conf = {
      libs   = ["-lqrencode -libpng16 -lz"];
      cflags = []
    } in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
        match C.Pkg_config.query pc ~package:"libqrencode libpng"  with
        | None -> default
        | Some v -> v in
    C.Flags.write_sexp "c_flags.sexp" conf.cflags ;
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs
  end
