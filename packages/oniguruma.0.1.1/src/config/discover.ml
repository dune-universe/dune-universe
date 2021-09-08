module C = Configurator.V1

let () =
  C.main ~name:"discover" begin fun c ->
    let default = C.Pkg_config.{ libs = ["-lonig"]; cflags = [] } in
    let conf = match C.Pkg_config.get c with
      | None -> default
      | Some p -> match C.Pkg_config.query ~package:"oniguruma" p with
        | None -> default
        | Some conf -> conf
    in
    C.Flags.write_sexp "cflags.sexp" conf.cflags;
    C.Flags.write_sexp "libs.sexp" conf.libs
  end
