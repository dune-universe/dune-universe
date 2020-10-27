module C = Configurator.V1

let pkg_config_combine ~default l =
  List.fold_left (fun conf x ->
    let x = Option.value ~default x in
    C.Pkg_config.{
      libs = conf.libs @ x.libs;
      cflags = conf.cflags @ x.cflags;
    }
  ) default l

let () =
  C.main ~name:"bemenu" (fun c ->
    let default =
      C.Pkg_config.{
        libs = [];
        cflags = [];
      }
    in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
          pkg_config_combine ~default [
            C.Pkg_config.query pc ~package:"wayland-client";
            C.Pkg_config.query pc ~package:"cairo";
            C.Pkg_config.query pc ~package:"pango";
            C.Pkg_config.query pc ~package:"pangocairo";
            C.Pkg_config.query pc ~package:"xkbcommon";
          ]
    in
    C.Flags.write_sexp "c_flags.sexp" conf.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs;
  )
