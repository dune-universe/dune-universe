module C = Configurator.V1

let () =
  C.main ~name:"foo" (fun c ->
    let default : C.Pkg_config.package_conf =
      { libs   = ["-lmpg123"];
        cflags = ["-I/usr/include"] }
    in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
        match C.Pkg_config.query pc ~package:"libmpg123" with
        | None -> default
        | Some deps -> deps
    in
    C.Flags.write_sexp "c_flags.sexp"         conf.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs;
    let oc = open_out "c_flags.txt" in
    List.iter (Printf.fprintf oc "%s ") conf.cflags;
    close_out oc)
;;
