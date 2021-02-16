module C = Configurator.V1

let mk_default_conf ?cflags ?libs libname =
  let open C.Pkg_config in
  ( libname,
    {
      libs = (match libs with None -> [ "-l" ^ libname ] | Some l -> l);
      cflags = (match cflags with None -> [] | Some l -> l);
    } )

let gen_conf h =
  let open C.Pkg_config in
  Hashtbl.fold
    (fun _k cf conf ->
      { libs = cf.libs @ conf.libs; cflags = cf.cflags @ conf.cflags })
    h { libs = []; cflags = [] }

let () =
  C.main ~name:"foo" (fun c ->
      let libraries =
        let h = Hashtbl.create 7 in
        List.iter
          (fun (libname, conf) -> Hashtbl.add h libname conf)
          [ mk_default_conf "freetype2"; mk_default_conf "cairo" ];
        h
      in

      let conf =
        ( match C.Pkg_config.get c with
        | None -> ()
        | Some pc ->
            Hashtbl.iter
              (fun package _ ->
                match C.Pkg_config.query pc ~package with
                | None -> ()
                | Some conf -> Hashtbl.replace libraries package conf)
              libraries );
        gen_conf libraries
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
