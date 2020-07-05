module C = Configurator.V1

let is_system op s =
  let re = Str.regexp_string_case_fold op in
  try
    ignore (Str.search_forward re s 0 : int);
    true
  with Not_found -> false

let () =
  let sys = ref "" in
  let args = [ ("-system", Arg.Set_string sys, "set system") ] in
  C.main ~args ~name:"gmp" @@ fun c ->
  let sys = !sys in
  let libs, cflags =
    if is_system "freebsd" sys || is_system "openbsd" sys then
      ([ "-L/usr/local/lib"; "-lgmp" ], [ "-I/usr/local/include" ])
    else if is_system "macosx" sys then
      ( [ "-L/opt/local/lib"; "-L/usr/local/lib"; "-lgmp" ],
        [ "-I/opt/local/include"; "-I/usr/local/include" ] )
    else ([ "-lgmp" ], [])
  in
  let default = { C.Pkg_config.libs; cflags } in
  let conf =
    match C.Pkg_config.get c with
    | None -> default
    | Some pc -> (
      match C.Pkg_config.query pc ~package:"gmp" with
      | None -> default
      | Some deps -> deps )
  in

  C.Flags.write_sexp "c_flags.sexp" conf.cflags;

  C.Flags.write_sexp "c_library_flags.sexp" conf.libs;

  C.Flags.write_lines "c_flags.lines" conf.cflags
