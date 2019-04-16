module C = Configurator.V1

let () =
  (* Extend the pkg-config path rather than overwriting it.
     See #25 *)
  let prepend = try Unix.getenv "OPAM_PKG_CONFIG_PATH" ^ ":" with _ -> "" in
  let onto = try Unix.getenv "PKG_CONFIG_PATH" with _ -> "" in
  let combined = prepend ^ onto in
  if not(String.equal combined "") then Unix.putenv "PKG_CONFIG_PATH" combined;

  C.main ~name:"mirage-xen" (fun c ->
    match C.Pkg_config.get c with
    | None -> failwith "pkg-config not found"
    | Some pc ->
       let flags_for package =
         match C.Pkg_config.query pc ~package with
         | None -> failwith (Printf.sprintf "pkg-config(%s) not found" package)
         | Some v -> v.cflags in
       let base_cflags = "-Wno-attributes" :: "-O2" :: "-fPIC" :: (flags_for "mirage-xen-ocaml") in
       let minios_cflags = base_cflags @ (flags_for "mirage-xen-minios") in
       C.Flags.write_lines "cflags" base_cflags;
       C.Flags.write_lines "minios-cflags" minios_cflags
  )
