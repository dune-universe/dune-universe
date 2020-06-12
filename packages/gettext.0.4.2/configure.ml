let () =
  let version = ref "dev" in
  let default_localedir = ref "/usr/share/locale" in
  let localedir = ref "/usr/local/share/locale" in
  Arg.parse
    (Arg.align
       [
         ( "--with-defaultlocaledir",
           Arg.String (fun s -> default_localedir := s),
           " Location of the default locale dir." );
         ( "--with-localedir",
           Arg.String (fun s -> localedir := s),
           " Additional location of the locale dir." );
         ( "--version",
           Arg.String (fun s -> version := s),
           " Current version of gettext." );
       ])
    (fun s ->
      raise (Arg.Bad (Printf.sprintf "don't know what to do with %S" s)))
    "Usage: ocaml configure.ml [OPTIONS]";
  let oc = open_out_bin "src/lib/gettext/base/gettextConfigGen.ml" in
  Printf.fprintf oc
    "let default_localedir = %S\nlet localedir = %S\nlet version = %S\n"
    !default_localedir !localedir !version;
  close_out oc
