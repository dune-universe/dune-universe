open Cmdliner

let help_secs = [ `S Manpage.s_common_options
                ; `P "These options are common to all comamnds."
                ; `S "MORE HELP"
                ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
                ; `S Manpage.s_authors
                ; `P "Darren Ldl <darrenldldev@gmail.com>"
                ; `S Manpage.s_bugs
                ; `P "Report bugs at ocaml-SeqBox GitHub page via issues (https://github.com/darrenldl/ocaml-SeqBox)"
                ]
;;

let sbx_version =
  let doc = "Sbx container version" in
  Arg.(value & opt (some string) None & info ["sbx-version"] ~docv:"SBX_VERSION" ~doc)
;;

let force =
  let doc = "Force overwrites even if $(docv) exists" in
  Arg.(value & flag & info ["f"; "force"] ~docv:"OUTFILE" ~doc)
;;

let hash =
  let open Multihash in
  let doc =
    "Hash function to use, one of (case-insensitive) : sha1 sha256(default) sha512 blake2b-512" in
  Arg.(value & opt (some string) None & info ["hash"] ~docv:"HASH_TYPE" ~doc)
;;

let default_cmd =
  let doc = "a SeqBox implementation written in OCaml" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = help_secs in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "osbx" ~version:Osbx_version.version ~doc ~sdocs ~exits ~man
;;

let encode_cmd =
  let open Osbx_encode in
  let doc = "Encode file" in
  (Term.(const encode $ force $ no_meta $ sbx_version $ uid $ hash $ in_file $ out_file),
   Term.info "encode" ~doc
  )
;;

let decode_cmd =
  let open Osbx_decode in
  let doc = "Decode sbx container" in
  (Term.(const decode $ force $ in_file $ out_file),
   Term.info "decode" ~doc
  )
;;

let rescue_cmd =
  let open Osbx_rescue in
  let doc = "Rescue sbx data from file" in
  (Term.(const rescue $ in_file $ out_dir $ log_file),
   Term.info "rescue" ~doc
  )
;;

let show_cmd =
  let open Osbx_show in
  let doc = "Search for and print metadata in sbx container (or file)" in
  (Term.(const show $ find_all $ in_file),
   Term.info "show" ~doc
  )
;;

let () =
  (* catch Ctrl-C breaks *)
  Sys.catch_break true;
  Term.exit @@ Term.eval_choice default_cmd [encode_cmd; decode_cmd; rescue_cmd; show_cmd]
;;
