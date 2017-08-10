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
  let doc = "Sbx container version, one of : 1(bs=512 bytes, default) 2(bs=128 bytes) 3(bs=4096 bytes) where bs=sbx block size" in
  Arg.(value & opt (some string) None & info ["sbx-version"] ~docv:"SBX-VERSION" ~doc)
;;

let silent =
  let doc = "One of : 0(only show progress stats when done) 1(show nothing). This only affects progress text printing." in
  let open Progress_report in
  Arg.(value & opt (some (enum [("0", L0); ("1", L1)])) None & info ["s"; "silent"] ~docv:"LEVEL" ~doc)
;;

let force =
  let doc = "Force overwrites even if $(docv) exists" in
  Arg.(value & flag & info ["f"; "force"] ~docv:"OUT" ~doc)
;;

let hash =
  let open Multihash in
  let doc =
    "Hash function to use, one of (case-insensitive) : sha1 sha256(default) sha512 blake2b-512" in
  Arg.(value & opt (some string) None & info ["hash"] ~docv:"HASH-TYPE" ~doc)
;;

let default_cmd =
  let doc = "a SeqBox implementation written in OCaml" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = help_secs in
  (Term.(ret (const (`Help (`Pager, None)))),
   Term.info "osbx" ~version:Osbx_version.version ~doc ~sdocs ~exits ~man
  )
;;

let encode_cmd =
  let open Osbx_encode in
  let doc = "Encode file" in
  (Term.(const encode $ silent $ force $ no_meta $ sbx_version $ uid $ hash $ in_file $ out_file),
   Term.info "encode" ~doc
  )
;;

let decode_cmd =
  let open Osbx_decode in
  let doc = "Decode sbx container" in
  (Term.(const decode $ silent $ force $ show_max $ in_file $ out_file),
   Term.info "decode" ~doc
  )
;;

let rescue_cmd =
  let open Osbx_rescue in
  let doc = "Rescue sbx data from file" in
  (Term.(const rescue $ silent $ in_file $ out_dir $ log_file),
   Term.info "rescue" ~doc
  )
;;

let show_cmd =
  let open Osbx_show in
  let doc = "Search for and print metadata in sbx container (or file)" in
  (Term.(const show $ silent $ find_max $ skip_to_byte $ in_file),
   Term.info "show" ~doc
  )
;;

let () =
  (* catch Ctrl-C breaks *)
  Sys.catch_break true;
  Term.exit @@ Term.eval_choice default_cmd [encode_cmd; decode_cmd; rescue_cmd; show_cmd]
;;
