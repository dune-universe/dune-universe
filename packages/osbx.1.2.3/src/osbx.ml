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
  let doc = "Sbx container version, one of : 1(bs=512 bytes) 2(bs=128 bytes) 3(bs=4096 bytes) where bs=sbx block size" in
  let open Sbx_specs in
  Arg.(value
       & opt
         (enum [("1", (`V1:version));
                ("2", (`V2:version));
                ("3", (`V3:version))])
         (`V1:version)
       & info ["sbx-version"]
         ~docv:"SBX-VERSION"
         ~doc)
;;

let silent =
  let doc = "One of : 0(show everything), 1(only show progress stats when done) 2(show nothing).
  This only affects progress text printing." in
  let open Progress_report in
  Arg.(value
       & opt
         (enum [("0", L0);
                ("1", L1);
                ("2", L2)])
         L0
       & info ["s"; "silent"]
         ~docv:"LEVEL"
         ~doc)
;;

let force =
  let doc = "Force overwrite even if $(docv) exists" in
  Arg.(value & flag & info ["f"; "force"] ~docv:"OUT" ~doc)
;;

let force_misalign =
  let doc = "Disable automatic rounding down of $(docv).
  This is not normally used and is only intended for data recovery or related purposes." in
  Arg.(value & flag & info ["force-misalign"] ~docv:"FROM-BYTE" ~doc)
;;

let hash =
  let open Multihash in
  let doc =
    "Hash function to use, one of (case-insensitive) : sha1 sha256(default) sha512 blake2b-512" in
  Arg.(value & opt (some string) None & info ["hash"] ~docv:"HASH-TYPE" ~doc)
;;

let from_byte =
  let doc = Printf.sprintf "Start from byte $(docv). The position is automatically rounded down to the closest multiple of %d bytes.
  If not specified, defaults to the start of file.
  Negative values are treated as 0.
  If $(docv) exceeds the largest possible position(file size - 1), then it will be treated as (file size - 1).
  The rounding procedure is applied after all auto-adjustments."
      Param.Common.block_scan_alignment in
  Arg.(value & opt (some int64) None & info ["from"; "skip-to"] ~docv:"FROM-BYTE" ~doc)
;;

let to_byte =
  let doc = "Last position to try to decode a block.
  If not specified, defaults to the end of file.
  Negative values are treated as 0.
  If $(docv) is smaller than FROM-BYTE, then it will be treated as FROM-BYTE." in
  Arg.(value & opt (some int64) None & info ["to"] ~docv:"TO-BYTE" ~doc)
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
  (Term.(const decode $ silent $ force $ no_meta $ show_max $ in_file $ out_file),
   Term.info "decode" ~doc
  )
;;

let rescue_cmd =
  let open Osbx_rescue in
  let doc = "Rescue sbx data from file" in
  (Term.(const rescue $ silent $ only_pick $ from_byte $ to_byte $ force_misalign $ in_file $ out_dir $ log_file),
   Term.info "rescue" ~doc
  )
;;

let show_cmd =
  let open Osbx_show in
  let doc = "Search for and print metadata in sbx container (or file)" in
  (Term.(const show $ silent $ find_max $ from_byte $ to_byte $ force_misalign $ in_file),
   Term.info "show" ~doc
  )
;;

let () =
  (* catch Ctrl-C breaks *)
  Sys.catch_break true;
  Term.exit @@ Term.eval_choice default_cmd [encode_cmd; decode_cmd; rescue_cmd; show_cmd]
;;
