module From = Migrate_parsetree.Convert (Migrate_parsetree.OCaml_407)
    (Migrate_parsetree.OCaml_current)

let dump_cmi filename =
  let infos = Cmi_format.read_cmi filename in
  let sig_ = Parsetree_of_types.signature infos.cmi_sign in
  Format.printf "%a@." Pprintast.signature (From.copy_signature sig_)

let dump filename =
  match Filename.extension filename with
  | ".cmi" -> dump_cmi filename
  | ext -> invalid_arg ("Unknown extension: " ^ ext)

let main files =
  files |> List.iter dump

let files =
  let doc = "Files to print" in
  Cmdliner.Arg.(
    value & pos_all non_dir_file [] &
    info [] ~docv:"FILE" ~doc)

let options = Cmdliner.Term.(
    const main $ files)

let info =
  let doc = "Dump OCaml files" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info "ocamldump" ~doc ~exits:Cmdliner.Term.default_exits ~man

let () =
  Cmdliner.Term.eval (options, info) |>
  Cmdliner.Term.exit
