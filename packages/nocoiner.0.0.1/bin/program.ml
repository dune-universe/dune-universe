type files = {commitment: string; opening: string}

let docs = Cmdliner.Manpage.s_common_options

let commit_file =
  let open Cmdliner in
  let doc = "The commitment file to read-from / write-to." in
  let default_arg = Arg.(opt string "nocoiner-commit.box") in
  let aliases_arg = ["cf"; "commitment-file"] in
  let docv = "COMMITMENT-FILE" in
  Arg.(value & default_arg & info aliases_arg ~docv ~doc ~docs)

let open_file =
  let open Cmdliner in
  let doc = "The opening file to read-from / write-to." in
  let default_arg = Arg.(opt string "nocoiner-open.key") in
  let aliases_arg = ["of"; "opening-file"] in
  let docv = "OPENING-FILE" in
  Arg.(value & default_arg & info aliases_arg ~docv ~doc ~docs)

let force_overwrite =
  let open Cmdliner in
  let doc = "Flag to force overwrite of files during commitment." in
  let aliases_arg = ["f"; "force"] in
  Arg.(value & flag & info aliases_arg ~doc ~docs)

let files commitment opening = {commitment; opening}

let files_term =
  let open Cmdliner in
  Term.(const files $ commit_file $ open_file)

module R = Nocoiner.Reasons
module H = Helpers

let commit_cmd files force =
  try
    let result = Commands.commit ~force files.commitment files.opening in
    `Ok result
  with
  | Sys_error reason ->
      `Error (false, H.system_error reason)
  | H.CantOverwrite message ->
      `Error (true, message)

let reveal_cmd files =
  let cfile = files.commitment in
  let ofile = files.opening in
  try
    let result = Commands.reveal cfile ofile in
    `Ok result
  with
  | Sys_error reason ->
      `Error (false, H.system_error reason)
  | R.InvalidOpening ->
      `Error (false, H.invalid_open ofile)
  | R.InvalidCommitment ->
      `Error (false, H.invalid_commit cfile)
  | R.BindingFailure ->
      `Error (false, H.invalid_pairs cfile ofile)

let commit_term =
  let open Cmdliner in
  let term = Term.(const commit_cmd $ files_term $ force_overwrite) in
  Term.ret term

let reveal_term =
  let open Cmdliner in
  let term = Term.(const reveal_cmd $ files_term) in
  Term.ret term

let issues_url = "https://github.com/marcoonroad/nocoiner/issues"

let help_secs =
  let open Cmdliner in
  [ `S Manpage.s_common_options
  ; `P "These options are available here for this command."
  ; `S Manpage.s_bugs
  ; `P ("Check & open bug reports at " ^ issues_url ^ ".") ]

let commit_term_info =
  let open Cmdliner in
  let man =
    [ `S Manpage.s_description
    ; `P
        "The secret to commit a pair is read from the process' STDIN. This \
         command outputs nothing on STDOUT. In case of errors, logs are sent \
         on STDERR."
    ; `Blocks help_secs ]
  in
  let doc = "Creates the commitment & opening pairs from a secret input" in
  ( commit_term
  , Term.info "commit" ~doc ~exits:Term.default_exits ~sdocs:docs ~man )

let reveal_term_info =
  let open Cmdliner in
  let man =
    [ `S Manpage.s_description
    ; `P
        "The secret, if successfully revealed, is shown on process' STDOUT. \
         In any case of errors, logs are written on STDERR."
    ; `Blocks help_secs ]
  in
  let doc =
    "Attempts to reveal the secret given the commitment & opening pairs"
  in
  ( reveal_term
  , Term.info "reveal" ~doc ~exits:Term.default_exits ~sdocs:docs ~man )

let help_term =
  let open Cmdliner in
  let cmd _ = `Help (`Auto, None) in
  let term = Term.(const cmd $ files_term) in
  Term.ret term

let default_term_info =
  let open Cmdliner in
  let doc = "A command-line interface for the nocoiner library." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P
        "nocoiner is a library for Commitment Schemes using Authenticated \
         Encryption."
    ; `Blocks help_secs ]
  in
  (help_term, Term.info "nocoiner" ~doc ~exits ~sdocs:docs ~man)

let () =
  let open Cmdliner in
  let term_info_cmds = [commit_term_info; reveal_term_info] in
  let result = Term.(eval_choice default_term_info term_info_cmds) in
  Term.exit result
