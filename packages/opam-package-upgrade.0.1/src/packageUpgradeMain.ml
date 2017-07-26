(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2017 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open Cmdliner

let upgrade_package_command =
  let files_arg =
    Arg.(value & pos_all OpamArg.existing_filename_dirname_or_dash [None] &
         info [] ~docv:"FILE" ~doc:
           "Package definition (\"opam\") files to update, or package \
            directory containing them, or \"-\" to read from stdin.")
  in
  let cmd files =
    OpamClientConfig.opam_init ();
    files |> List.map @@ fun f ->
    let upgrade_file f =
      let o1 = OpamFile.OPAM.read f in
      let o2 = OpamFormatUpgrade.opam_file o1 in
      if o2 <> o1 then
        (OpamFile.OPAM.write f o2;
         OpamConsole.note "File %s upgraded to format %s"
           (OpamFile.to_string f)
           (OpamVersion.to_string OpamFormatUpgrade.latest_version))
      else
        OpamConsole.note "File %s is already at latest version"
          (OpamFile.to_string f)
    in
    match f with
    | None ->
      OpamFile.OPAM.read_from_channel stdin |>
      OpamFormatUpgrade.opam_file |>
      OpamFile.OPAM.write_to_channel stdout
    | Some (OpamFilename.F f) -> upgrade_file (OpamFile.make f)
    | Some (OpamFilename.D d) ->
      match OpamPinned.files_in_source d with
      | [] -> OpamConsole.error "No opam files found in %s"
                (OpamFilename.Dir.to_string d)
      | fs -> List.iter (fun (_, f) -> upgrade_file f) fs
  in
  Term.(pure cmd $ files_arg),
  Term.info "opam-package-upgrade"
    ~doc:"Upgrades opam package definition files to the latest format version"
    ~man:[
      `S "DESCRIPTION";
      `P (Printf.sprintf
            "This simple command-line tool updates the format of opam files from \
             earlier versions to %s (current as of opam %s)"
            (OpamVersion.to_string (OpamFormatUpgrade.latest_version))
            (OpamVersion.to_string (OpamVersion.current)));
      `P "Files listed on the command-line are updated in place. You can also \
          specify a directory, in which case all opam files found there will \
          be updated. With $(b,-), or no argument, reads from stdin, and \
          prints back to stdout."
    ]

let () =
  OpamSystem.init ();
  match Term.eval upgrade_package_command with
  | `Error _ -> exit 1
  | _ -> exit 0

