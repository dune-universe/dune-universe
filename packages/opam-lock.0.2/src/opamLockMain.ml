(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStateTypes
open Cmdliner

let get_git_url url nv dir =
  OpamFilename.in_dir dir @@ fun () ->
  try
    match OpamSystem.read_command_output ["git";"remote";"get-url";"origin"] with
    | [url0] ->
      let u = OpamUrl.parse ~backend:`git url0 in
      if OpamUrl.local_dir u <> None then None else
        let hash =
          match url.OpamUrl.hash with
          | None ->
            OpamProcess.Job.run (OpamGit.VCS.current_branch dir)
          | Some hash ->
            match OpamSystem.read_command_output
                    ["git"; "branch"; "-r"; "--contains"; hash] with
            | _::_ -> Some hash
            | [] ->
              (let b_default =
                 match List.map (fun x -> OpamStd.String.split x '/')
                         (OpamSystem.read_command_output
                            ["git"; "symbolic-re"; "refs/remotes/origin/HEAD"]) with
                 | [_::_::_::b::[]] -> Some b
                 | _ -> None
               in
               OpamConsole.warning
                 "Referenced git branch for %s is not available in remote: %s.%s"
                 (OpamConsole.colorise `underline (OpamPackage.to_string nv))
                 (OpamUrl.to_string u)
                 (OpamStd.Option.to_string
                    (fun b ->
                       Printf.sprintf "\nReplace it by default remote branch %s."
                         (OpamConsole.colorise `underline b))
                    b_default);
               b_default)
        in
        Some { u with OpamUrl.hash = hash }
    | _ -> None
  with OpamSystem.Command_not_found _ | OpamSystem.Process_error _ ->
    (OpamConsole.error "Can't retrieve remote informations for %s"
       (OpamPackage.to_string nv);
     None)

let lock_opam ?(only_direct=false) st opam =
  let opam = OpamFormatUpgrade.opam_file opam in
  let nv = OpamFile.OPAM.package opam in
  let st =
    { st with opams = OpamPackage.Map.add nv opam st.opams }
  in
  let univ =
    OpamSwitchState.universe st
      ~requested:(OpamPackage.Name.Set.singleton nv.name)
      Query
  in
  let all_depends =
    OpamSolver.dependencies
      ~depopts:true ~build:true ~post:true ~installed:true
      univ (OpamPackage.Set.singleton nv) |>
    List.filter (fun nv1 -> nv1 <> nv)
  in
  let depends =
    if only_direct then
      let names =
        OpamFilter.filter_formula ~default:true (fun _ -> None)
          (OpamFile.OPAM.depends opam) |>
        OpamFormula.fold_left (fun acc (n,_) -> OpamPackage.Name.Set.add n acc)
          OpamPackage.Name.Set.empty
      in
      List.filter (fun nv -> OpamPackage.Name.Set.mem nv.name names) all_depends
    else all_depends
  in
  let depends = List.sort (fun a b -> compare b.name a.name) depends in
  let depends_formula =
    OpamFormula.ands
      (List.rev_map (fun nv ->
           Atom (nv.name, Atom
                   (Constraint
                      (`Eq, FString (OpamPackage.version_to_string nv)))))
          depends)
  in
  let all_depopts =
    OpamFormula.packages st.packages
      (OpamFilter.filter_deps
         ~build:true ~test:true ~doc:true ~dev:true ~default:true ~post:false
         (OpamFile.OPAM.depopts opam))
  in
  let installed_depopts = OpamPackage.Set.inter all_depopts st.installed in
  let uninstalled_depopts =
    OpamPackage.(Name.Set.diff
                   (names_of_packages all_depopts)
                   (names_of_packages installed_depopts))
  in
  let conflicts =
    OpamFormula.ors
      (OpamFile.OPAM.conflicts opam ::
       List.map (fun n -> Atom (nv.name, Empty))
         (OpamPackage.Name.Set.elements uninstalled_depopts))
  in
  let pin_depends =
    OpamStd.List.filter_map (fun nv ->
        if not (OpamSwitchState.is_pinned st nv.name) then None else
        match OpamSwitchState.primary_url st nv with
        | None -> None
        | Some u ->
          match OpamUrl.local_dir u with
          | Some d ->
            let err () =
              OpamConsole.warning "Dependency %s is pinned to local target %s"
                (OpamPackage.to_string nv) (OpamUrl.to_string u);
              None
            in
            if u.OpamUrl.backend = `git then
              match get_git_url u nv d with
              | Some resolved_u ->
                OpamConsole.note "Local pin %s resolved to %s"
                  (OpamUrl.to_string u) (OpamUrl.to_string resolved_u);
                Some (nv, resolved_u)
              | None -> err ()
            else err ()
          | None -> Some (nv, u))
      all_depends
  in
  opam |>
  OpamFile.OPAM.with_depopts OpamFormula.Empty |>
  OpamFile.OPAM.with_depends depends_formula |>
  OpamFile.OPAM.with_conflicts conflicts |>
  OpamFile.OPAM.with_pin_depends pin_depends

let lock_command only_direct switch files =
  let switch =
    OpamStd.Option.map OpamSwitch.of_string switch
  in
  let files =
    List.fold_left (fun acc f ->
        if Sys.is_directory f then
          let d = OpamFilename.Dir.of_string f in
          let fs = OpamPinned.files_in_source d in
          if fs = [] then
            OpamConsole.error_and_exit `Bad_arguments
              "No package definition files found at %s"
              OpamFilename.Dir.(to_string d);
          List.rev_append fs acc
        else
          let file = OpamFilename.of_string f in
          (OpamPinned.name_of_opam_filename (OpamFilename.dirname file) file,
            OpamFile.make file) :: acc)
      [] files
    |> List.rev
  in
  let opams =
    List.map (fun (nameopt, f) ->
        let opam = OpamFile.OPAM.read f in
        let opam =
          match nameopt with
          | None -> opam
          | Some n -> OpamFile.OPAM.with_name n opam
        in
        let opam =
          match OpamFile.OPAM.version_opt opam with
          | None ->
            OpamFile.OPAM.with_version (OpamPackage.Version.of_string "dev") opam
          | Some version -> opam
        in
        f,opam)
      files
  in
  let gt = OpamGlobalState.load `Lock_none in
  OpamSwitchState.with_ `Lock_none ?switch gt @@ fun st ->
  List.iter (fun (f, opam) ->
      let locked = lock_opam ~only_direct st opam in
      let locked_file =
        OpamFile.(make (OpamFilename.add_extension (filename f) "locked"))
      in
      OpamFile.OPAM.write_with_preserved_format ~format_from:f locked_file locked;
      OpamConsole.msg "Wrote %s\n" (OpamFile.to_string locked_file))
    opams

let only_direct_flag =
  Arg.(value & flag & info ["direct-only"; "d"] ~doc:
         "Only lock direct dependencies, rather than the whole dependency tree")

let switch_arg =
  Arg.(value & opt (some string) None & info ["switch"] ~docv:"SWITCH" ~doc:
         "Select the opam switch to use to determine the versions to lock")

let opamfile_arg =
  Arg.(value & pos_all file ["."] & info [] ~docv:"FILE" ~doc:
         "Select opam files to rewrite. the output will be put in \
          $(i,FILE).locked. If a directory is specified, all package \
          definitions in this directory will be processed.")

let man = [
  `S "DESCRIPTION";
  `P "This utility reads opam package definitions files, checks the current \
      state of their installed dependencies, and outputs modified versions of \
      the files with a $(i,.locked) suffix, where all the (transitive) \
      dependencies and pinnings have been bound strictly to the currently \
      installed version.";
  `P "By using these locked opam files, it is then possible to recover the \
      precise build environment that was setup when they were generated."
]

let lock_command =
  Term.(pure lock_command $ only_direct_flag $ switch_arg $ opamfile_arg),
  Term.info "opam-lock" ~man ~doc:
    "Create locked opam files to share build environments across hosts."

let () =
  OpamSystem.init ();
  let root = OpamStateConfig.opamroot () in
  ignore (OpamStateConfig.load_defaults root);
  OpamFormatConfig.init ();
  OpamStd.Config.init ~safe_mode:true ();
  OpamRepositoryConfig.init ();
  OpamSolverConfig.init ();
  OpamStateConfig.init ();
  ignore @@ Term.eval lock_command
