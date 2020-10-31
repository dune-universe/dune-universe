(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.TYPES
open EzFile.OP
open EzConfig.OP

let cmd_name = "install"

let add_repo ~repo ~url =
  if not ( Sys.file_exists (Globals.opam_repo_dir // repo ) ) then
    Misc.call
      [| "opam"; "remote" ; "add" ; repo ; "--dont-select"; url |]

let install_exe () =
  let s = FileString.read_file Sys.executable_name in
  if Sys.file_exists Globals.opambin_bin then
    Sys.remove Globals.opambin_bin;
  EzFile.write_file Globals.opambin_bin s;
  Unix.chmod  Globals.opambin_bin 0o755;
  Printf.eprintf "Executable copied as %s\n%!" Globals.opambin_bin;
  EzFile.make_dir ~p:true Globals.opam_plugins_bin_dir ;
  Misc.call [|
    "ln"; "-sf" ;
    ".." // Globals.command // Globals.command_exe ;
    Globals.opam_plugins_bin_dir // Globals.command
  |]

let hooks = [
  "pre-session-commands",
  Printf.sprintf {| ["%s" "pre-session"] |}
    Globals.opambin_bin ;
  "pre-build-commands",
  Printf.sprintf
    {| ["%s" "pre-build" name version "--opamfile=%%{opamfile}%%" {?opamfile} depends] |}
    Globals.opambin_bin ;
  "wrap-build-commands",
  Printf.sprintf {| ["%s" "wrap-build" name version depends "--"] |}
    Globals.opambin_bin ;
  "pre-install-commands",
  Printf.sprintf {| ["%s" "pre-install" name version depends] |}
    Globals.opambin_bin ;
  "wrap-install-commands",
  Printf.sprintf {| ["%s" "wrap-install" name version depends "--"] |}
    Globals.opambin_bin ;
  "post-install-commands",
  Printf.sprintf
    {| ["%s" "post-install" name version "--opamfile=%%{opamfile}%%" {?opamfile} depends installed-files] { error-code = 0} |}
    Globals.opambin_bin  ;
  "post-session-commands",
  Printf.sprintf {| ["%s" "post-session"] |}
    Globals.opambin_bin  ;
  "pre-remove-commands",
  Printf.sprintf {| ["%s" "pre-remove" name version depends] |}
    Globals.opambin_bin ;
]

let remove_opam_hooks file_contents =
  let rec iter items found rev =
    match items with
    | [] ->
      if found then begin
        Printf.eprintf "Found hooks to remove\n%!";
        Some ( List.rev rev )
      end
      else begin
        Printf.eprintf "No hooks to remove\n%!";
        None
      end
    | item :: items ->
      match item with
        | OpamParserTypes.Variable (_, name, _) ->
            if List.mem_assoc name hooks then
              iter items true rev
            else
              iter items found ( item :: rev )
      | _ ->
        iter items found ( item :: rev )
  in
  iter file_contents false []

let install_hooks () =

  Misc.change_opam_config (fun file_contents ->
      let file_contents =
        match remove_opam_hooks file_contents with
        | None -> file_contents
        | Some file_contents -> file_contents
      in
      Printf.eprintf "Adding %s hooks\n%!" Globals.command;
      Some (
        List.rev @@
        (
          (
            List.map (fun (hook_name, hook) ->
                Misc.opam_variable hook_name "%s" hook) hooks
          )
          @
          List.rev file_contents
        )
      )
    )

let install_repos () =

  EzFile.make_dir ~p:true Globals.opambin_store_repo_packages_dir;
  EzFile.write_file ( Globals.opambin_store_repo_dir // "repo" )
    {|
opam-version: "2.0"
archive-mirrors: "../../cache"
|};
  EzFile.write_file ( Globals.opambin_store_repo_dir // "version" )
    "0.9.0";

  add_repo ~repo:Globals.opam_opambin_repo
    ~url:( Printf.sprintf "file://%s"
             Globals.opambin_store_repo_dir )

let install_patches () =
  let patches_url = !!Config.patches_url in
  if EzString.starts_with patches_url ~prefix:"file://" then
    (* nothing to do *)
    Printf.eprintf "Using %s for patches\n%!" patches_url
  else
    let opambin_patches_dir = Globals.opambin_patches_dir in
    let tmp_dir = opambin_patches_dir ^ ".tmp" in

    if EzString.starts_with patches_url ~prefix:"git@" then begin
      Misc.call [| "rm"; "-rf"; tmp_dir |];
      Misc.call [| "git"; "clone" ; patches_url ; tmp_dir |];
      Misc.call [| "rm"; "-rf"; opambin_patches_dir |];
      Misc.call [| "mv"; tmp_dir; opambin_patches_dir |]
    end else

    if EzString.starts_with patches_url ~prefix:"https://"
    || EzString.starts_with patches_url ~prefix:"http://" then begin
      let output = Globals.opambin_dir // "relocation-patches.tar.gz" in
      Printf.eprintf "Downloading patches...\n%!";
      match Misc.wget ~url:patches_url ~output with
      | None ->
        Printf.kprintf failwith "Could not retrieve archive at %s" patches_url
      | Some output ->

        Misc.call [| "rm"; "-rf"; tmp_dir |];
        EzFile.make_dir ~p:true tmp_dir ;

        Unix.chdir tmp_dir ;
        Misc.call [| "tar" ; "zxf" ; output |] ;
        Unix.chdir Globals.curdir;

        let patches_subdir = tmp_dir // "patches" in
        if not ( Sys.file_exists patches_subdir ) then
          Printf.kprintf failwith
            "archive %s does not contain 'patches/' subdir" patches_url;

        Misc.call [| "rm"; "-rf"; opambin_patches_dir |];
        EzFile.make_dir ~p:true opambin_patches_dir;
        Sys.rename patches_subdir (opambin_patches_dir // "patches");
        Misc.call [| "rm"; "-rf"; tmp_dir |];
        Sys.remove output

    end
    else
      begin
        Printf.eprintf
          "Error: patches_url '%s' should either be local (file://) or git (git@, http[s]://)\n%!" patches_url;
        exit 2
      end


let action args =
  Printf.eprintf "%s\n\n%!" Globals.about ;

  EzFile.make_dir ~p:true Globals.opambin_dir ;
  Config.save ();

  EzFile.make_dir ~p:true Globals.opambin_cache_dir;

  match args with
  | [] ->
    install_exe ();
    install_hooks ();
    install_repos ();
    install_patches ()
  | _ ->
    List.iter (function
        | "exe" -> install_exe ()
        | "hooks" -> install_hooks ()
        | "repos" -> install_repos ()
        | "patches" -> install_patches ()
        | s ->
          Printf.eprintf "Error: unexpected argument %S" s;
          exit 2)
      args

let cmd =
  let anon_args = ref [] in
  {
    cmd_name ;
    cmd_action = (fun () -> action !anon_args) ;
    cmd_args = [

      [], Anons (fun list -> anon_args := list),
      Ezcmd.info "No args = all, otherwise 'exe', 'hooks' and/or 'repos'";

    ];
    cmd_man = [
      `S "DESCRIPTION" ;
      `Blocks [
        `P {|Here are the names of the actions performed by the command, that can be passed as arguments:|};
        `I ("[exe]",
            {|Install the current executable as an opam plugin, so that it is possible to use "opam bin" instead of "opam-bin" from anywhere;|});
        `I ("[hooks]",
            {|Modify the hooks of opam to call opam-bin everytime a package is installed;|});
        `I ("[repos]",
            {|Add the repository of generated binary packages as one of the default opam repository|});
        `I ("[patches]", {|Download/upgrade the set of relocation patches|});
      ];
      `P {|Without argument, all the actions are performed.|}
    ];
    cmd_doc = {|
installs opam-bin in opam and download the set of relocation patches.
|}
  }
