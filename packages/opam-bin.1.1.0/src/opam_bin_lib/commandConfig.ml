(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Ez_opam_file.V1
open Ezcmd.TYPES
open EzConfig.OP
open EzFile.OP

module OpamParserTypes = OpamParserTypes.FullPos
module OpamParser = OpamParser.FullPos
module OpamPrinter = OpamPrinter.FullPos

let need_saving = ref false
let need_refactoring = ref false
let need_install_patches = ref false

let refactor () =
  Printf.eprintf "Refactoring...\n%!";
  let open OpamParserTypes in
  let refactor = function
    | { pelem = Section s; pos} ->
        let s =
          if s.section_kind.pelem = "url" then
            let pelem =
              List.map (fun v ->
                  match v.pelem with
                  | Variable ( ({ pelem = "src"; _ } as src),
                               { pelem = String url; pos = url_pos }) ->
                      let archive = Filename.basename url in
                      let url = !!Config.base_url //
                                "archives" // archive in
                      { pos = v.pos ;
                        pelem = Variable (src, { pelem = String url; pos = url_pos })}
                  | _ -> v) s.section_items.pelem
            in
            { s with section_items = { pos = s.section_items.pos; pelem }}
          else s
        in
        { pos; pelem = Section s }
    | v -> v
  in
  let f path =
    let file = Globals.opambin_store_repo_packages_dir // path in
    let opam = OpamParser.file file in
    let opam = { opam with
                 file_contents = List.map refactor opam.file_contents }
    in
    EzFile.write_file file (OpamPrinter.opamfile opam)
  in
  let select = EzFile.select ~deep:true ~glob:"opam" () in
  EzFile.iter_dir ~select ~f Globals.opambin_store_repo_packages_dir

let action () =
  Printf.eprintf "%s\n%!" Globals.about ;

  if !need_saving then begin
    EzFile.make_dir ~p:true Globals.opambin_dir ;
    Config.save ();
    if !need_refactoring then refactor ();
    if !need_install_patches then
      CommandInstall.install_patches ();
  end else begin
    let open EzConfig.LowLevel in
    Printf.printf "Current options (from %s):\n"
      Config.config_filename;
    let options = simple_options "" Config.config in
    List.iter (fun o ->
        Printf.printf "  %s : %s\n"
          ( String.concat "." o.option_name )
          o.option_value ;
      ) options ;
    Printf.printf "  switches : %s\n"
      (String.concat "  " !!Config.switches );
    Printf.printf "  protected_switches : %s\n"
      (String.concat "  " !!Config.protected_switches );
  end;
  ()

let modify_list_of_switches option s =
  let add s =
    option =:= !! Config.switches @ [ s ]
  in
  let remove s =
    Config.switches =:=
    List.filter ( (<>) s ) !!Config.switches
  in
  List.iter (function
      | "" -> ()
      | "-" -> Config.switches =:= []
      | s ->
        let c = s.[0] in
        let len = String.length s in
        match c with
        | '+' -> add ( String.sub s 1 ( len - 1 ))
        | '-' -> remove ( String.sub s 1 ( len - 1 ))
        | _ -> add s
    ) (EzString.split s ',')

let cmd = {
  cmd_name = "config" ;
  cmd_action = action ;
  cmd_args = [

    [ "base-url" ], Arg.String (fun s ->
        Config.base_url =:= s;
        need_refactoring := true;
        need_saving := true;
      ),
    Ezcmd.info "URL where the archives folder is available";

    [ "rsync-url" ], Arg.String (fun s ->
        Config.rsync_url =:= ( match s with
            | "" | "-" -> None
            | _ -> Some s );
        need_saving := true;
      ),
    Ezcmd.info @@
    Printf.sprintf
      "target for rsync to push new binary packages with `%s push`"
      Globals.command;

    [ "patches-url" ], Arg.String (fun s ->
        Config.patches_url =:= s ;
        need_saving := true ;
        need_install_patches := true;
      ),
    Ezcmd.info @@
    Printf.sprintf
      "location of relocation patches (git@, file:// or https://)" ;

    [ "title" ], Arg.String (fun s ->
        Config.title =:= s;
        need_saving := true;
      ),
    Ezcmd.info @@
    "The title in the generated index.html file" ;

    [ "enable-create" ], Arg.Unit (fun () ->
        Config.create_enabled =:= true ;
        need_saving := true;
      ),
    Ezcmd.info
      "create a binary package after building a source package";

    [ "disable-create" ], Arg.Unit (fun () ->
        Config.create_enabled =:= false ;
        need_saving := true;
      ),
    Ezcmd.info
      "opposite of --enable-create";

    [ "enable-share" ], Arg.Unit (fun () ->
        Config.share_enabled =:= true ;
        need_saving := true;
      ),
    Ezcmd.info
      "share binary and source files between switches";

    [ "disable-share" ], Arg.Unit (fun () ->
        Config.share_enabled =:= false ;
        need_saving := true;
      ),
    Ezcmd.info
      "opposite of --enable-share";

    [ "enable" ], Arg.Unit (fun () ->
        Config.enabled =:= true ;
        need_saving := true;
      ),
    Ezcmd.info "enable binary packages";

    [ "disable" ], Arg.Unit (fun () ->
        Config.enabled =:= false ;
        need_saving := true;
      ),
    Ezcmd.info
      "disable binary packages";

    [ "all-switches" ], Arg.Unit (fun () ->
        Config.all_switches =:= true ;
        need_saving := true ;
      ),
    Ezcmd.info "Activate on all switches, except protected switches";

    [ "not-all-switches" ], Arg.Unit (fun () ->
        Config.all_switches =:= false ;
        need_saving := true ),
    Ezcmd.info "Activate only on switches specified with --switches";

    [ "switches" ], Arg.String (fun s ->
        modify_list_of_switches Config.switches s ;
        need_saving := true;
      ),
    Ezcmd.info "With --not-all-switches, specify which switches \
                (comma-separated) should create/use binary \
                packages. '-' means none, 'SWITCH' or '+SWITCH' means \
                add SWITCH to the list, `-SWITCH` means remove SWITCH \
                from the list. A glob regexp can also be used for \
                SWITCH.";

    [ "save" ], Arg.Set need_saving,
    Ezcmd.info
      "Save new version of config file (~/.opam/plugins/opam-bin/config)";

    [ "protected-switches" ], Arg.String (fun s ->
        modify_list_of_switches Config.protected_switches s ;
        need_saving := true;
      ),
    Ezcmd.info "With --all-switches, specify which switches \
                (comma-separated) should NOT create/use binary \
                packages. '-' means none, 'SWITCH' or '+SWITCH' means \
                add SWITCH to the list, `-SWITCH` means remove SWITCH \
                from the list. A glob regexp can also be used for \
                SWITCH.";

  ];
  cmd_man = [];
  cmd_doc = "configure options";
}
