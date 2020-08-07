(* Copyright (C) 2017 Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
 *)

open Support
open Container


module Topisort = Topological_sort.Make (Module.MSet)

module PC = Proof_context



let alba_version = "0.4.x-dune"



let alba_directory_path (cmd_line:Command_line.t): string =
  (* A path to the '.alba' directory of the working directory
   *)
  Platform.Filename.concat
    (Command_line.working_directory cmd_line)
    ".alba"




let has_alba_dir (cmd_line:Command_line.t): bool =
  (* Does the working directory have a '.alba' directory? *)
  try
    Platform.is_directory (alba_directory_path cmd_line)
  with Sys_error _ ->
    false



let check_alba_dir (cmd_line:Command_line.t): unit =
  (* Check if the working directory has a '.alba' directory? *)
  if not (has_alba_dir cmd_line) then begin
    Format.printf
      "@[<v>%s \"%s\" %s@,@,%s@,@,    %s@,    %s@."
      "The working directory"
      (Command_line.working_directory cmd_line)
      "is not an Albatross directory."
      "Hint: In order to create an Albatross directory issue one of the commands"
      "alba init"
      "alba init -work-dir <path>";
    exit 1
  end





let init (cmd_line:Command_line.t): unit =
  (* Initialize the working directory provided on the command line as
     an Alba directory, i.e. create an empty subdirectory '.alba'.*)
  let path = alba_directory_path cmd_line in
  try
    Platform.mkdir path 0o755
  with Sys_error str ->
    Format.eprintf "@[<v>%s \"%s\"@,@,@[<hov>%s@ %s@ %s@]@]@."
      "Cannot create directory"
      path
      "Either the directory already exists"
      "or it is an illegal path"
      "or you don't have the permission to create the directory";
    exit 1



let get_module_set (cmd:Command_line.t): Module.MSet.t =
  check_alba_dir cmd;
  Module.make_set cmd





let status (cmd:Command_line.t): unit =
  let mset = get_module_set cmd in
  let print_src (n:int) (s:Module.Src.t): int =
    let open Module.Src in
    let open Format in
    if is_new s then
      (printf "  %-8s  %s@." "new" (path s); n+1)
    else if is_modified s then
      (printf "  %-8s  %s@." "modified" (path s); n+1)
    else if is_affected s then
      (printf "  %-8s  %s@." "affected" (path s); n+1)
    else
      n
  in
  let n =
    Module.MSet.fold
      (fun n m ->
        let open Module in
        let n =
          if M.has_interface m then
            print_src n (M.interface m)
          else
            n
        in
        if M.has_implementation m then
          print_src n (M.implementation m)
        else
          n
      )
      0
      mset
  in
  if n=0 then
    Format.printf "Nothing to be done@."



let analyze
      (ast:declaration list) (pc:PC.t) (src:Module.Src.t)
    : unit =
  try
    Ast.analyze ast pc
  with Error_info (info,str) ->
    Module.Src.info_abort info str src



let add_used_module (m:Module.M.t) (mset:Module.MSet.t) (pc:PC.t): unit =
  let open Module in
  assert (M.has_interface m);
  let src_ali = M.interface m in
  PC.add_used_module m pc;
  let ast = Src.parse src_ali in
  analyze ast pc src_ali



let check_core (mset:Module.MSet.t): unit =
  let open Module in
  if MSet.has_id 0 mset then
    begin
      let open Format in
      let m0 = MSet.module_of_id 0 mset in
      if ST.string (M.base_name m0) <> "core" then
        begin
          eprintf "The module \"core\" is not used@.";
          exit 1
        end
      else
        let pkg = string_of_library (M.package_name m0) in
        if pkg <> "" && pkg <> "alba.base" then
          begin
            eprintf "@[<v>The module \"core\" is used from the package@,@,";
            eprintf "  %s" pkg;
            eprintf "@,@,instead of the package@,@,  alba.base@]@."
          end
    end


let compile_module (m:Module.M.t) (mset:Module.MSet.t): unit =
  check_core mset;
  let open Module in
  assert (M.has_implementation m);
  let verbosity = MSet.verbosity mset in
  let comp = Compile.make m mset in
  if verbosity > 0 then
    Format.printf "Compile module \"%s\"@." (M.string_of_name m);
  let pc = PC.make comp in
  let src_al = M.implementation m in
  let deps = Src.full_dependencies src_al in
  List.iter
    (fun i ->
      let m = MSet.module_of_id i mset in
      if verbosity > 1 then
        Format.printf " use \"%s\"@." (M.string_of_name m);
      add_used_module m mset pc
    )
    deps;
  let nme,_ = M.name m
  in
  PC.add_current_module m pc;
  let ast = Src.parse src_al in
  if verbosity > 1 then
    Format.printf " verify implementation \"%s\"@." (ST.string nme);
  analyze ast pc src_al;
  if M.has_interface m then
    begin
      if verbosity > 1 then
        Format.printf " verify interface \"%s\"@." (ST.string nme);
      MSet.verify_dependencies m (Compile.set comp);
      let src_ali = M.interface m in
      let ast = Src.parse src_ali in
      PC.set_interface_check pc;
      analyze ast pc src_ali;
      Module.Src.write_meta src_ali
    end;
  Module.Src.write_meta src_al



let compile (cmd:Command_line.t): unit =
  (* Compile the modules provided as arguments on the command line or all
     modules, if no arguments are provided. Compilation is done only when
     needed or if the option [-force] is activated. *)
  let mset = get_module_set cmd in
  let open Module in
  MSet.iter
    (fun m ->
      if M.is_external m && M.is_affected m then
        begin
          Format.eprintf
            "%s \"%s\" %s@."
            "The package"
            (string_of_library (M.package_name m))
            "has to be recompiled before this package can be compiled.";
          exit 1
        end
      else if M.is_external m then
        ()
      else if M.is_affected m
              || Command_line.is_forced (M.base_name m) cmd then
        compile_module m mset
      else
        ()
    )
    mset







let run (): unit =
  let cmd_line = Command_line.get () in
  try begin
      match Command_line.command cmd_line with
      | ["init"] ->
         init cmd_line
      | ["status"] ->
         if Command_line.arguments cmd_line <> [] then begin
             Format.eprintf "The command \"status\" does not accept arguments@.";
             exit 1
           end;
         status cmd_line
      | ["compile"] ->
         compile cmd_line
      | ["version"] ->
         Format.printf "Version %s@." alba_version
      | _ ->
         assert false (* Cannot happen, Command_line.get () must not return an
                      illegal command *)
    end
  with Sys_error str ->
    Format.printf "Unexpected system error \"%s\"@." str;
    exit 2
