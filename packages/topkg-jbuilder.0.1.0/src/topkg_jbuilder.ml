open StdLabels
open Topkg

let build =
  Pkg.build ()
    ~cmd:(fun _c os _files ->
      let jbuilder = Conf.tool "jbuilder" os in
      OS.Cmd.run @@ Cmd.(jbuilder % "build" % "--root" % "."))
    ~clean:(fun os ~build_dir ->
      let rm = Conf.tool "rm" os in
      let find = Conf.tool "find" os in
      OS.Cmd.run @@ Cmd.(rm % "-rf" % build_dir) >>= fun () ->
      OS.Cmd.run @@ Cmd.(find % "." % "-name" % ".merlin" % "-delete"))

let describe
      ?delegate
      ?readmes
      ?licenses
      ?change_logs
      ?lint_files
      ?lint_custom
      ?distrib
      ?publish
      ?name
      ()
  =
  let opam_files =
    Sys.readdir "."
    |> Array.to_list
    |> List.filter ~f:(String.is_suffix ~affix:".opam")
  in
  if opam_files = [] then begin
    Log.err (fun m -> m "no <package>.opam files found.");
    exit 1
  end;
  let package_names =
    let suffix_len = String.length ".opam" in
    List.map opam_files ~f:(fun s ->
      String.sub s 0 (String.length s - suffix_len))
  in
  let name =
    match name with
    | Some name ->
      if not (List.mem name ~set:package_names) then begin
        Log.err (fun m -> m "file %s.opam doesn't exist." name);
        exit 1
      end;
      name
    | None ->
      let shortest =
        match package_names with
        | [] -> assert false
        | first :: rest ->
          List.fold_left rest ~init:first ~f:(fun acc s ->
            if String.length s < String.length acc then
              s
            else
              acc)
      in
      if List.for_all package_names ~f:(String.is_prefix ~affix:shortest) then
        shortest
      else begin
        Log.err (fun m ->
          m "cannot determine name automatically.\n\
             You must pass a [name] argument to \
             [Topkg_jbuilder.describe] in pkg/pkg.ml");
        exit 1
      end
  in
  Pkg.describe name
    ?delegate
    ?readmes
    ?licenses
    ?change_logs
    ~metas:[]
    ~opams:(List.map opam_files ~f:(Pkg.opam_file ~lint_deps_excluding:None))
    ?lint_files
    ?lint_custom
    ?distrib
    ?publish
    ~build
    @@ fun _c ->
    Ok [ (*Pkg.test "jbuilder" ~auto:false ~args:Cmd.(empty % "runtest")*) ]
