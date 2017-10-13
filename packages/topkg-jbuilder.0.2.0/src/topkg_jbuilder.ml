open StdLabels
open Topkg
open Fpath

let uerror = Unix.error_message

let rec mkdir dir =
  let mode = 0o755 in
  let aux d =
    Log.debug (fun l -> l "mkdir %s" dir);
    try Ok (Unix.mkdir d mode) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
    | Unix.Unix_error (e, _, _) ->
      if d = dir
      then R.error_msgf "create directory %s: %s" d (uerror e)
      else R.error_msgf "create directory %s: %s: %s" dir d (uerror e)
  in
  OS.Dir.exists dir >>= function
  | true  -> Ok ()
  | false ->
    let parent = Fpath.dirname dir in
    OS.Dir.exists parent >>= function
    | true  -> aux dir
    | false -> mkdir parent >>= fun () -> aux dir

let remove_file file =
  Log.debug (fun l -> l "remove file %s" file);
  try Ok (Unix.unlink file) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  | Unix.Unix_error (e, _, _) ->
    R.error_msgf "remove file %s: %s" file (uerror e)

let remove_dir dir =
  Log.debug (fun l -> l "remove dir %s" dir);
  try Ok (Unix.rmdir dir) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  | Unix.Unix_error (e, _, _) ->
    R.error_msgf "remove directory %s: %s" dir (uerror e)

let rmdir dir =
  let rec aux = function
    | [] -> Ok ()
    | path :: todo ->
      if not (Sys.file_exists path) then Ok ()
      else if not (Sys.is_directory path) then
        remove_file path >>= fun () ->
        aux todo
      else
        OS.Dir.contents ~dotfiles:true ~rel:false path >>= fun paths ->
        if paths = [] then
          remove_dir path >>= fun () ->
          aux todo
        else
          aux (paths @ path :: todo)
  in
  aux [dir]

let rename src dst =
  Log.debug (fun l -> l "rename %s into %s" src dst);
  try Ok (Unix.rename src dst) with
  | Unix.Unix_error (e, _, _) ->
    R.error_msgf "rename %s into %s: %s" src dst (uerror e)

let is_doc files =
  (* [topkg doc] generate a call to build index.html *)
  List.mem "doc/api.docdir/index.html" ~set:files

let build =
  let run_jbuilder conf os args =
      let jbuilder = Conf.tool "jbuilder" os in
      let dev_opt = Cmd.(on (Conf.build_context conf = `Dev) (v "--dev")) in
      OS.Cmd.run @@ Cmd.(jbuilder %% args % "--root" % "." %% dev_opt)
  in
  Pkg.build ()
    ~cmd:(fun conf os files ->
      Log.debug (fun l -> l "files=%s" (String.concat ", " files));
      if is_doc files then (
        run_jbuilder conf os Cmd.(empty % "build" % "@doc")
        >>= fun () ->
        let src = "_build" // "default" // "_doc" in
        let dst = "_build" // "doc" // "api.docdir" in
        rmdir dst >>= fun () ->
        mkdir dst >>= fun () ->
        rename src dst
      ) else
        run_jbuilder conf os Cmd.(empty % "build"))
    ~post:(fun conf ->
        if Conf.build_tests conf then
          run_jbuilder conf `Host_os Cmd.(empty % "runtest" % "-j" % "1")
        else Ok ()
    )
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
    Ok []
