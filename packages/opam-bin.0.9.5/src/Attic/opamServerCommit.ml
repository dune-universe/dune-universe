
open EzCompat
open Ezcmd.Modules
open EzFile.String.Op
open OpamParserTypes

let parse_opam_file file_name =
  if Sys.file_exists file_name then begin
    let opam = OpamParser.file file_name in
    OpamServerMisc.global_log "%s read" opam.file_name;
    opam.file_contents
  end else begin
    OpamServerMisc.global_log "%s does not exist" file_name;
    []
  end

(*
  let v = {
    name ; version ;
    maintainer = None ;
    authors = None ;
    license = None ;
    homepage = None ;
    synopsis = None ;
  } in
  List.iter (function
      | Variable (_, name, value) -> begin
          match name, value with
          | "version", String (_, x) -> assert (v.version = x)
          | "maintainer", String (_, s) -> v.maintainer <- Some s
          | "authors", String (_, s) -> v.authors <- Some [ s ]
          | "authors", List (_, l) ->
            let list = ref [] in
            List.iter (function
                | String (_, s) -> list := s :: !list
                | _ -> ()
              ) l;
            if !list <> [] then
              v.authors <- Some (List.rev !list)
          | "license", String (_, s) -> v.license <- Some s
          | "homepage", String (_, s) -> v.homepage <- Some s
          | "synopsis", String (_, s) -> v.synopsis <- Some s
          | _ -> ()
        end
      | _ -> ()
    ) opam.file_contents;
  v
*)

let commit name version package_uid _depends files =
  let binary_archive =
    OpamServerGlobals.package_binary_archive ~name ~version ~package_uid in
  if not ( Sys.file_exists binary_archive ) then begin
    match OpamServerMisc.opam_switch_prefix () with
    | None -> ()
    | Some destdir ->

      Printf.eprintf "Binary archive does not exist: packing...\n%!";
      let package_descr =
        OpamServerGlobals.package_archive_descr
          ~name ~version ~package_uid in
      OpamServerMisc.log package_descr "create binary archive";
      OpamServerMisc.global_log "create binary archive %s..." binary_archive;
      OpamServerMisc.global_log "0";
      Unix.chdir destdir;
      OpamServerMisc.global_log "1";
      let temp_binary_archive = binary_archive ^ ".tmp" in
      begin
        match files with
        | [] ->
          (* an empty tar achive is this... *)
          EzFile.String.write_file temp_binary_archive
            (String.make 10240 '\000');
          OpamServerMisc.call
            [| "gzip" ; "-n"; temp_binary_archive |];
          Sys.rename ( temp_binary_archive ^ ".gz" ) binary_archive
        | _ ->
          OpamServerMisc.call
          @@
          Array.of_list ( "tar" ::
                          "-cf" :: temp_binary_archive ::
                          "--mtime=2020/07/13" ::
                          "--group=user:1000" ::
                          "--owner=user:1000" ::
                          files );
          OpamServerMisc.call [| "gzip"; "-n"; temp_binary_archive |];
          Sys.rename ( temp_binary_archive ^ ".gz" ) binary_archive;

      end;
      OpamServerMisc.global_log "3 %S" OpamServerGlobals.curdir;
      Unix.chdir OpamServerGlobals.curdir;
      OpamServerMisc.global_log "create binary archive DONE";

      let md5 =
        let content = EzFile.String.read_file binary_archive in
        let v = Digest.string content in
        Digest.to_hex v
      in
      OpamServerMisc.global_log "bin md5 = %s" md5;

      let opam_file = name ^ ".opam" in
      let opam = parse_opam_file opam_file in

      let repos = OpamServerGlobals.opam_server_repos in
      let switch =
        let basename = Filename.basename destdir in
        if String.lowercase basename = "_opam" then
          Filename.basename (Filename.dirname destdir)
        else
          basename
      in
      let new_version = Printf.sprintf "%s+bin+%s" version md5 in
      let package_dir =
        repos // OpamServerGlobals.system // switch // "packages"
        // Printf.sprintf "%s.%s" name new_version
      in
      EzFile.String.make_dir ~p:true package_dir;

      let opam =
        let file_contents =
          List.fold_left (fun acc v ->
              match v with
              | Variable (_, name, _value) -> begin
                  match name with

                  (* keep *)
                  | "maintainer"
                  | "authors"
                  | "opam-version"
                  | "synopsis"
                  | "description"
                  | "homepage"
                  | "bug-reports"
                  | "license"
                  | "tags" (* ?? *)
                  | "dev-repo"
                  | "post-messages"
                  | "doc"
                  | "setenv"
                  | "conflict-class"
                  | "flags"
                    -> v :: acc

                  (* discard *)
                  | "version"
                  | "build"
                  | "install"
                  | "remove"
                  | "depends"
                  | "depopts"
                    ->
                    acc
                  | _ ->
                    OpamServerMisc.global_log
                      "discarding unknown field %S" name;
                    acc
                end
              | _ -> acc
            ) [] opam in
        (*
        Variable(0, "build",
              OpamParser.value_from_string {| [[  "cp" "-aT" "." "%{prefix}%" ]] |} *)
        let file_contents = List.rev file_contents in
        { file_contents ; file_name = "" }
      in
      let s = OpamPrinter.opamfile opam in
      EzFile.String.write_file ( package_dir // "opam" ) s
  end

let action args =
  OpamServerMisc.global_log "CMD: %s\n%!"
    ( String.concat "\n    " ( "commit" :: args) ) ;
  OpamServerMisc.make_cache_dir ();
  match args with
  | name :: version :: package_uid :: depends :: files ->
    commit name version package_uid depends files
  | _ ->
    Printf.eprintf
      "Unexpected args: usage is 'opam-server commit uid files...'\n%!";
    exit 2

let cmd =
  let args = ref [] in
  Arg.{
  cmd_name = "commit" ;
  cmd_action = (fun () -> action !args) ;
  cmd_args = [
    [], Anons (fun list -> args := list),
    Ezcmd.info "args"
  ];
  cmd_man = [];
  cmd_doc = "Save files after install";
}
