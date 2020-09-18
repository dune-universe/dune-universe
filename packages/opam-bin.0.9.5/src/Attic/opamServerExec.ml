
open Ezcmd.Modules

let action args =
  OpamServerMisc.global_log "CMD: %s"
    ( String.concat "\n    " ( "exec" :: args) ) ;
  OpamServerMisc.make_cache_dir ();
  match args with

  | name :: version :: package_uid :: _depends :: ( ( cmd :: _ ) as args ) ->
    if not
        ( Sys.file_exists (OpamServerGlobals.package_binary_archive
                             ~name ~version ~package_uid ))
    then
      let source_archive =
        OpamServerGlobals.package_source_archive
          ~name ~version ~package_uid in
      if not
          ( Sys.file_exists source_archive ) then begin
        Printf.eprintf "Source archive does not exist. Packing sources...\n%!";
        let package_descr =
          OpamServerGlobals.package_archive_descr
            ~name ~version ~package_uid in
        if Sys.file_exists package_descr then
          Sys.remove package_descr;
        OpamServerMisc.log package_descr "create source archive";
        let temp_source_archive = source_archive ^ ".tmp" in
        OpamServerMisc.global_log "create source archive %s" source_archive;
        OpamServerMisc.call [| "tar"; "zcf" ; temp_source_archive ; "." |];
        Sys.rename temp_source_archive source_archive
      end;

      let package_descr =
        OpamServerGlobals.package_archive_descr
          ~name ~version ~package_uid in
      OpamServerMisc.log package_descr  "'%s'" (String.concat "' '" args);
      Unix.execvp cmd (Array.of_list args)
    else
      Printf.eprintf "Binary archive exists. Skipping command...\n%!"
  | _ ->
    Printf.eprintf
      "Unexpected args: usage is 'opam-server exec uid command...'\n%!";
    exit 2

let cmd =
  let args = ref [] in
  Arg.{
    cmd_name = "exec" ;
    cmd_action = (fun () -> action !args) ;
    cmd_args = [
      [], Anons (fun list -> args := list),
      Ezcmd.info "args"
    ];
    cmd_man = [];
    cmd_doc = "exec a command for a package build/install";
  }
