
open Ezcmd.Modules

let action args =
  OpamServerMisc.global_log "CMD: %s"
    ( String.concat "\n    " ( "checkout" :: args) ) ;
  match args with
  | name :: version :: package_uid :: _depends :: [] ->
    let binary_archive =
      OpamServerGlobals.package_binary_archive
        ~name ~version ~package_uid in
    if Sys.file_exists binary_archive then begin
      match OpamServerMisc.opam_switch_prefix () with
      | None -> ()
      | Some destdir ->
        Printf.eprintf "Binary archive exists. Extracting...\n%!";
        let install_file = Printf.sprintf "%s.install" name in
        if Sys.file_exists install_file then
          Sys.remove install_file;
        Unix.chdir destdir;
        Unix.execvp "tar" [| "tar"; "-Jxf" ; binary_archive |]
    end
  | _ ->
    Printf.eprintf
      "Unexpected args: usage is 'opam-server checkout uid name'\n%!";
    exit 2

let cmd =
  let args = ref [] in
  Arg.{
  cmd_name = "checkout" ;
  cmd_action = (fun () -> action !args) ;
  cmd_args = [
    [], Anons (fun list -> args := list),
    Ezcmd.info "args"
  ];
  cmd_man = [];
  cmd_doc = "install a package from cache";
}
