open Graphics;;
open Format;;

let print_apgv_version version =
  prerr_endline
   (Printf.sprintf
      "Apgv, the Active-DVI portable graphics previewer, version %s"
      version);
  exit 0
;;

let print_apgv_short_version () =
  print_apgv_version Config.apgv_short_version
;;

let print_apgv_full_version () =
  print_apgv_version Config.apgv_full_version
;;

let version_spec = function
  | "-v" as opt ->
       opt, Arg.Unit print_apgv_short_version,
       ": print Apgv version."
  | opt ->
       opt, Arg.Unit print_apgv_full_version,
       "  print the full Apgv version, sub-version and release date."
;;

let apgv_geometry = ref " 640x480";;

let set_geom s = apgv_geometry := s;;

let spec_list = [
  ("-geometry", Arg.String set_geom,
   Printf.sprintf
     "<geom>  set the (maximum) geometry to <geom>,\
     \n\t (the default geometry is %S." !apgv_geometry);
  ("-g", Arg.String set_geom,
   "<geom>  same as -geometry <geom>.");
  version_spec "-v";
  version_spec "--version";
  ] in

List.iter (fun (nm, act, man) -> Options.add nm act man) spec_list
;;

let run_apg fname =
  let prog = Apg.load_apg fname in
  Apg_run.run prog;
  let rec wait () =
    let k = read_key () in
    match k with
    | ' ' | '' | 'q' -> ()
    | _ -> wait () in
  wait ()
;;

let apgv_files = ref [];;

let set_apgv_filename s = apgv_files := s :: !apgv_files;;

let set_apgv_geometry s = apgv_geometry := s;;

let usage_msg =
  Printf.sprintf "usage: %s [options] apgfile" Sys.argv.(0)
;;

let sort_apgv_options () =
  let sort = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) in
  sort (Options.all ())
;;

let apgv_options = sort_apgv_options ();;

let init_arguments () =
  Arg.parse apgv_options set_apgv_filename usage_msg
;;

let main () =
  init_arguments ();
  apgv_files := List.rev !apgv_files;
  try List.iter run_apg !apgv_files
  with
  | Sys_error s | Failure s | Graphics.Graphic_failure s ->
      eprintf "Fatal error when running: %s@." s;
      eprintf "%s@.Try %s -help for more information@."
        usage_msg Sys.argv.(0);
      exit 1
;;

let () = main ();;
