(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

open Format;;

(*** Parsing command-line arguments ***)

let crop_flag = ref true;;

let hmargin = ref (Dimension.Cm 1.0);;
let vmargin = ref (Dimension.Cm 1.0);;
let geometry = ref "864x864";;

let set_geom g =
  Dviview.set_autoresize false;
  Dviview.set_autoscale false;
  geometry := g;;

let set_dimen r s = r := Dimension.dimen_of_string s;;

let print_advi_version () =
  prerr_endline
   (Printf.sprintf
      "The Active-DVI previewer and graphics presenter, version %s"
      Config.advi_version_number);
  exit 0;;

let print_advi_full_version () =
  prerr_endline
   (Printf.sprintf
      "The Active-DVI previewer and graphics presenter, version %s"
      Config.advi_full_version);
  exit 0;;

let version_spec = function
  | "-v" as opt ->
       opt, Arg.Unit print_advi_version,
       "  print the Active-DVI version."
  | opt ->
       opt, Arg.Unit print_advi_full_version,
       "  print the full Active-DVI version, sub-version and release date.";;

let spec_list = [
  ("-geometry", Arg.String set_geom,
   Printf.sprintf
     "<geom>  set the (maximum) geometry to <geom>,\
     \n\t (the default geometry is %S." !geometry);
  ("-g", Arg.String set_geom,
   "<geom>  same as -geometry <geom>.");
  ("-crop", Arg.Set crop_flag,
   "  crop the window to the best size,\
   \n\t (this is the default).");
  ("-nocrop", Arg.Clear crop_flag,
   "  disable the default cropping behaviour.");
  ("-nomargins", Arg.Unit
     (fun () -> set_dimen hmargin "0cm"; set_dimen vmargin "0cm"),
   "  suppress the horizontal and vertical margins,\
   \n\t (the default is respectively 1cm and 1cm).");
  ("-hmargin", Arg.String (set_dimen hmargin),
   "<dimen>  set the horizontal margin\
   \n\t (the default is 1cm).");
  ("-vmargin", Arg.String (set_dimen vmargin),
   "<dimen>  set the vertical margin\
   \n\t (the default is 1cm).");
  version_spec "-v";
  version_spec "--version";
  ] in

List.iter (fun (nm, act, man) -> Options.add nm act man) spec_list;;

let usage_msg =
  Printf.sprintf "usage: %s [options] dvifile" Sys.argv.(0);;

let sort_advi_options () =
  let sort = List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) in
  sort (Options.all ());;

let get_advi_options () =
 (* We must add an option that uses the list of options we are defining.
    We use a reference to break the cycle. *)
 let advi_options = ref [] in
 Options.add
   "-options-file"
   (Arg.String
      (fun fname ->
        Userfile.load_options_file
          !advi_options Userfile.set_dvi_filename usage_msg fname))
     "<file>  load <file>, when parsing this option, to set up\
     \n\t options and override the default ones set in ~/.advirc\
     \n\t or ~/.advi/advirc init files.";
 advi_options := sort_advi_options ();
 !advi_options;;

let advi_options = get_advi_options ();;

let init_arguments () =
  Userfile.load_init_files advi_options Userfile.set_dvi_filename usage_msg;
  Arg.parse advi_options Userfile.set_dvi_filename usage_msg;;

let set_dvi_geometry () =
  Dviview.set_crop !crop_flag;
  Dviview.set_hmargin !hmargin;
  Dviview.set_vmargin !vmargin;
  Dviview.set_geometry !geometry;;

let treat_files master clients =
  Rc.init ();
  set_dvi_geometry ();
  try Dviview.main_loop master clients with
  | Dviview.Error s | Sys_error s
  | Failure s | Graphics.Graphic_failure s ->
      eprintf "Fatal error when running: %s@." s;;

let standalone_main () =
  init_arguments ();
  (* Find the file to view. *)
  let master, clients  =
    match Userfile.get_dvi_filenames () with
    | [] -> Config.splash_screen, []
    | master :: clients -> master, clients in
  (* Test if file can be found, otherwise print console stuff. *)
  begin try let ic = open_in master in close_in ic with
  | Sys_error s ->
      eprintf "%s@.Try %s -help for more information@." s Sys.argv.(0);
      Launch.exit 1
  end;
  (* Let's go! *)
  treat_files master clients;;

let interactive_main () =
  (* Load the .advirc file ... *)
  Userfile.load_init_files advi_options Userfile.set_dvi_filename usage_msg;
  (* Find the file to view. *)
  let master, clients =
    match Userfile.get_dvi_filenames () with
    | master :: clients -> master, clients
    | [] ->
       let master =
         Format.printf "Dvi file name: @?";
         input_line stdin in
       try let ic = open_in master in close_in ic; master, [] with
       | Sys_error s ->
           eprintf "%s@.Try %s -help for more information@." s Sys.argv.(0);
           Config.splash_screen, [] in
  Userfile.set_dvi_filename master;
  (* Let's go! *)
  treat_files master clients;;

(* To quit nicely, killing all embedded processes. *)
at_exit Gs.kill;;
at_exit Embed.kill_all_embedded_apps;;
(* Even in case of signal, we must kill embedded processes. *)
Sys.set_signal Sys.sigquit (Sys.Signal_handle (fun _ -> Launch.exit 0));;

let main = if !Sys.interactive then interactive_main else standalone_main;;

Unix.putenv "GHOSTVIEW" "Test";;

Printexc.catch (Misc.handle_fatal_error main) ();;
