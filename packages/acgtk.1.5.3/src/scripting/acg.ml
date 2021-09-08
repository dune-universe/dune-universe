(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2021 INRIA                             *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner
open UtilsLib
open Scripting
open Functions

let () = Sys.catch_break true
               
module P = Parse_functions

module F = Functions


let welcome_string = "Welcome to the ACG toplevel"
let version_string = Printf.sprintf "Version %s" Version.version
let copyright_string = "©INRIA 2008-2021"
let bug_string = "Please send your comments or bug reports or feature requests to sylvain.pogodalla@inria.fr"

let resize_terminal pp_output =
  if pp_output then
    let () = Utils.sterm_set_size () in
    Utils.term_set_size ()
  else
    ()

let welcome_msg pp_output =
  let () = resize_terminal pp_output in
  let l = 
    let l = Format.get_margin () in
    if l > 1000 then
      ((String.length bug_string)+10)
    else
      l in
  let _ = Format.flush_str_formatter () in
  let () = Format.printf "@[<v>" in
  let () = 
    List.iter
      (fun s -> 
	let prepend =
	  try
	    (String.make ((l-(String.length s))/2) ' ')
	  with
	  | Invalid_argument _ -> "" in
	Format.printf "@[<h>%s%s@]@," prepend s)
      [welcome_string;version_string;copyright_string;bug_string] in
  Format.printf "@,@[<v>@[<v15>Type@,help;@]@,@[to@ get@ help.@]@]@,@]@.@?"
    
let rec repeat f (ctx,env) =
  match f (ctx,env) with
  | P.Continue (c',e') -> repeat f (c',e')
  | P.Stop _ -> ()
  | exception F.Stop -> ()
  | exception F.Quit -> ()
  | exception Sys.Break -> ()
  | exception End_of_file -> ()

  
let parse_files dirs svg_output no_color no_pp _no_svg svg_config filenames =
  let () = Log.set_level ~app:"acg" ~colored:(not no_color) Logs.Warning in
  let dirs =
    match dirs with
    | [""] -> dirs
    | _ -> ("")::dirs in
  let rendering_config =
    match svg_config with
    | None -> Rendering_config.default
    | Some conf_file -> Rendering_config.get_config conf_file dirs in
  let ctx = F.make_context ~script_filename:None ~wait:false ~colored_output:(not no_color) ~pretty_printed_output:(not no_pp) ~echo:true ~svg:svg_output ~dirs:dirs ~rendering_config ~parse_fun:P.parse_file in
  let env = AcgData.Environment.Environment.empty in
  (* ANSITerminal get the size info from stdin, In case of redirection,
     the latter may not be set. That's why it is first duplicated and
     stdin is then duplicated from stdout *)
  let stdin_tmp=Unix.dup Unix.stdin in
  let stdin_tmp_in_ch = Unix.in_channel_of_descr stdin_tmp in
  let () = Unix.dup2 Unix.stdout Unix.stdin in
  let () = welcome_msg (not no_pp) in
  try
    let ctx',env' =
      List.fold_left
        (fun (c,e) filename -> P.parse_file filename c e)
        (ctx,env)
        filenames in
    repeat (fun (c,e) -> P.parse_entry ~resize:(F.resize ctx') stdin_tmp_in_ch c e) (ctx',env')
  with
  | F.Stop -> ()
                 
let parse_files_t =
  let doc = "Interactive ACG command interpreter. Also parse and interpret files given on the command line (if any)." in
  let dirs =
    let doc = "Sets $(docv) as a directory in which file arguments can be looked for." in
    Arg.(value & opt_all dir [""] & info ["I";"include"] ~docv:"DIR" ~doc) in
  let svg_output = 
    let doc = "Set the file name of the svg output of the $(i,realize) command to $(docv)." in
    Arg.(value & opt (some string) None & info ["svg"] ~docv:"FILE" ~doc) in
  let no_color =
    let doc = "Toggle off coloring the output." in
    Arg.(value & flag & info ["nc";"no-color"] ~doc) in
  let no_pp =
    let doc = "Toggle off pretty printing the output." in
    Arg.(value & flag & info ["npp";"no-pretty-printing"] ~doc) in
  let no_svg =
    let doc = "Toggle off svg output when running the $(i,realize) command." in
    Arg.(value & flag & info ["nsvg";"no-svg"] ~doc) in
  let svg_config =
    let doc = "Sets the json config rendering file for the svg generated (by the $(i,realize) command) files to $(docv)." in
    Arg.(value & opt (some string) None & info ["realize"] ~docv:"FILE" ~doc) in
  let man = [
      `S Manpage.s_description ;
      `P "$(tname) parses each file $(i,FILE) (if any), which is supposed to be a file containing ACG commands, and interpret them. Then interactively run the ACG command interpreter.";
      `P "A list of the available commands is available by running the \"help;\" command in the interpreter." ;
      `S Manpage.s_bugs;
      `P "Report bugs by submitting issues at https://gitlab.inria.fr/ACG/dev/ACGtk/issues.";
      `P "Or report bugs to <sylvain.pogodalla@inria.fr>.";
    ] in
  let files = Arg.(value & pos_all string [] & info [] ~docv:"FILE") in
  Term.(const parse_files $ dirs $ svg_output $ no_color $ no_pp $ no_svg $ svg_config $ files),
  Term.info "acg" ~version:Version.version ~doc ~exits:Term.default_exits  ~man

let () =
  Term.(exit @@ eval parse_files_t)
