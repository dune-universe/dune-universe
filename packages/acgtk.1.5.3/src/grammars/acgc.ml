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

module Actual_env = AcgData.Environment.Environment
module Sg=Actual_env.Signature1
module Actual_parser = Grammars.Parsers
                     
(** This module correspond to the main funtion of the ACG compiler and
its command line interface. *)

type return_status =
  | Failure
  | Success of ((string option) * Actual_env.t) (* The pair is meant
                                                   to keep the name of
                                                   the current parsing
                                                   file, and the build
                                                   environment *)
                     
let resize_terminal () =
  let () = Utils.sterm_set_size () in
  Utils.term_set_size ()

  
let parse_term sg =
  let t = ref None in
  let rec parse_rec = function
    | true ->
       let () = Printf.printf "Enter a term: " in
       let term_string = read_line () in
       (match Actual_parser.parse_term ~color:true term_string sg with
       | None -> parse_rec true
       | Some ta -> let () = t:= (Some ta) in false )
    | false -> false in
  let () =
    while (parse_rec true) do
      ()
    done in
  match !t with
  | Some u -> u
  | _ -> failwith "Strange..."
     
type file_type = Source | Object | Neither
    
let parse filename dirs status =
  match status with
  | Failure -> Failure
  | Success (name,env) ->
     let () = Logs.debug
                (fun m -> m
                            "The environment currently has %d signature(s) and %d lexicon(s)."
                            (Actual_env.sig_number env)
                            (Actual_env.lex_number env)) in
     let file_type =
       match Filename.check_suffix filename ".acg" with
       | true -> Source
       | false ->
	  (match Filename.check_suffix filename ".acgo" with
	   | true -> Object
	   | false -> Neither) in
     match file_type with
     | Neither ->
	let () = Logs.err
                   (fun m -> m
                               "File name's suffixes should be \".acg\" or \".acgo\". The name \"%s\" has not this suffix.\n"
                               filename) in
	Failure
     | Source ->
	let basename=Filename.basename filename in
	let name_wo_suffix = Filename.chop_suffix basename ".acg" in
	begin
	  match Actual_parser.parse_data filename dirs env with
	  | None -> Failure
	  | Some e -> Success (Some (Printf.sprintf "%s.acgo" name_wo_suffix),e)
	end
     | Object ->
	let new_env = Actual_env.read filename dirs in
	match new_env with
	| None -> Failure
	| Some n_e -> Success (name,Actual_env.append env n_e)

let parse_files status dirs files =
  List.fold_left
    (fun acc file -> parse file dirs acc)
    status
    files
                    
	   
let term_parsing i env =
  if not i then
    ()
  else
    let n = Actual_env.sig_number env in
    let m = Actual_env.lex_number env in
    let available_data =
      Utils.string_of_list
	"\n"
	(fun x -> x)
	(Actual_env.fold
	   (fun d a -> 
	     match d with
	     | Actual_env.Signature sg -> (Printf.sprintf "\tSignature\t%s" (fst (Actual_env.Signature1.name sg)))::a
	     | Actual_env.Lexicon lx -> (Printf.sprintf "\tLexicon\t\t%s" (fst (Actual_env.Lexicon.name lx)))::a)
	   []
	   env) in
    let chosen_sig=Actual_env.choose_signature env in
    let chosen_sig_name_loaded =
      match chosen_sig with
      | None -> ""
      | Some s -> Printf.sprintf "Signature \"%s\" loaded." (fst (Sg.name s))  in
    if (n+m=0) || (not i)
    then
      ()
    else
      try
	let () = if (n=1)&&(m=0) then Printf.printf "%s\n" chosen_sig_name_loaded else () in
	while true do
	  try
	    let () = Printf.printf "Available data:\n%s\n" available_data in
	    let entry =
	      match n,chosen_sig with
	      | 1, Some s -> Actual_env.Signature s
	      | _,_ -> 
		 let () = Printf.printf "Enter a name: " in
		 let sig_string = read_line () in 
		 Actual_env.get sig_string env in
	    match entry with
	    | Actual_env.Signature sg -> ignore (parse_term sg)
	    | Actual_env.Lexicon lex -> 
	       let abs,obj=Actual_env.Lexicon.get_sig lex in
	       let t,ty = parse_term abs in
	       let t',ty'=Actual_env.Lexicon.interpret t ty lex in
	       Printf.printf
		 "Interpreted as:\n%s : %s\n"
		 (Actual_env.Signature1.term_to_string t' obj)
		 (Actual_env.Signature1.type_to_string ty' obj)
	  with
	  | Actual_env.Signature_not_found sig_name -> Printf.printf "No such signature in %s\n" sig_name
	done
      with
      | End_of_file -> let () = print_newline () in ()
						 
						 
let output_env ?output_file name env =
  let actual_output_file =
    match output_file with
    | None -> name
    | Some f -> f in
  let () = Actual_env.write actual_output_file env in
  Logs.app (fun m -> m "Output written on: \"%s\"\n%!" actual_output_file)


let main_function interactive output_file dirs files =
  let () = resize_terminal () in
  let dirs =
    match dirs with
    | [""] -> dirs
    | _ -> ("")::dirs in
  let return_status = parse_files (Success (None,Actual_env.empty)) dirs files in
  match return_status with
  | Failure -> 
     `Error (false,"No output file was generated.")
  | Success (name,env) -> 
     match name with
     | None -> 
	let () = Printf.fprintf stderr "No ouput file is produced\nPlease specify an output file.\n%!"
	in `Ok 0
     | Some n ->
        let () =
          Logs.debug (fun m -> m "The environment currently has %d signature(s) and %d lexicon(s)."  (Actual_env.sig_number env) (Actual_env.lex_number env)) in
	let () = output_env ?output_file:output_file n env in
	let () = term_parsing interactive env in
	`Ok 0

let main =
  let doc = "Compile ACG files into a binary representation." in
  let output_file = 
    let doc = "Outputs the result of the command in $(docv) instead of printing it on the standard output." in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc) in
  let dirs =
    let doc = "Sets $(docv) as a directory in which file arguments can be looked for." in
    Arg.(value & opt_all dir [""] & info ["I";"include"] ~docv:"DIR" ~doc) in
  let interactive =
    let doc = "Enters the interaction loop to parse terms according to signatures." in
    Arg.(value & flag & info ["i";"interactive"] ~doc) in
  let man = [
      `S Manpage.s_description ;
      `P "$(tname) parses each file $(i,FILE), which is supposed to be a file containing ACG signatures or lexicons, either as source files (typically with the .acg extension) or object files (with the .acgo extension).";
      `P "If all the files are successfully parsed, a binary containing all the ACG signatures and lexicons is created. By default, the name of the generated binay file is \"$(b,FILE)n.acgo\" where $(b,FILE)n.acg is the last parameter (see option -o)." ;
      `P "Files should have \".acg\" or \".acgo\" as suffix.";
      `S Manpage.s_bugs;
      `P "Report bugs by submitting issues at https://gitlab.inria.fr/ACG/dev/ACGtk/issues.";
      `P "Or report bugs to <sylvain.pogodalla@inria.fr>.";
    ] in
  let files = Arg.(non_empty & pos_all string [] & info [] ~docv:"FILE") in
  let exits = Term.((exit_info ~doc:"on failure (at least one of the argument files $(b,FILE) does not compile)." 1)::default_exits) in
  Term.(ret (const main_function $ interactive $ output_file $ dirs $ files)),
  Term.info "acgc" ~version:Version.version ~doc ~exits ~man
	  
let () =
  let () = Log.set_level ~app:"acgc" Logs.Warning in 
  (*  let () = Log.set_level ~app:"acgc" Logs.Debug in  *)
  Term.(exit @@ eval main)
    

          
