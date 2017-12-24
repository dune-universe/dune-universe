(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
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

open UtilsLib

module Actual_env = Grammars.Environment.Environment
module Sg=Actual_env.Signature1
    
type return_status = Failure | Success of ((string option) * Actual_env.t) 
(* The pair is meant to keep the name of the current parsing file,
     and the build environment *)
    
module Actual_parser = Grammars.Data_parser.Parser

let resize_terminal () =
  let () = Utils.sterm_set_size () in
  Utils.term_set_size ()

  
let empty_env = Actual_env.empty
let interactive = ref false
let dirs = ref [""]
let output_name = ref None
let return_status = ref (Success (None,empty_env))
  
let initialize name =
  let () = output_name := Some name in
  return_status := Success (!output_name,empty_env)
    
let options =
  [
    ("-version", Arg.Unit (fun () -> Printf.printf "%s\n" Version.version;exit 0), Format.sprintf "@[@,@[<hov 10>@[Prints@ the@ version@ number@]@]@]");
    ("-o", Arg.String initialize, Format.sprintf "@[-o file_name@, @[<hv 9>@[sets@ the@ name@ of@ the@ ouput@ file@ to@ \"file_name\".@ The@ default@ is@ to@ use@ the@ base@ name@ (with@ no@ extension)@ of@ the@ first@ file@ argument@ with@ the@ suffix@ \".acgo\"@]@]@]");
    ("-i", Arg.Set interactive , Format.sprintf "@[@,@[<5>Enters@ the@ interaction@ loop@ to@ parse@ terms@ according@ to@ signatures@]@]");
    ("-I", Arg.String (fun dir -> dirs := (!dirs)@[dir]) , Format.sprintf "@[@[-I dir@, @[<15>@[sets@ dir@ as@ a@ directory@ in@ which@ file@ arguments@ can@ be@ looked@ for@]@]@]@]")
  ]
    
let usg_msg = Format.sprintf "@[usage:\n@[\t%s [options] file1 file2 ...@]@, @[This@ command@ parse@ the@ files@ which@ are@ supposed@ to@ be@ files@ containing@ acg@ signatures@ or@ lexicons@,@ either@ as@ source@ files@ (the_file.acg)@ or@ object@ files@ (the_file.acgo).@ If@ all@ the@ parses@ are@ successful@,@ a@ binary@ containing@ all@ the@ acg@ signatures@ and@ lexicons@ is@ created.@ Its@ default@ name@ is@ \"filen.acgo\"@ where@ filen.acg@ is@ the@ last@ acg@ source@ file@ of@ the@ list@ (see@ option@ -o).@ Files@ should@ have@ a@ suffix@ \".acg\"@ or@ \".acgo\".@]@]@]" Sys.executable_name
  
  
let parse_term sg =
  let t = ref None in
  let rec parse_rec = function
    | true ->
       let () = Printf.printf "Enter a term: " in
       let term_string = read_line () in
       (match Actual_parser.parse_term term_string sg with
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
     let file_type =
       match Filename.check_suffix filename ".acg" with
       | true -> Source
       | false ->
	  (match Filename.check_suffix filename ".acgo" with
	  | true -> Object
	  | false -> Neither) in
     match file_type with
     | Neither ->
	let () = Printf.fprintf stderr "File name's suffixes should be \".acg\" or \".acgo\". The name \"%s\" has not this suffix.\n" filename in
	Failure
     | Source ->
	let basename=Filename.basename filename in
	let name_wo_suffix = Filename.chop_suffix basename ".acg" in
	begin
	  match Actual_parser.parse_data filename dirs env with
	  | None -> Failure
	  | Some e -> Success ((Some (Printf.sprintf "%s.acgo" name_wo_suffix)),e)
	end
     | Object ->
	let new_env = Actual_env.read filename dirs in
	match new_env with
	| None -> Failure
	| Some n_e -> Success (name,Actual_env.append env n_e)
	   
	   
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
						 
						 
let output_env name env =
  let () = Actual_env.write name env in
  Printf.printf "Output written on: \"%s\"\n%!" name
    
    
let main () =
  let () = resize_terminal () in
  let anon_fun file =
    return_status := parse file !dirs !return_status in
  let () = Arg.parse options anon_fun usg_msg in
  match !return_status with
  | Failure -> 1
  | Success (name,env) -> 
     match name with
     | None -> 
	let () = Printf.fprintf stderr "No ouput file is produced\nPlease specify an output file.\n%!"
	in 0
     | Some n ->
	let () = output_env n env in
	let () = term_parsing !interactive env in
	0
	  
	  
let _ = main ()
