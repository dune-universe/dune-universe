(* Time-stamp: <modified the 26/03/2014 (at 17:07) by Erwan Jahier> *)

(* generate ocaml glue code that makes it possible to call lutin
   from ocaml with the current set of arguments.
*)
let (f: string array -> MainArg.t -> unit) =
  fun argv opt -> 
    let outfile = 
      match (MainArg.outfile opt) with
        | None -> (
          let file = List.hd (MainArg.infile opt) in
          try (Filename.chop_extension (Filename.basename file))^ ".ml"
          with _ -> 
		      print_string ("*** '"^file^"': bad file name.\n"); exit 2
        )
        | Some f -> f
    in
    let cma_file = (Filename.chop_extension outfile) ^".cma" in
    let remove_me = ["-ocaml"; "-o"; outfile] in
    let args = 
      Array.fold_right (fun x acc -> if List.mem x remove_me then acc else x::acc) argv []
    in
    let args_str = "\"" ^ (String.concat "\";\"" args) ^"\"" in
    let oc = open_out (outfile)  in
    let entete = Util.entete "(*" "*)" in
    Printf.fprintf oc "%s
let plugin = 
  let args = Array.of_list [%s] in
  LutinRun.make args

(* The following is to make it possible for rdbg to call this lutin program.
But of course, one can use it to dynamically call those steps functions from 
any ocaml programs (as rdbg does).
*)
let dyn_file = (Dynlink.adapt_filename \"%s\")
let _ = 
  OcamlRM.reg_plugin dyn_file plugin
" entete args_str cma_file
