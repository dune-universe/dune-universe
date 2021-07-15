(* Time-stamp: <modified the 10/07/2018 (at 10:42) by Erwan Jahier> *)

(* generate ocaml glue code that makes it possible to call lus2lic
   from ocaml with the current set of arguments (with Lv6Run.make).
*)
open Lv6MainArgs

let (f: string array -> Lv6MainArgs.t -> unit) =
  fun argv opt -> 
    let outfile = if opt.outfile <> "" then opt.outfile else
        let file = List.hd opt.infiles in
        try (Filename.chop_extension (Filename.basename file))^ ".ml"
        with _ -> 
		    print_string ("*** Error: '"^file^"'is a bad file name.\n"); exit 2
    in
    let cma_file = (Filename.chop_extension outfile) ^".cma" in
    let remove_me = ["-exec"; "-ocaml";"-o";opt.outfile] in
    let args = 
      Array.fold_right (fun x acc -> if List.mem x remove_me then acc else x::acc) argv []
    in
    let args_str = "\"" ^ (String.concat "\";\"" args) ^"\"" in
    let oc = open_out (outfile)  in
    Lv6util.entete oc "(*" "*)";
    Printf.fprintf oc "
let plugin = 
  let args = Array.of_list [%s] in
  Lv6Run.make args

let dyn_file = (Dynlink.adapt_filename \"%s\")
let _ = 
  OcamlRM.reg_plugin dyn_file plugin
" args_str cma_file
