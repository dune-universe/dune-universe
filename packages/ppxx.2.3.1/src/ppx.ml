(* PPX related tools *)

open Utils
open Migrate_parsetree
   
let handle_error f =
  try f () with 
  | Syntaxerr.Error e ->
      !!% "%a@." Syntaxerr.report_error e;
      exit 2
  | e ->
      Format.eprintf "%a@."  Location.report_exception e;
      exit 2
      
module Make(A : sig 
  val name : string
  val options : (Arg.key * Arg.spec * Arg.doc) list
  val make_mapper : Driver.config -> Driver.cookies -> Ast_405.Ast_mapper.mapper
end) = struct
  open A

  let debug = ref false
  let opt_debug = ( "-debug", Arg.Set debug, "ppx debug mode which takes .ml and .mli" )

  (* Some command line argument hack *)
  let check_debug () =
    let options' =
      opt_debug :: List.map (fun (k,_,d) -> k, Arg.Unit (fun () -> ()), d) options
    in
    Arg.parse options' (fun _ -> ())
      (Printf.sprintf "%s [options] <input ast file> <output ast file>" name);
    Arg.current := 0; (* reset for the next Arg.parse *)
    !debug

  let register () = 
    Driver.register
      ~name
      ~args:A.options
      Versions.ocaml_405
      make_mapper
  
  let legacy_main () =
    let debug = check_debug () in
    Driver.register
      ~name
      ~args:(opt_debug :: options)
      Versions.ocaml_405
      make_mapper;
    if debug then Migrate_parsetree.Driver.run_main ()
    else Driver.run_as_ppx_rewriter ()
end
