open Parsetree
open Ast_mapper
open Ast_helper

module Embed = struct
  (* e [@embed e'] => e' *)

  open Parsetree

  let extend super = 
    let expr self e = match e.pexp_attributes with
      | [ {txt="typpx_embed"}, PStr [ { pstr_desc= Pstr_eval (e, []) } ] ] -> e
      | _ -> super.expr self e
    in
    { super with expr }

  let mapper = extend Ast_mapper.default_mapper

end

module F(A : sig
  val tool_name : string
  val args : (Arg.key * Arg.spec * Arg.doc) list
  val firstUntypedTransformation : mapper
  module Typemod : S.Typemod
  module TypedTransformation : S.TypedTransformation
  val lastUntypedTransformation : mapper
end) = struct

  open A
      
  module Compile = Compile.Make(Typemod)(TypedTransformation)
    
  let dump_first = ref None
  let dump_untype = ref None

  let dump path ast = match path with
    | None -> ()
    | Some path ->
        let oc = open_out path in
        let ppf = Format.formatter_of_out_channel oc in
        begin match ast with
        | `Str str -> Pprintast.structure ppf str
        | `Sig sg -> Pprintast.signature ppf sg
        end;
        Format.pp_print_newline ppf ();
        close_out oc

  let dump_str path str = dump path (`Str str); str
  let dump_sig path sg  = dump path (`Sig sg);  sg
    
  let rev_ppxs = ref []
  let add_ppx s = rev_ppxs := s :: !rev_ppxs

  (* Unfortunately OCaml 4.04.0 does not export -bin-annot and -annot options
     of the parent tool. We force -bin-annot on.

     In addition, ppx does not know the destination paths, 
     [sourcefile] and [outputprefix] for Compile.implementation/interface.
  *)

  let get_source_of_str = function
    | [] -> None
    | s::_ -> Some s.pstr_loc.Location.loc_start.Lexing.pos_fname

  let get_source_of_sg = function
    | [] -> None
    | s::_ -> Some s.psig_loc.Location.loc_start.Lexing.pos_fname

  let debug () = if Debug.debug then begin
    Format.eprintf "tool_name: %s@." (Ast_mapper.tool_name ());
    Format.eprintf "@[<2>include_dirs: @[<v>%a@]@]@." (Ppxx.Utils.List.format "@ " Format.pp_print_string) !Clflags.include_dirs
  end
      
  (* The PPX mapper *)

  let make_mapper () =
    debug ();
    match Ast_mapper.tool_name () with
    | "ocamldep" ->
        (* If the tool is ocamldep, we CANNOT type-check *)
        firstUntypedTransformation
    | tool_name ->
        Clflags.all_ppx := List.rev !rev_ppxs;
        let structure str =
          let sname = match get_source_of_str str with
            | Some n -> Clflags.binary_annotations := true; n
            | None -> "no file info.xxx"
          in
          Clflags.dont_write_files := true;
          Warnings.parse_options false "a"; (* print warning *)
          Warnings.parse_options true  "a"; (* warning as error *)
          firstUntypedTransformation.structure firstUntypedTransformation str
          |> dump_str !dump_first 
          |> Compile.implementation Format.err_formatter sname (Filename.remove_extension sname)
          |> dump_str !dump_untype
          |> Embed.mapper.structure Embed.mapper
          |> lastUntypedTransformation.structure lastUntypedTransformation
          |> Pparse.apply_rewriters_str ~tool_name
        in
        let signature sg =
          let sname = match get_source_of_sg sg with
            | Some n -> Clflags.binary_annotations := true; n 
            | None -> "no file info.xxx"
          in
          Clflags.dont_write_files := true;
          Warnings.parse_options false "a"; (* print warning *)
          Warnings.parse_options true  "a"; (* warning as error *)
          firstUntypedTransformation.signature firstUntypedTransformation sg
          |> dump_sig !dump_first
          |> Compile.interface Format.err_formatter sname (Filename.remove_extension sname)
          |> dump_sig !dump_untype
          |> Embed.mapper.signature Embed.mapper
          |> lastUntypedTransformation.signature lastUntypedTransformation
          |> Pparse.apply_rewriters_sig ~tool_name
        in
        let warn_and_error_of_error = function
          | Location.Error e ->
              Some (Ast_mapper.attribute_of_warning e.Location.loc e.Location.msg,
                    Ast_mapper.extension_of_error e)
          | exn ->
             match Location.error_of_exn exn with
             | Some (`Ok e) -> 
                 Some (Ast_mapper.attribute_of_warning e.Location.loc e.Location.msg,
                       Ast_mapper.extension_of_error e)
             | Some `Already_displayed -> None
             | None ->
                let loc = Location.in_file !Location.input_name in
                let e = Location.errorf ~loc "Uncaught exception: %s" (Printexc.to_string exn) in
                Some (Ast_mapper.attribute_of_warning e.Location.loc e.Location.msg,
                      Ast_mapper.extension_of_error e)
        in
        let structure _x str =
          try structure str with
          | e ->
              match warn_and_error_of_error e with
              | Some (w, e) -> 
                  Str.attribute w :: str @ [ Str.extension e ]
              | None -> str
        in
        let signature _x sg =
          try signature sg with
          | e ->
              match warn_and_error_of_error e with
              | Some (w, e) ->
                  Sig.attribute w :: sg @ [ Sig.extension e ]
              | None -> sg
        in
        { default_mapper with structure; signature }

  let opts =
    let set_string_opt r = Arg.String (fun s -> r := Some s) in
    [( "-typpx-dump-first", set_string_opt dump_first, "<path>: (TyPPX) Dump the result of the first untyped transformation stage" );
     ( "-typpx-dump-untype", set_string_opt dump_untype, "<path>: (TyPPX) Dump the result of the untype stage" );
     ( "-ppx", Arg.String add_ppx, "<command>: (TyPPX) Run extra PPX preprocessing at the final phase of TyPPX" );
    ]

  (* [make_mapper] is for the latest AST version.
     We cannot use Ppxx.Ppx.Make since it may be for older Ast.
   *)

  open Migrate_parsetree
     
  let name = tool_name
  let options = args @ opts @ Options.compiler_libs_options

  let debug = ref false
  let opt_debug = ( "-debug", Arg.Set debug, "ppx debug mode" )

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
      ~args:options
      Versions.ocaml_current
      (fun _ _ -> make_mapper ())
  
  let legacy_main () =
    Driver.register
      ~name
      ~args:(opt_debug :: options)
      Versions.ocaml_current
      (fun _ _ -> make_mapper ());
    if check_debug () then Migrate_parsetree.Driver.run_main ()
    else Driver.run_as_ppx_rewriter ()

end
