open Ocaml_common
open Parsetree

let print_position outx lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d : %s" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    (Lexing.lexeme lexbuf)

let prepare_error err =
  let open Syntaxerr in
  match err with
  | Unclosed (opening_loc, opening, closing_loc, closing) ->
      Location.errorf ~loc:closing_loc
        ~sub:
          [
            Location.msg ~loc:opening_loc "This '%s' might be unmatched" opening;
          ]
        "Syntax error: '%s' expected" closing
  | Expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %s expected." nonterm
  | Not_expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %s not expected." nonterm
  | Applicative_path loc ->
      Location.errorf ~loc
        "Syntax error: applicative paths of the form F(X).t are not supported \
         when the option -no-app-func is set."
  | Variable_in_scope (loc, var) ->
      Location.errorf ~loc
        "In this scoped type, variable %a is reserved for the local type %s."
        Pprintast.tyvar var var
  | Other loc -> Location.errorf ~loc "Syntax error"
  | Ill_formed_ast (loc, s) ->
      Location.errorf ~loc "broken invariant in parsetree: %s" s
  | Invalid_package_type (loc, s) ->
      Location.errorf ~loc "invalid package type: %s" s

let parse_compilo_string st =
  let open Lexing in
  let lexbuf = Lexing.from_string st in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "" };
  try
    let tree = Parse.interface lexbuf in
    tree
  with
  | Syntaxerr.Error e ->
      let e2 = prepare_error e in
      Location.print_report Format.std_formatter e2;
      exit 1
  | Parsing.Parse_error ->
      Printf.fprintf stderr "%a: syntax error in %s \n" print_position lexbuf st;
      exit 1

(*
let rec print_core_desc f = function
    | Ptyp_any -> Format.pp_print_text f "any"
    | Ptyp_var v -> Format.fprintf f "[var %s]" v
    | Ptyp_constr (a,_) -> Format.fprintf f "[constr %a]" Pprintast.longident a.txt
    | Ptyp_tuple al -> Format.pp_print_list ~pp_sep:(fun f () ->Format.pp_print_text f "*") print_core f al
    | Ptyp_arrow (_,t1,t2) -> Format.fprintf f "[%a -> %a]" print_core t1 print_core t2


    | _ -> failwith "not yet implemented print_core"
and print_core f a = print_core_desc f a.ptyp_desc


let print_constr f c =
  Format.fprintf f "%s" c.pcd_name.txt


let print_type_dec f pd =
  Format.fprintf f "type %s =" pd.ptype_name.txt;
  match pd.ptype_kind with
    Ptype_abstract -> Format.fprintf f "abstract"
  | Ptype_variant cl -> Format.pp_print_list ~pp_sep:(fun f () ->Format.pp_print_text f "|") print_constr f cl
  | _ -> failwith "not yet implemented ptype kind"


let print_signature_item_desc f = function
    Psig_value v -> Format.fprintf f "[Val name:'%s' core:'%a']" v.pval_name.Asttypes.txt print_core v.pval_type
  | Psig_type (_,vl) -> Format.pp_print_list print_type_dec f vl
   |  _ -> failwith "not yet implemented"

let print_signature =
  List.iter (fun s ->
      Format.printf "%a@." print_signature_item_desc s.psig_desc)*)

let rec to_compo_type t =
  let open Type in
  match t.ptyp_desc with
  | Ptyp_var v -> Abstract v
  | Ptyp_constr (a, args) ->
      Name (Longident.last a.txt, List.map to_compo_type args)
  | Ptyp_tuple al -> Prod (List.map to_compo_type al)
  | Ptyp_arrow _ ->
      let args, res = uncurry t in
      Fun (args, res)
  | _ -> failwith "Not yet supported"

and uncurry t =
  match t.ptyp_desc with
  | Ptyp_arrow (_, t1, t2) ->
      let tl, f = uncurry t2 in
      (to_compo_type t1 :: tl, f)
  | _ -> ([], to_compo_type t)

let build_constr conslist =
  List.map
    (fun x ->
      let at2 =
        match
          List.filter (fun y -> y.attr_name.txt = "weight") x.pcd_attributes
        with
        | y :: _ -> (
            match y.attr_payload with
            | PStr tstr ->
                let tstr2 = Pprintast.string_of_structure tstr in
                let tstr3 = String.sub tstr2 2 (String.length tstr2 - 2) in
                Some (float_of_string tstr3)
            | _ -> None )
        | _ -> None
      in
      match x.pcd_args with
      | Pcstr_tuple [] -> (x.pcd_name.txt, None, at2)
      | Pcstr_tuple args ->
          (x.pcd_name.txt, Some (Type.Prod (List.map to_compo_type args)), at2)
      | _ -> failwith "Not implemented, and I do not understand Parsetree doc")
    conslist

let build_type_def vl =
  let param_list =
    List.map
      (fun (t, _) ->
        match t.ptyp_desc with
        | Ptyp_var s -> s
        | _ -> failwith "Not yet implemented type params shape")
      vl.ptype_params
  in
  match vl.ptype_kind with
  | Ptype_variant conslist ->
      ((vl.ptype_name.txt, param_list), build_constr conslist)
  | _ -> failwith "Not yet supported type kind"

let to_val_and_def =
  List.fold_left
    (fun (def, value) t ->
      match t.psig_desc with
      | Psig_value v ->
          let args, res = uncurry v.pval_type in
          ( def,
            { Type.name = v.pval_name.txt; intypes = args; outtype = res }
            :: value )
      | Psig_type (_, vl) ->
          let vl2 = List.map build_type_def vl in
          (vl2 @ def, value)
      | _ -> failwith "Not yet supported")
    ([], [])

let parse_string s =
  let v = parse_compilo_string s in
  let def, vl = to_val_and_def v in
  match vl with t :: _ -> (def, t) | _ -> failwith "No function defined"

let parse_typedef s =
  let v = parse_compilo_string s in
  let def, _ = to_val_and_def v in
  match def with
  | t :: _ -> t
  | _ -> failwith "Require exactly one type definition"

(*
open Type_lib

let _ =
  let v = parse_compilo_string Sys.argv.(1) in
  let (def,vl) = to_val_and_def v in
  List.iter Recursive_type_gen.evaluate def;
  List.iter (fun x -> print_endline (Type.string_of_sum x)) def; 
  List.iter (Type.print_func stdout) vl
*)
