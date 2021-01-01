(*
 * Generic Transformers: Camlp5 syntax extension.
 * Copyright (C) 2016-2021
 *   Dmitry Boulytchev, Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(* #load "q_MLast.cmo";; *)
(* #load "q_ast.cmo";; *)

open Ploc

let oops loc str = Ploc.raise loc (Failure str)
let get_val loc = function
| VaVal x -> x
| _       -> failwith "could not get VaVal _ (should not happen)"

(* type descr_t =
 *            [ `Poly of
 *               [ `Con of string * ctyp typ list | `Type of ctyp typ ] list
 *           | `Struct of (string * bool * ctyp typ) list
 *           | `Tuple of ctyp typ list
 *           | `Vari of
 *               [ `Con of string * ctyp typ list
 *                | `Tuple of ctyp typ list
 *                | `Type of ctyp typ ]
 *               list ]
 *
 * let tdecl_to_descr loc t =
 *   let name = get_val loc (snd (get_val loc t.tdNam)) in
 *   let args =
 *     List.map (fun (x, _) ->
 *      match get_val loc x with
 * 	   | Some y -> y
 * 	   | None   -> failwith "wildcard type parameters not supported"
 *     )
 *     (get_val loc t.tdPrm)
 *   in
 *   let convert =
 *     let convert_concrete typ =
 *       let rec inner = function
 *       | <:ctyp< ( $list:typs$ ) >> as typ -> Tuple (typ, List.map inner typs)
 *       | <:ctyp< ' $a$ >> as typ -> Variable (typ, a)
 *       | <:ctyp< $t$ $a$ >> as typ ->
 *           (match inner t, inner a with
 *            | _, Arbitrary _ -> Arbitrary typ
 *            | Instance (_, targs, tname), a -> Instance (typ, targs@[a], tname)
 *            | _ -> Arbitrary typ
 *           )
 *       | <:ctyp< $q$ . $t$ >> as typ ->
 *           (match inner q, inner t with
 *           | Instance (_, [], q), Instance (_, [], t) -> Instance (typ, [], q@t)
 *           | _ -> Arbitrary typ
 *           )
 *       | (<:ctyp< $uid:n$ >> | <:ctyp< $lid:n$ >>) as typ -> Instance (typ, [], [n])
 *       | t -> Arbitrary t
 *       in
 *       let rec replace = function
 *       | Tuple (t, typs) -> Tuple (t, List.map replace typs)
 *       | Instance (t, args', qname) as orig when qname = [name] ->
 *          (try
 *            let args' =
 *              List.map (function
 * 	               | Variable (_, a) -> a
 *                  | _ -> invalid_arg "Not a variable"
 *                  )
 *              args'
 *            in
 *            if args' = args then Self (t, args, qname) else orig
 *          with Invalid_argument "Not a variable" -> orig
 *          )
 *       | x -> x
 *       in
 *       replace (inner typ)
 *     in
 *     function
 *     | <:ctyp< [ $list:const$ ] >> | <:ctyp< $_$ == $priv:_$ [ $list:const$ ] >> ->
 *        let const = List.map (fun (loc, name, args, d) ->
 * 	       match d with
 * 			   | None -> `Con (get_val loc name, List.map convert_concrete (get_val loc args))
 * 			   | _    -> oops loc "unsupported constructor declaration"
 *          )
 *          const
 *     	in
 *       `Vari const
 *
 *     | <:ctyp< { $list:fields$ } >> | <:ctyp< $_$ == $priv:_$ { $list:fields$ } >> ->
 *       let fields = List.map (fun (_, name, mut, typ) -> name, mut, convert_concrete typ) fields in
 *       `Struct fields
 *
 *     | <:ctyp< ( $list:typs$ ) >> -> `Tuple (List.map convert_concrete typs)
 *
 *     | <:ctyp< [ = $list:variants$ ] >> ->
 *       let wow () = oops loc "unsupported polymorphic variant type constructor declaration" in
 *       let variants =
 *         List.map (function
 * 	       | <:poly_variant< $typ$ >> ->
 *             (match convert_concrete typ with
 *             | Arbitrary _ -> wow ()
 *             | typ -> `Type typ
 *             )
 * 	       | <:poly_variant< ` $c$ >> -> `Con (c, [])
 * 	       | <:poly_variant< ` $c$ of $list:typs$ >> ->
 *              let typs =
 *                List.flatten (
 *                  List.map (function
 *                  | <:ctyp< ( $list:typs$ ) >> -> List.map convert_concrete typs
 *                  | typ -> [convert_concrete typ]
 *    	              )
 * 		             typs
 *               )
 *              in
 *              `Con (c, typs)
 * 	       | _ -> wow ()
 * 	      )
 * 	    variants
 *       in
 *       `Poly variants
 *
 *     | typ ->
 *        (match convert_concrete typ with
 *         | Arbitrary _ -> oops loc "unsupported type"
 *         | typ         ->
 *           `Vari [
 *             match typ with
 *             | Variable (t, _) -> `Tuple [Tuple (<:ctyp< ($list:[t]$) >>, [typ])]
 *             | _ -> `Type typ
 *           ]
 * 	     )
 *   in
 *   (args, name, convert t.tdDef) *)

module Migr =
  Ppxlib_ast__Versions.Convert(Ppxlib_ast__Versions.OCaml_current)(Ppxlib.Import_for_core.Js)

open GTCommon


let generate_str tdecls loc =
  let info = snd @@ List.hd @@ List.rev tdecls in
  let module H = Expander.Make(Camlp5Helpers) in
  (* Expander.notify "with annotations %s" (String.concat "," info); *)
  let generator_f si =
    H.str_type_decl_many_plugins ~loc si
      (List.map (fun s -> (s,Expander.Use []) ) info)
  in
  let out =
    let sis = <:str_item< type $list:(List.map fst tdecls)$ >>  in
    let caml_ast = Ast2pt.implem "filename.here" [sis] in
    let () = assert (List.length caml_ast = 1) in
    match (List.hd caml_ast).pstr_desc with
    | Pstr_type (flg, tds) ->
       let tds = List.map Migr.copy_type_declaration tds in
       generator_f [sis] (Recursive, tds)
    |  _ -> failwith "type declaration expected"
  in

  <:str_item< declare $list:out$ end >>

let generate_sig tdecls loc =
  let info = snd @@ List.hd @@ List.rev tdecls in
  (* Expander.notify "with annotations %s" (String.concat "," info); *)
  let module H = Expander.Make(Camlp5Helpers) in
  let generator_f si =
    H.sig_type_decl_many_plugins ~loc si
      (List.map (fun s -> (s,Expander.Use []) ) info)
  in

  let out =
     let ts = List.map fst tdecls in
     let sis = <:sig_item< type $list:ts$ >>  in
     let caml_ast = Ast2pt.interf "asdf" [sis] in
     assert (List.length caml_ast  =  1);
     match (List.hd caml_ast).psig_desc with
     | Psig_type (flg, tds) ->
      let tds = List.map Migr.copy_type_declaration tds in
       generator_f [sis] (Recursive, tds)
     | _ -> assert false
  in

  <:sig_item< declare $list:out$ end >>
