(*
 * Generic Transformers: Camlp5 syntax extension.
 * Copyright (C) 2016-2021
 *   Dmitry Boulytchev, Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Ploc

let oops loc str = Ploc.raise loc (Failure str)
let get_val loc = function
| VaVal x -> x
| _       -> failwith "could not get VaVal _ (should not happen)"


module Migr =
  Ppxlib_ast__Versions.Convert(Ppxlib_ast__Versions.OCaml_current)(Ppxlib.Import_for_core.Js)

open GTCommon


let generate_str is_nonrec tdecls loc =
  let info = snd @@ List.hd @@ List.rev tdecls in
  let module H = Expander.Make(Camlp5Helpers) in
  (* Expander.notify "with annotations %s" (String.concat "," info); *)
  let generator_f si =
    H.str_type_decl_many_plugins ~loc si
      (List.map (fun s -> (s,Expander.Use []) ) info)
  in
  let out =
    let sis = <:str_item< type $list:(List.map fst tdecls)$ >>  in
    let caml_ast = Ast2pt.implem "harcoded_filename.ml" [sis] in
    let () = assert (List.length caml_ast = 1) in
    match (List.hd caml_ast).pstr_desc with
    | Pstr_type (flg, tds) ->
       let tds = List.map Migr.copy_type_declaration tds in
       generator_f [sis] ((if is_nonrec then Ppxlib.Nonrecursive else Recursive), tds)
    |  _ -> failwith "type declaration expected"
  in

  <:str_item< declare $list:out$ end >>

let generate_sig is_nonrec tdecls loc =
  let info = snd @@ List.hd @@ List.rev tdecls in
  (* Expander.notify "with annotations %s" (String.concat "," info); *)
  let module H = Expander.Make(Camlp5Helpers) in
  let generator_f si =
    H.sig_type_decl_many_plugins ~loc si
      (List.map (fun s -> (s,Expander.Use []) ) info)
  in

  let out =
     let ts = List.map fst tdecls in
     let sis = <:sig_item< type $list:ts$ >> in
     let caml_ast = Ast2pt.interf "harcoded_filename.mli" [sis] in
     assert (List.length caml_ast  =  1);
     match (List.hd caml_ast).psig_desc with
     | Psig_type (flg, tds) ->
      let tds = List.map Migr.copy_type_declaration tds in
       generator_f [sis] ((if is_nonrec then Ppxlib.Nonrecursive else Recursive), tds)
     | _ -> assert false
  in

  <:sig_item< declare $list:out$ end >>
