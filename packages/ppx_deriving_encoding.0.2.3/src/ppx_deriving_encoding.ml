open Ppxlib
open Ast_builder.Default
open Utils

module type S = S

let str_gen ~loc ~path:_ (rec_flag, l)
    enum ign mu force_debug rm_prefix option title description schema name modu =
  if fake then []
  else (
    wrap modu;
    let l = List.map (fun t ->
        let enc = Encoding.expressions ~loc ~enum ~ign ~mu ?rm_prefix
            ?option ?title ?description ?schema t in
        let enc_name = match name with
          | None -> enc_name ~search:false t.ptype_name.txt
          | Some n -> add_enc_name t.ptype_name.txt n; n in
        let params = List.map fst t.ptype_params in
        let expr = add_params_fun ~loc enc params in
        let typ =
          ptyp_constr ~loc (llid ~loc @@ enc_mod "encoding") [
            ptyp_constr ~loc (llid ~loc t.ptype_name.txt) params ] in
        let typ = add_params_fun_sig ~loc typ params in
        value_binding ~loc ~pat:(ppat_constraint ~loc (pvar ~loc enc_name) typ)
          ~expr) l in
    let rec_flag = if List.length l < 2 then Nonrecursive else rec_flag in
    let s = [ pstr_value ~loc rec_flag l ] in
    debug ~force:force_debug "%s\n" (str_of_structure s);
    unwrap ();
    s)

let sig_gen ~loc ~path:_ (_rec_flag, l) name modu =
  if fake then []
  else (
    wrap modu;
    let l = List.map (fun t ->
        let name = match name with None -> enc_name ~search:false t.ptype_name.txt | Some n -> n in
        let params = List.map fst t.ptype_params in
        let typ = add_params_fun_sig ~loc
            (ptyp_constr ~loc (llid ~loc @@ enc_mod "encoding") [
                ptyp_constr ~loc (llid ~loc t.ptype_name.txt) params ]) params in
        value_description ~loc ~name:{txt=name; loc} ~type_:typ ~prim:[]) l in
    let s = List.map (psig_value ~loc) l in
    debug "%s\n" (str_of_signature s);
    unwrap ();
    s)

let eprefix t =
  let f = Deriving.Args.to_func t in
  Deriving.Args.of_func (fun ctx loc x k ->
      match rm_prefix_of_expr x with
      | None -> raise_error ~loc "wrong boolean argument"
      | Some x -> f ctx loc x k)

let () =
  let args_str = Deriving.Args.(
      empty
      +> flag "enum"
      +> flag "ignore"
      +> flag "recursive"
      +> flag "debug"
      +> arg "remove_prefix" (eprefix __)
      +> arg "option" (estring __)
      +> arg "title"  __
      +> arg "description" __
      +> arg "schema" __
      +> arg "name" (estring __)
      +> arg "module_name" (estring __)
    ) in
  let args_sig = Deriving.Args.(
      empty
      +> arg "name" (estring __)
      +> arg "module_name" (estring __)
    ) in
  let str_type_decl = Deriving.Generator.make args_str str_gen in
  let sig_type_decl = Deriving.Generator.make args_sig sig_gen in
  Deriving.ignore @@ Deriving.add "encoding" ~str_type_decl ~sig_type_decl;
  Deriving.ignore @@ Deriving.add "json_encoding" ~str_type_decl ~sig_type_decl;
