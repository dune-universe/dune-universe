open Ppxlib
open Ast_builder.Default
open Utils

let str_gen ~loc ~path:_ (rec_flag, l)
    enum ign mu force rm_prefix option title description schema name =
  let rm_prefix = match rm_prefix with None -> None | Some s -> Some (bool_of_string s) in
  let l = List.map (fun t ->
      let enc = Encoding.expressions ~loc ~enum ~ign ~mu ?rm_prefix
          ?option ?title ?description ?schema t in
      let name = match name with None -> t.ptype_name.txt | Some n -> n in
      let params = List.map fst t.ptype_params in
      let expr = add_params_fun ~loc enc params in
      let typ =
        ptyp_constr ~loc (llid ~loc @@ enc_mod "encoding") [
          ptyp_constr ~loc (llid ~loc name) params ] in
      let typ =
        if not mu then typ
        else ptyp_arrow ~loc Nolabel typ typ in
      let typ = add_params_fun_sig ~loc typ params in
      value_binding ~loc ~pat:(ppat_constraint ~loc (pvar ~loc (enc_name name)) typ)
        ~expr) l in
  let rec_flag = if List.length l < 2 then Nonrecursive else rec_flag in
  let s = [ pstr_value ~loc rec_flag l ] in
  debug ~force "%s\n" (str_of_structure s);
  s

let sig_gen ~loc ~path:_ (_rec_flag, l) name =
  let l = List.map (fun t ->
      let name = match name with None -> t.ptype_name.txt | Some n -> n in
      let params = List.map fst t.ptype_params in
      let typ = add_params_fun_sig ~loc
          (ptyp_constr ~loc (llid ~loc @@ enc_mod "encoding") [
              ptyp_constr ~loc (llid ~loc name) params ]) params in
      value_description ~loc ~name:{txt=enc_name name; loc} ~type_:typ ~prim:[]) l in
  let s = List.map (psig_value ~loc) l in
  debug "%s\n" (str_of_signature s);
  s

let () =
  let args_str = Deriving.Args.(
      empty
      +> flag "enum"
      +> flag "ignore"
      +> flag "recursive"
      +> flag "debug"
      +> arg "remove_prefix" (estring __)
      +> arg "option" (estring __)
      +> arg "title"  __
      +> arg "description" __
      +> arg "schema" __
      +> arg "name" (estring __)
    ) in
  let args_sig = Deriving.Args.(
      empty
      +> arg "name" (estring __)
    ) in
  let str_type_decl = Deriving.Generator.make args_str str_gen in
  let sig_type_decl = Deriving.Generator.make args_sig sig_gen in
  Deriving.ignore @@ Deriving.add "json_encoding" ~str_type_decl ~sig_type_decl
