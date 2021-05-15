(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
 
open Ppxlib

let raise_errorf = Location.raise_errorf

let rec module_expr_desc_of_type_desc = function
  | Pmty_ident ident ->
      Pmod_ident ident
  | Pmty_signature items ->
      Pmod_structure (structure_of_signature items)
  | Pmty_functor (a, b) ->
      Pmod_functor (a, module_expr_of_type b)
  | Pmty_with (_module_type, _with_constraints) ->
      raise_errorf "ppx_gen_rec: Pmty_with not supported yet"
  | Pmty_typeof _module_expr ->
      raise_errorf "ppx_gen_rec: Pmty_typeof not supported yet"
  | Pmty_extension extension ->
      Pmod_extension extension
  | Pmty_alias _ident ->
      raise_errorf "ppx_gen_rec: Pmty_alias not supported yet"

and module_expr_of_type { pmty_desc; pmty_loc; pmty_attributes; } =
  {
    pmod_desc = module_expr_desc_of_type_desc pmty_desc;
    pmod_loc = pmty_loc;
    pmod_attributes = pmty_attributes;
  }

and module_binding_of_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc; } =
  {
    pmb_name = pmd_name;
    pmb_expr = module_expr_of_type pmd_type;
    pmb_attributes = pmd_attributes;
    pmb_loc = pmd_loc;
  }

and open_declaration_of_open_description desc =
    let ident_loc = desc.popen_expr in
    let module_expr = {
      pmod_attributes=[];
      pmod_loc=ident_loc.loc;
      pmod_desc=Pmod_ident ident_loc
    } in
    { desc with popen_expr = module_expr }
  
and structure_item_desc_of_signature_item_desc = function
  | Psig_value {pval_loc; _} ->
      raise_errorf ~loc:pval_loc "ppx_gen_rec: Psig_value not supported yet"
  | Psig_type (rec_flag, decls) ->
      Pstr_type (rec_flag, decls)
  | Psig_typext type_extension ->
      Pstr_typext type_extension
  | Psig_exception extension_constructor ->
      Pstr_exception extension_constructor
  | Psig_module decl ->
      Pstr_module (module_binding_of_declaration decl)
  | Psig_recmodule _module_declarations ->
      raise_errorf "ppx_gen_rec: Psig_recmodule not supported yet"
  | Psig_modtype module_type_declaration ->
      Pstr_modtype module_type_declaration
  | Psig_open desc ->
      Pstr_open (open_declaration_of_open_description desc)
  | Psig_include _include_description ->
      raise_errorf "ppx_gen_rec: Psig_include not supported yet"
  | Psig_class _class_descriptions ->
      raise_errorf "ppx_gen_rec: Psig_class not supported yet"
  | Psig_class_type _class_type_declarations ->
      raise_errorf "ppx_gen_rec: Psig_class_type not supported yet"
  | Psig_attribute attr ->
      Pstr_attribute attr
  | Psig_extension (ext, attrs) ->
      Pstr_extension (ext, attrs)
  | Psig_typesubst _ -> 
      raise_errorf "ppx_gen_rec: Psig_typesubst not supported yet"
  | Psig_modsubst _ ->
      raise_errorf  "ppx_gen_rec: Psig_modsubst not supported yet"

and structure_item_of_signature_item { psig_desc; psig_loc; } =
  {
    pstr_desc = structure_item_desc_of_signature_item_desc psig_desc;
    pstr_loc = psig_loc;
  }

and structure_of_signature signature_items =
  List.map structure_item_of_signature_item signature_items

let mapper =
    object
  inherit Ast_traverse.map as super
  
  method! module_binding = function
  | { pmb_name = {txt = name; _};
      pmb_expr = ({
        pmod_desc = Pmod_constraint (
          ({ pmod_desc = Pmod_ident { txt = Longident.Lident ident; _ }; _ } as m),
          ({ pmty_desc = Pmty_signature signature_items; _ } as mty)
        );
        _
      } as expr);
      _
    } as binding when name = Some ident ->
    { binding with pmb_expr = { expr with
      pmod_desc = Pmod_constraint (
        { m with pmod_desc = Pmod_structure (structure_of_signature signature_items) },
        mty
      )
    } }

  | other ->
    super # module_binding other
end   
  
let expand_struct_item = function
  | PStr [{
    pstr_desc = Pstr_recmodule decls;
    pstr_loc;
  }] -> let decls = List.map (mapper # module_binding) decls in
  { pstr_desc = Pstr_recmodule decls; pstr_loc }
  | _ -> failwith "%gen can only be used with rec modules"
  
  
let extensions = [
  Extension.declare
  "gen"
  Extension.Context.structure_item
  Ast_pattern.__ 
  (fun ~loc:_ ~path:_ x -> expand_struct_item x)
]

let () =
  Driver.register_transformation "ppx_gen_rec"
    ~extensions
