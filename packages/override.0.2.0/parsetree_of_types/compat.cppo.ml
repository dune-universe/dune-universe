let convert_attribute (attribute : Parsetree.attribute)
    : Compat_types.attribute =
#if OCAML_VERSION >= (4, 08, 0)
  { attr_name = attribute.attr_name;
    attr_payload = attribute.attr_payload;
    attr_loc = attribute.attr_loc }
#else
  let (attr_name, attr_payload) = attribute in
  { attr_name; attr_payload; attr_loc = Location.none }
#endif

#if OCAML_VERSION >= (4, 08, 0)
let convert_visibility (visibility : Types.visibility)
    : Compat_types.visibility =
  match visibility with
  | Exported -> Exported
  | Hidden -> Hidden

let convert_module_presence (module_presence : Types.module_presence)
    : Compat_types.module_presence =
  match module_presence with
  | Mp_present -> Mp_present
  | Mp_absent -> Mp_absent

let convert_signature_item (item : Types.signature_item)
    : Compat_types.signature_item =
  match item with
  | Sig_value (ident, desc, visibility) ->
      Sig_value (ident, desc, convert_visibility visibility)
  | Sig_type (ident, decl, rec_status, visibility) ->
      Sig_type (ident, decl, rec_status, convert_visibility visibility)
  | Sig_typext (ident, ext_constr, ext_status, visibility) ->
      Sig_typext (ident, ext_constr, ext_status, convert_visibility visibility)
  | Sig_module (ident, presence, decl, rec_status, visibility) ->
      Sig_module (
        ident, convert_module_presence presence, decl, rec_status,
        convert_visibility visibility)
  | Sig_modtype (ident, decl, visibility) ->
      Sig_modtype (ident, decl, convert_visibility visibility)
  | Sig_class (ident, decl, rec_status, visibility) ->
      Sig_class (ident, decl, rec_status, convert_visibility visibility)
  | Sig_class_type (ident, decl, rec_status, visibility) ->
      Sig_class_type (ident, decl, rec_status, convert_visibility visibility)
#else
let convert_signature_item (item : Types.signature_item)
    : Compat_types.signature_item =
  match item with
  | Sig_value (ident, desc) ->
      Sig_value (ident, desc, Exported)
  | Sig_type (ident, decl, rec_status) ->
      Sig_type (ident, decl, rec_status, Exported)
  | Sig_typext (ident, ext_constr, ext_status) ->
      Sig_typext (ident, ext_constr, ext_status, Exported)
  | Sig_module (ident, decl, rec_status) ->
      Sig_module (ident, Mp_present, decl, rec_status, Exported)
  | Sig_modtype (ident, decl) ->
      Sig_modtype (ident, decl, Exported)
  | Sig_class (ident, decl, rec_status) ->
      Sig_class (ident, decl, rec_status, Exported)
  | Sig_class_type (ident, decl, rec_status) ->
      Sig_class_type (ident, decl, rec_status, Exported)
#endif

let alias_of_module_type (mt : Types.module_type) =
  match mt with
#if OCAML_VERSION >= (4, 08, 0)
  | Mty_alias p
#else
  | Mty_alias (_, p)
#endif
      -> Some p
  | _ -> None
