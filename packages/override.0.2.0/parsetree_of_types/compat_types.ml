type attribute = {
    attr_name : string Location.loc;
    attr_payload : Parsetree.payload;
    attr_loc : Location.t;
  }

type visibility =
  | Exported
  | Hidden

type module_presence =
  | Mp_present
  | Mp_absent

type signature_item =
  | Sig_value of Ident.t * Types.value_description * visibility
  | Sig_type of
      Ident.t * Types.type_declaration * Types.rec_status * visibility
  | Sig_typext of
      Ident.t * Types.extension_constructor * Types.ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * Types.module_declaration *
        Types.rec_status * visibility
  | Sig_modtype of Ident.t * Types.modtype_declaration * visibility
  | Sig_class of
      Ident.t * Types.class_declaration * Types.rec_status * visibility
  | Sig_class_type of
      Ident.t * Types.class_type_declaration * Types.rec_status * visibility
