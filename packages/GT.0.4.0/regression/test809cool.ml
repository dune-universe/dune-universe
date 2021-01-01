open GT

module Location = struct
  (* from Printast module *)
  let fmt_position with_name f l =
    let open Format in
    let open Lexing in
    let fname = if with_name then l.pos_fname else "" in
    if l.pos_lnum = -1
    then fprintf f "%s[%d]" fname l.pos_cnum
    else fprintf f "%s[%d,%d+%d]" fname l.pos_lnum l.pos_bol
        (l.pos_cnum - l.pos_bol)

  type      t = [%import:    Location.t]

  let fmt_location f loc =
    let open Format in
    let p_2nd_name = loc.loc_start.pos_fname <> loc.loc_end.pos_fname in
    fprintf f "(%a..%a)" (fmt_position true) loc.loc_start
      (fmt_position p_2nd_name) loc.loc_end;
    if loc.loc_ghost then fprintf f " ghost";

  class virtual ['inh,'self,'syn] t_t =
    object method virtual  do_t : 'inh -> t -> 'syn end
  let gcata_t tr inh subj = tr#do_t inh subj

  class ['self] html_t_t _fself = object
    inherit  [unit,'self,View.viewer] t_t
    method do_t () _ = HTML.string "<noloc>"
  end
  let html_t () subj = GT.transform_gc gcata_t (new html_t_t) () subj

  class ['self] fmt_t_t fself = object
    inherit  [Format.formatter, 'self, unit] t_t
    method do_t = fmt_location
  end
  let fmt_t fmt subj = GT.transform_gc gcata_t (new fmt_t_t) fmt subj
  let t =
    { GT.gcata = gcata_t
    ; GT.fix = (fun eta -> transform_gc gcata_t eta)
    ; GT.plugins = (object method html = html_t () method fmt = fmt_t end)
    }

  type 'a loc = [%import: 'a Location.loc]
  [@@deriving gt ~options:{ fmt; html }]

end

module Longident = struct
  type t = [%import: Longident.t] [@@deriving gt ~options:{ fmt; html }]
end

module Asttypes = struct
  type rec_flag       = [%import: Asttypes.rec_flag]       [@@deriving gt ~options:{ fmt; html }]
  type direction_flag = [%import: Asttypes.direction_flag] [@@deriving gt ~options:{ fmt; html }]
  type private_flag   = [%import: Asttypes.private_flag]   [@@deriving gt ~options:{ fmt; html }]
  type mutable_flag   = [%import: Asttypes.mutable_flag]   [@@deriving gt ~options:{ fmt; html }]
  type virtual_flag   = [%import: Asttypes.virtual_flag]   [@@deriving gt ~options:{ fmt; html }]
  type override_flag  = [%import: Asttypes.override_flag]  [@@deriving gt ~options:{ fmt; html }]
  type closed_flag    = [%import: Asttypes.closed_flag]    [@@deriving gt ~options:{ fmt; html }]

  type label = string [@@deriving gt ~options:{ fmt; html }]
  type arg_label =  [%import: Asttypes.arg_label] [@@deriving gt ~options:{ fmt; html }]
  type 'a loc   = [%import: 'a Asttypes.loc]   [@@deriving gt ~options:{ fmt; html }]
  type variance = [%import: Asttypes.variance] [@@deriving gt ~options:{ fmt; html }]
end
open Asttypes

type constant = [%import: Parsetree.constant] [@@deriving gt ~options:{ fmt; html }]

(*
type attribute = [%import: Parsetree.attribute]
and extension = [%import: Parsetree.extension]
and attributes = [%import: Parsetree.attributes]
and payload = [%import: Parsetree.payload]
and core_type = [%import: Parsetree.core_type]
and core_type_desc = [%import: Parsetree.core_type_desc]
and package_type = [%import: Parsetree.package_type]

and row_field = [%import: Parsetree.row_field]

and object_field = [%import: Parsetree.object_field]
and structure = [%import: Parsetree.structure]
and structure_item = [%import: Parsetree.structure_item]
and structure_item_desc = [%import: Parsetree.structure_item_desc]
and value_binding = [%import: Parsetree.value_binding]
and value_description = [%import: Parsetree.value_description]
and type_declaration = [%import: Parsetree.type_declaration]
and type_extension = [%import: Parsetree.type_extension]
and module_binding = [%import: Parsetree.module_binding]
and module_type_declaration = [%import: Parsetree.module_type_declaration]
and open_description = [%import: Parsetree.open_description]
and class_type_declaration = [%import: Parsetree.class_type_declaration]
and class_type = [%import: Parsetree.class_type]
and class_type_desc = [%import: Parsetree.class_type_desc]
and class_signature = [%import: Parsetree.class_signature]
and class_type_field = [%import: Parsetree.class_type_field]
and class_type_field_desc = [%import: Parsetree.class_type_field_desc]
and include_declaration = [%import: Parsetree.include_declaration]
and 'a include_infos = [%import: 'a Parsetree.include_infos]
and module_expr = [%import: Parsetree.module_expr]
and module_expr_desc = [%import: Parsetree.module_expr_desc]
and module_type = [%import: Parsetree.module_type]
and module_type_desc = [%import: Parsetree.module_type_desc]
and class_declaration = [%import: Parsetree.class_declaration]
and 'a class_infos = [%import: 'a Parsetree.class_infos]
and class_expr = [%import: Parsetree.class_expr]
and class_expr_desc = [%import: Parsetree.class_expr_desc]
and class_structure = [%import: Parsetree.class_structure]
and class_field = [%import: Parsetree.class_field]
and class_field_desc = [%import: Parsetree.class_field_desc]
and class_field_kind = [%import: Parsetree.class_field_kind]
and type_kind = [%import: Parsetree.type_kind]
and constructor_declaration = [%import: Parsetree.constructor_declaration]
and constructor_arguments = [%import: Parsetree.constructor_arguments]
and label_declaration = [%import: Parsetree.label_declaration]
and with_constraint = [%import: Parsetree.with_constraint]
and signature = [%import: Parsetree.signature]
and signature_item = [%import: Parsetree.signature_item]
and signature_item_desc = [%import: Parsetree.signature_item_desc]
and module_declaration = [%import: Parsetree.module_declaration]
and include_description = [%import: Parsetree.include_description]
and class_description = [%import: Parsetree.class_description]
and pattern = [%import: Parsetree.pattern]
and pattern_desc = [%import: Parsetree.pattern_desc]
and expression = [%import: Parsetree.expression]
and expression_desc = [%import: Parsetree.expression_desc]
and extension_constructor = [%import: Parsetree.extension_constructor]
and extension_constructor_kind = [%import: Parsetree.extension_constructor_kind]
and case = [%import: Parsetree.case]
[@@deriving gt ~options:{ fmt; }]
*)

type attribute = (string Asttypes.loc * payload)
and extension = (string Asttypes.loc * payload)
and attributes = attribute list
and payload =
  | PStr of int
  | PSig of signature
  | PTyp of core_type
  | PPat of pattern * expression option
and core_type =
  {
  ptyp_desc: core_type_desc ;
  ptyp_loc: Location.t ;
  ptyp_attributes: attributes }
and core_type_desc =
  | Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of Asttypes.arg_label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t Asttypes.loc * core_type list
  | Ptyp_object of object_field list * Asttypes.closed_flag
  | Ptyp_class of Longident.t Asttypes.loc * core_type list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * Asttypes.closed_flag * Asttypes.label
  list option
  | Ptyp_poly of string Asttypes.loc list * core_type
  | Ptyp_package of package_type
  | Ptyp_extension of extension
and package_type =
  (Longident.t Asttypes.loc * (Longident.t Asttypes.loc * core_type) list)
and row_field =
  | Rtag of Asttypes.label Asttypes.loc * attributes * bool * core_type list

  | Rinherit of core_type
and object_field =
  | Otag of Asttypes.label Asttypes.loc * attributes * core_type
  | Oinherit of core_type
and structure = structure_item list
and structure_item =
  {
  pstr_desc: structure_item_desc ;
  pstr_loc: Location.t }
and structure_item_desc =
  | Pstr_eval of expression * attributes
  | Pstr_value of Asttypes.rec_flag * value_binding list
  | Pstr_primitive of value_description
  | Pstr_type of Asttypes.rec_flag * type_declaration list
  | Pstr_typext of type_extension
  | Pstr_exception of extension_constructor
  | Pstr_module of module_binding
  | Pstr_recmodule of module_binding list
  | Pstr_modtype of module_type_declaration
  | Pstr_open of open_description
  | Pstr_class of class_declaration list
  | Pstr_class_type of class_type_declaration list
  | Pstr_include of include_declaration
  | Pstr_attribute of attribute
  | Pstr_extension of extension * attributes
and value_binding =
  {
  pvb_pat: pattern ;
  pvb_expr: expression ;
  pvb_attributes: attributes ;
  pvb_loc: Location.t }
and value_description =
  {
  pval_name: string Asttypes.loc ;
  pval_type: core_type ;
  pval_prim: string list ;
  pval_attributes: attributes ;
  pval_loc: Location.t }
and type_declaration =
  {
  ptype_name: string Asttypes.loc ;
  ptype_params: (core_type * Asttypes.variance) list ;
  ptype_cstrs: (core_type * core_type * Location.t) list ;
  ptype_kind: type_kind ;
  ptype_private: Asttypes.private_flag ;
  ptype_manifest: core_type option ;
  ptype_attributes: attributes ;
  ptype_loc: Location.t }
and type_extension =
  {
  ptyext_path: Longident.t Asttypes.loc ;
  ptyext_params: (core_type * Asttypes.variance) list ;
  ptyext_constructors: extension_constructor list ;
  ptyext_private: Asttypes.private_flag ;
  ptyext_attributes: attributes }
and module_binding =
  {
  pmb_name: string Asttypes.loc ;
  pmb_expr: module_expr ;
  pmb_attributes: attributes ;
  pmb_loc: Location.t }
and module_type_declaration =
  {
  pmtd_name: string Asttypes.loc ;
  pmtd_type: module_type option ;
  pmtd_attributes: attributes ;
  pmtd_loc: Location.t }
and open_description =
  {
  popen_lid: Longident.t Asttypes.loc ;
  popen_override: Asttypes.override_flag ;
  popen_loc: Location.t ;
  popen_attributes: attributes }
and class_type_declaration = class_type class_infos
and class_type =
  {
  pcty_desc: class_type_desc ;
  pcty_loc: Location.t ;
  pcty_attributes: attributes }
and class_type_desc =
  | Pcty_constr of Longident.t Asttypes.loc * core_type list
  | Pcty_signature of class_signature
  | Pcty_arrow of Asttypes.arg_label * core_type * class_type
  | Pcty_extension of extension
  | Pcty_open of Asttypes.override_flag * Longident.t Asttypes.loc *
  class_type
and class_signature =
  {
  pcsig_self: core_type ;
  pcsig_fields: class_type_field list }
and class_type_field =
  {
  pctf_desc: class_type_field_desc ;
  pctf_loc: Location.t ;
  pctf_attributes: attributes }
and class_type_field_desc =
  | Pctf_inherit of class_type
  | Pctf_val of (Asttypes.label Asttypes.loc *
                 Asttypes.mutable_flag *
                 Asttypes.virtual_flag *
                 core_type
                )
  (* | Pctf_method of (Asttypes.label Asttypes.loc * Asttypes.private_flag *
   * Asttypes.virtual_flag * core_type)
   * | Pctf_constraint of (core_type * core_type)
   * | Pctf_attribute of attribute
   * | Pctf_extension of extension *)
and include_declaration = module_expr include_infos
and 'a include_infos =
  {
  pincl_mod: 'a ;
  pincl_loc: Location.t ;
  pincl_attributes: attributes }
and module_expr =
  {
  pmod_desc: module_expr_desc ;
  pmod_loc: Location.t ;
  pmod_attributes: attributes }
and module_expr_desc =
  | Pmod_ident of Longident.t Asttypes.loc
  | Pmod_structure of structure
  | Pmod_functor of string Asttypes.loc * module_type option * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression
  | Pmod_extension of extension
and module_type =
  {
  pmty_desc: module_type_desc ;
  pmty_loc: Location.t ;
  pmty_attributes: attributes }
and module_type_desc =
  | Pmty_ident of Longident.t Asttypes.loc
  | Pmty_signature of signature
  | Pmty_functor of string Asttypes.loc * module_type option * module_type
  | Pmty_with of module_type * with_constraint list
  | Pmty_typeof of module_expr
  | Pmty_extension of extension
  | Pmty_alias of Longident.t Asttypes.loc
and class_declaration = class_expr class_infos
and 'a class_infos =
  {
  pci_virt: Asttypes.virtual_flag ;
  pci_params: (core_type * Asttypes.variance) list ;
  pci_name: string Asttypes.loc ;
  pci_expr: 'a ;
  pci_loc: Location.t ;
  pci_attributes: attributes }
and class_expr =
  {
  pcl_desc: class_expr_desc ;
  pcl_loc: Location.t ;
  pcl_attributes: attributes }
and class_expr_desc =
  | Pcl_constr of Longident.t Asttypes.loc * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of Asttypes.arg_label * expression option * pattern * class_expr

  | Pcl_apply of class_expr * (Asttypes.arg_label * expression) list
  | Pcl_let of Asttypes.rec_flag * value_binding list * class_expr
  | Pcl_constraint of class_expr * class_type
  | Pcl_extension of extension
  | Pcl_open of Asttypes.override_flag * Longident.t Asttypes.loc *
  class_expr
and class_structure =
  {
  pcstr_self: pattern ;
  pcstr_fields: class_field list }
and class_field =
  {
  pcf_desc: class_field_desc ;
  pcf_loc: Location.t ;
  pcf_attributes: attributes }
and class_field_desc =
  | Pcf_inherit of Asttypes.override_flag * class_expr * string Asttypes.loc
  option
  | Pcf_val of (Asttypes.label Asttypes.loc * Asttypes.mutable_flag *
  class_field_kind)
  | Pcf_method of (Asttypes.label Asttypes.loc * Asttypes.private_flag *
  class_field_kind)
  | Pcf_constraint of (core_type * core_type)
  | Pcf_initializer of expression
  | Pcf_attribute of attribute
  | Pcf_extension of extension
and class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of Asttypes.override_flag * expression
and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list
  | Ptype_open
and constructor_declaration =
  {
  pcd_name: string Asttypes.loc ;
  pcd_args: constructor_arguments ;
  pcd_res: core_type option ;
  pcd_loc: Location.t ;
  pcd_attributes: attributes }
and constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list
and label_declaration =
  {
  pld_name: string Asttypes.loc ;
  pld_mutable: Asttypes.mutable_flag ;
  pld_type: core_type ;
  pld_loc: Location.t ;
  pld_attributes: attributes }
and with_constraint =
  | Pwith_type of Longident.t Asttypes.loc * type_declaration
  | Pwith_module of Longident.t Asttypes.loc * Longident.t Asttypes.loc
  | Pwith_typesubst of Longident.t Asttypes.loc * type_declaration
  | Pwith_modsubst of Longident.t Asttypes.loc * Longident.t Asttypes.loc
and signature = signature_item list
and signature_item =
  {
  psig_desc: signature_item_desc ;
  psig_loc: Location.t }
and signature_item_desc =
  | Psig_value of value_description
  | Psig_type of Asttypes.rec_flag * type_declaration list
  | Psig_typext of type_extension
  | Psig_exception of extension_constructor
  | Psig_module of module_declaration
  | Psig_recmodule of module_declaration list
  | Psig_modtype of module_type_declaration
  | Psig_open of open_description
  | Psig_include of include_description
  | Psig_class of class_description list
  | Psig_class_type of class_type_declaration list
  | Psig_attribute of attribute
  | Psig_extension of extension * attributes
and module_declaration =
  {
  pmd_name: string Asttypes.loc ;
  pmd_type: module_type ;
  pmd_attributes: attributes ;
  pmd_loc: Location.t }
and include_description = module_type include_infos
and class_description = class_type class_infos
and pattern =
  {
  ppat_desc: pattern_desc ;
  ppat_loc: Location.t ;
  ppat_attributes: attributes }
and pattern_desc =
  | Ppat_any
  | Ppat_var of string Asttypes.loc
  | Ppat_alias of pattern * string Asttypes.loc
  | Ppat_constant of constant
  | Ppat_interval of constant * constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t Asttypes.loc * pattern option
  | Ppat_variant of Asttypes.label * pattern option
  | Ppat_record of (Longident.t Asttypes.loc * pattern) list *
  Asttypes.closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of Longident.t Asttypes.loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string Asttypes.loc
  | Ppat_exception of pattern
  | Ppat_extension of extension
  | Ppat_open of Longident.t Asttypes.loc * pattern
and expression =
  {
  pexp_desc: expression_desc ;
  pexp_loc: Location.t ;
  pexp_attributes: attributes }
and expression_desc =
  | Pexp_ident of Longident.t Asttypes.loc
  | Pexp_constant of constant
  | Pexp_let of Asttypes.rec_flag * value_binding list * expression
  | Pexp_function of case list
  | Pexp_fun of Asttypes.arg_label * expression option * pattern * expression

  | Pexp_apply of expression * (Asttypes.arg_label * expression) list
  | Pexp_match of expression * case list
  | Pexp_try of expression * case list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t Asttypes.loc * expression option
  | Pexp_variant of Asttypes.label * expression option
  | Pexp_record of (Longident.t Asttypes.loc * expression) list * expression
  option
  | Pexp_field of expression * Longident.t Asttypes.loc
  | Pexp_setfield of expression * Longident.t Asttypes.loc * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of pattern * expression * expression * Asttypes.direction_flag *
  expression
  | Pexp_constraint of expression * core_type
  | Pexp_coerce of expression * core_type option * core_type
  | Pexp_send of expression * Asttypes.label Asttypes.loc
  | Pexp_new of Longident.t Asttypes.loc
  | Pexp_setinstvar of Asttypes.label Asttypes.loc * expression
  | Pexp_override of (Asttypes.label Asttypes.loc * expression) list
  | Pexp_letmodule of string Asttypes.loc * module_expr * expression
  | Pexp_letexception of extension_constructor * expression
  | Pexp_assert of expression
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string Asttypes.loc * expression
  | Pexp_pack of module_expr
  | Pexp_open of Asttypes.override_flag * Longident.t Asttypes.loc *
  expression
  | Pexp_extension of extension
  | Pexp_unreachable
and extension_constructor =
  {
  pext_name: string Asttypes.loc ;
  pext_kind: extension_constructor_kind ;
  pext_loc: Location.t ;
  pext_attributes: attributes }
and extension_constructor_kind =
  | Pext_decl of constructor_arguments * core_type option
  | Pext_rebind of Longident.t Asttypes.loc
and case =
  {
  pc_lhs: pattern ;
  pc_guard: expression option ;
  pc_rhs: expression }
[@@deriving gt ~options:{ fmt;  }]



