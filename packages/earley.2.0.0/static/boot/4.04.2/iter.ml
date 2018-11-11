open Asttypes
open Parsetree
open Longident
let iter_option fn o1 = match o1 with | None  -> () | Some e1 -> fn e1 
let iter_list = List.iter 
let do_local_ident = ref (fun _  -> ()) 
let rec iter_longident i1 =
  match i1 with | Lident s1 -> (!do_local_ident) s1 | _ -> () 
(* asttypes.mli *)
let iter_constant c1 =
  match c1 with
  | Const_int(x) -> (fun _ -> ()) x
  | Const_char(x) -> (fun _ -> ()) x
  | Const_string(x1,x2) -> () ; ((fun _ -> ()) x1) ; ((iter_option (fun _ -> ())) x2)  | Const_float(x) -> (fun _ -> ()) x
  | Const_int32(x) -> (fun _ -> ()) x
  | Const_int64(x) -> (fun _ -> ()) x
  | Const_nativeint(x) -> (fun _ -> ()) x

let iter_rec_flag c1 =
  match c1 with
  | Nonrecursive -> ()
  | Recursive -> ()

let iter_direction_flag c1 =
  match c1 with
  | Upto -> ()
  | Downto -> ()

let iter_private_flag c1 =
  match c1 with
  | Private -> ()
  | Public -> ()

let iter_mutable_flag c1 =
  match c1 with
  | Immutable -> ()
  | Mutable -> ()

let iter_virtual_flag c1 =
  match c1 with
  | Virtual -> ()
  | Concrete -> ()

let iter_override_flag c1 =
  match c1 with
  | Override -> ()
  | Fresh -> ()

let iter_closed_flag c1 =
  match c1 with
  | Closed -> ()
  | Open -> ()

let iter_label c1 = (fun _ -> ()) c1
let iter_arg_label c1 =
  match c1 with
  | Nolabel -> ()
  | Labelled(x) -> (fun _ -> ()) x
  | Optional(x) -> (fun _ -> ()) x

let iter_loc : 'a. ('a -> unit) -> 'a loc -> unit = fun iter_a r1 -> () ; iter_a r1.txt  ; (fun _ -> ()) r1.loc 
let iter_variance c1 =
  match c1 with
  | Covariant -> ()
  | Contravariant -> ()
  | Invariant -> ()

(* parsetree.mli *)
let iter_constant c1 =
  match c1 with
  | Pconst_integer(x1,x2) -> () ; ((fun _ -> ()) x1) ; ((iter_option (fun _ -> ())) x2)  | Pconst_char(x) -> (fun _ -> ()) x
  | Pconst_string(x1,x2) -> () ; ((fun _ -> ()) x1) ; ((iter_option (fun _ -> ())) x2)  | Pconst_float(x1,x2) -> () ; ((fun _ -> ()) x1) ; ((iter_option (fun _ -> ())) x2)
let rec iter_attribute c1 = (fun (x1,x2) -> () ; ((iter_loc (fun _ -> ())) x1) ; (iter_payload x2)) c1
and iter_extension c1 = (fun (x1,x2) -> () ; ((iter_loc (fun _ -> ())) x1) ; (iter_payload x2)) c1
and iter_attributes c1 = (iter_list iter_attribute) c1
and iter_payload c1 =
  match c1 with
  | PStr(x) -> iter_structure x
  | PSig(x) -> iter_signature x
  | PTyp(x) -> iter_core_type x
  | PPat(x1,x2) -> () ; (iter_pattern x1) ; ((iter_option iter_expression) x2)
and iter_core_type = fun r1 -> () ; iter_core_type_desc r1.ptyp_desc  ; (fun _ -> ()) r1.ptyp_loc  ; iter_attributes r1.ptyp_attributes 
and iter_core_type_desc c1 =
  match c1 with
  | Ptyp_any -> ()
  | Ptyp_var(x) -> (fun _ -> ()) x
  | Ptyp_arrow(x1,x2,x3) -> () ; (iter_arg_label x1) ; (iter_core_type x2) ; (iter_core_type x3)  | Ptyp_tuple(x) -> (iter_list iter_core_type) x
  | Ptyp_constr(x1,x2) -> () ; ((iter_loc iter_longident) x1) ; ((iter_list iter_core_type) x2)  | Ptyp_object(x1,x2) -> () ; ((iter_list (fun (x1,x2,x3) -> () ; ((fun _ -> ()) x1) ; (iter_attributes x2) ; (iter_core_type x3))) x1) ; (iter_closed_flag x2)  | Ptyp_class(x1,x2) -> () ; ((iter_loc iter_longident) x1) ; ((iter_list iter_core_type) x2)  | Ptyp_alias(x1,x2) -> () ; (iter_core_type x1) ; ((fun _ -> ()) x2)  | Ptyp_variant(x1,x2,x3) -> () ; ((iter_list iter_row_field) x1) ; (iter_closed_flag x2) ; ((iter_option (iter_list iter_label)) x3)  | Ptyp_poly(x1,x2) -> () ; ((iter_list (fun _ -> ())) x1) ; (iter_core_type x2)  | Ptyp_package(x) -> iter_package_type x
  | Ptyp_extension(x) -> iter_extension x

and iter_package_type c1 = (fun (x1,x2) -> () ; ((iter_loc iter_longident) x1) ; ((iter_list (fun (x1,x2) -> () ; ((iter_loc iter_longident) x1) ; (iter_core_type x2))) x2)) c1
and iter_row_field c1 =
  match c1 with
  | Rtag(x1,x2,x3,x4) -> () ; (iter_label x1) ; (iter_attributes x2) ; ((fun _ -> ()) x3) ; ((iter_list iter_core_type) x4)  | Rinherit(x) -> iter_core_type x

and iter_pattern = fun r1 -> () ; iter_pattern_desc r1.ppat_desc  ; (fun _ -> ()) r1.ppat_loc  ; iter_attributes r1.ppat_attributes 
and iter_pattern_desc c1 =
  match c1 with
  | Ppat_any -> ()
  | Ppat_var(x) -> (iter_loc (fun _ -> ())) x
  | Ppat_alias(x1,x2) -> () ; (iter_pattern x1) ; ((iter_loc (fun _ -> ())) x2)  | Ppat_constant(x) -> iter_constant x
  | Ppat_interval(x1,x2) -> () ; (iter_constant x1) ; (iter_constant x2)  | Ppat_tuple(x) -> (iter_list iter_pattern) x
  | Ppat_construct(x1,x2) -> () ; ((iter_loc iter_longident) x1) ; ((iter_option iter_pattern) x2)  | Ppat_variant(x1,x2) -> () ; (iter_label x1) ; ((iter_option iter_pattern) x2)  | Ppat_record(x1,x2) -> () ; ((iter_list (fun (x1,x2) -> () ; ((iter_loc iter_longident) x1) ; (iter_pattern x2))) x1) ; (iter_closed_flag x2)  | Ppat_array(x) -> (iter_list iter_pattern) x
  | Ppat_or(x1,x2) -> () ; (iter_pattern x1) ; (iter_pattern x2)  | Ppat_constraint(x1,x2) -> () ; (iter_pattern x1) ; (iter_core_type x2)  | Ppat_type(x) -> (iter_loc iter_longident) x
  | Ppat_lazy(x) -> iter_pattern x
  | Ppat_unpack(x) -> (iter_loc (fun _ -> ())) x
  | Ppat_exception(x) -> iter_pattern x
  | Ppat_extension(x) -> iter_extension x
  | Ppat_open(x1,x2) -> () ; ((iter_loc iter_longident) x1) ; (iter_pattern x2)
and iter_expression = fun r1 -> () ; iter_expression_desc r1.pexp_desc  ; (fun _ -> ()) r1.pexp_loc  ; iter_attributes r1.pexp_attributes 
and iter_expression_desc c1 =
  match c1 with
  | Pexp_ident(x) -> (iter_loc iter_longident) x
  | Pexp_constant(x) -> iter_constant x
  | Pexp_let(x1,x2,x3) -> () ; (iter_rec_flag x1) ; ((iter_list iter_value_binding) x2) ; (iter_expression x3)  | Pexp_function(x) -> (iter_list iter_case) x
  | Pexp_fun(x1,x2,x3,x4) -> () ; (iter_arg_label x1) ; ((iter_option iter_expression) x2) ; (iter_pattern x3) ; (iter_expression x4)  | Pexp_apply(x1,x2) -> () ; (iter_expression x1) ; ((iter_list (fun (x1,x2) -> () ; (iter_arg_label x1) ; (iter_expression x2))) x2)  | Pexp_match(x1,x2) -> () ; (iter_expression x1) ; ((iter_list iter_case) x2)  | Pexp_try(x1,x2) -> () ; (iter_expression x1) ; ((iter_list iter_case) x2)  | Pexp_tuple(x) -> (iter_list iter_expression) x
  | Pexp_construct(x1,x2) -> () ; ((iter_loc iter_longident) x1) ; ((iter_option iter_expression) x2)  | Pexp_variant(x1,x2) -> () ; (iter_label x1) ; ((iter_option iter_expression) x2)  | Pexp_record(x1,x2) -> () ; ((iter_list (fun (x1,x2) -> () ; ((iter_loc iter_longident) x1) ; (iter_expression x2))) x1) ; ((iter_option iter_expression) x2)  | Pexp_field(x1,x2) -> () ; (iter_expression x1) ; ((iter_loc iter_longident) x2)  | Pexp_setfield(x1,x2,x3) -> () ; (iter_expression x1) ; ((iter_loc iter_longident) x2) ; (iter_expression x3)  | Pexp_array(x) -> (iter_list iter_expression) x
  | Pexp_ifthenelse(x1,x2,x3) -> () ; (iter_expression x1) ; (iter_expression x2) ; ((iter_option iter_expression) x3)  | Pexp_sequence(x1,x2) -> () ; (iter_expression x1) ; (iter_expression x2)  | Pexp_while(x1,x2) -> () ; (iter_expression x1) ; (iter_expression x2)  | Pexp_for(x1,x2,x3,x4,x5) -> () ; (iter_pattern x1) ; (iter_expression x2) ; (iter_expression x3) ; (iter_direction_flag x4) ; (iter_expression x5)  | Pexp_constraint(x1,x2) -> () ; (iter_expression x1) ; (iter_core_type x2)  | Pexp_coerce(x1,x2,x3) -> () ; (iter_expression x1) ; ((iter_option iter_core_type) x2) ; (iter_core_type x3)  | Pexp_send(x1,x2) -> () ; (iter_expression x1) ; ((fun _ -> ()) x2)  | Pexp_new(x) -> (iter_loc iter_longident) x
  | Pexp_setinstvar(x1,x2) -> () ; ((iter_loc (fun _ -> ())) x1) ; (iter_expression x2)  | Pexp_override(x) -> (iter_list (fun (x1,x2) -> () ; ((iter_loc (fun _ -> ())) x1) ; (iter_expression x2))) x
  | Pexp_letmodule(x1,x2,x3) -> () ; ((iter_loc (fun _ -> ())) x1) ; (iter_module_expr x2) ; (iter_expression x3)  | Pexp_letexception(x1,x2) -> () ; (iter_extension_constructor x1) ; (iter_expression x2)  | Pexp_assert(x) -> iter_expression x
  | Pexp_lazy(x) -> iter_expression x
  | Pexp_poly(x1,x2) -> () ; (iter_expression x1) ; ((iter_option iter_core_type) x2)  | Pexp_object(x) -> iter_class_structure x
  | Pexp_newtype(x1,x2) -> () ; ((fun _ -> ()) x1) ; (iter_expression x2)  | Pexp_pack(x) -> iter_module_expr x
  | Pexp_open(x1,x2,x3) -> () ; (iter_override_flag x1) ; ((iter_loc iter_longident) x2) ; (iter_expression x3)  | Pexp_extension(x) -> iter_extension x
  | Pexp_unreachable -> ()

and iter_case = fun r1 -> () ; iter_pattern r1.pc_lhs  ; (iter_option iter_expression) r1.pc_guard  ; iter_expression r1.pc_rhs 
and iter_value_description = fun r1 -> () ; (iter_loc (fun _ -> ())) r1.pval_name  ; iter_core_type r1.pval_type  ; (iter_list (fun _ -> ())) r1.pval_prim  ; iter_attributes r1.pval_attributes  ; (fun _ -> ()) r1.pval_loc 
and iter_type_declaration = fun r1 -> () ; (iter_loc (fun _ -> ())) r1.ptype_name  ; (iter_list (fun (x1,x2) -> () ; (iter_core_type x1) ; (iter_variance x2))) r1.ptype_params  ; (iter_list (fun (x1,x2,x3) -> () ; (iter_core_type x1) ; (iter_core_type x2) ; ((fun _ -> ()) x3))) r1.ptype_cstrs  ; iter_type_kind r1.ptype_kind  ; iter_private_flag r1.ptype_private  ; (iter_option iter_core_type) r1.ptype_manifest  ; iter_attributes r1.ptype_attributes  ; (fun _ -> ()) r1.ptype_loc 
and iter_type_kind c1 =
  match c1 with
  | Ptype_abstract -> ()
  | Ptype_variant(x) -> (iter_list iter_constructor_declaration) x
  | Ptype_record(x) -> (iter_list iter_label_declaration) x
  | Ptype_open -> ()

and iter_label_declaration = fun r1 -> () ; (iter_loc (fun _ -> ())) r1.pld_name  ; iter_mutable_flag r1.pld_mutable  ; iter_core_type r1.pld_type  ; (fun _ -> ()) r1.pld_loc  ; iter_attributes r1.pld_attributes 
and iter_constructor_declaration = fun r1 -> () ; (iter_loc (fun _ -> ())) r1.pcd_name  ; iter_constructor_arguments r1.pcd_args  ; (iter_option iter_core_type) r1.pcd_res  ; (fun _ -> ()) r1.pcd_loc  ; iter_attributes r1.pcd_attributes 
and iter_constructor_arguments c1 =
  match c1 with
  | Pcstr_tuple(x) -> (iter_list iter_core_type) x
  | Pcstr_record(x) -> (iter_list iter_label_declaration) x

and iter_type_extension = fun r1 -> () ; (iter_loc iter_longident) r1.ptyext_path  ; (iter_list (fun (x1,x2) -> () ; (iter_core_type x1) ; (iter_variance x2))) r1.ptyext_params  ; (iter_list iter_extension_constructor) r1.ptyext_constructors  ; iter_private_flag r1.ptyext_private  ; iter_attributes r1.ptyext_attributes 
and iter_extension_constructor = fun r1 -> () ; (iter_loc (fun _ -> ())) r1.pext_name  ; iter_extension_constructor_kind r1.pext_kind  ; (fun _ -> ()) r1.pext_loc  ; iter_attributes r1.pext_attributes 
and iter_extension_constructor_kind c1 =
  match c1 with
  | Pext_decl(x1,x2) -> () ; (iter_constructor_arguments x1) ; ((iter_option iter_core_type) x2)  | Pext_rebind(x) -> (iter_loc iter_longident) x

and iter_class_type = fun r1 -> () ; iter_class_type_desc r1.pcty_desc  ; (fun _ -> ()) r1.pcty_loc  ; iter_attributes r1.pcty_attributes 
and iter_class_type_desc c1 =
  match c1 with
  | Pcty_constr(x1,x2) -> () ; ((iter_loc iter_longident) x1) ; ((iter_list iter_core_type) x2)  | Pcty_signature(x) -> iter_class_signature x
  | Pcty_arrow(x1,x2,x3) -> () ; (iter_arg_label x1) ; (iter_core_type x2) ; (iter_class_type x3)  | Pcty_extension(x) -> iter_extension x

and iter_class_signature = fun r1 -> () ; iter_core_type r1.pcsig_self  ; (iter_list iter_class_type_field) r1.pcsig_fields 
and iter_class_type_field = fun r1 -> () ; iter_class_type_field_desc r1.pctf_desc  ; (fun _ -> ()) r1.pctf_loc  ; iter_attributes r1.pctf_attributes 
and iter_class_type_field_desc c1 =
  match c1 with
  | Pctf_inherit(x) -> iter_class_type x
  | Pctf_val(x1,x2,x3,x4) -> () ; ((fun _ -> ()) x1) ; (iter_mutable_flag x2) ; (iter_virtual_flag x3) ; (iter_core_type x4)  | Pctf_method(x1,x2,x3,x4) -> () ; ((fun _ -> ()) x1) ; (iter_private_flag x2) ; (iter_virtual_flag x3) ; (iter_core_type x4)  | Pctf_constraint(x1,x2) -> () ; (iter_core_type x1) ; (iter_core_type x2)  | Pctf_attribute(x) -> iter_attribute x
  | Pctf_extension(x) -> iter_extension x

and iter_class_infos : 'a. ('a -> unit) -> 'a class_infos -> unit = fun iter_a r1 -> () ; iter_virtual_flag r1.pci_virt  ; (iter_list (fun (x1,x2) -> () ; (iter_core_type x1) ; (iter_variance x2))) r1.pci_params  ; (iter_loc (fun _ -> ())) r1.pci_name  ; iter_a r1.pci_expr  ; (fun _ -> ()) r1.pci_loc  ; iter_attributes r1.pci_attributes 
and iter_class_description c1 = (iter_class_infos iter_class_type) c1
and iter_class_type_declaration c1 = (iter_class_infos iter_class_type) c1
and iter_class_expr = fun r1 -> () ; iter_class_expr_desc r1.pcl_desc  ; (fun _ -> ()) r1.pcl_loc  ; iter_attributes r1.pcl_attributes 
and iter_class_expr_desc c1 =
  match c1 with
  | Pcl_constr(x1,x2) -> () ; ((iter_loc iter_longident) x1) ; ((iter_list iter_core_type) x2)  | Pcl_structure(x) -> iter_class_structure x
  | Pcl_fun(x1,x2,x3,x4) -> () ; (iter_arg_label x1) ; ((iter_option iter_expression) x2) ; (iter_pattern x3) ; (iter_class_expr x4)  | Pcl_apply(x1,x2) -> () ; (iter_class_expr x1) ; ((iter_list (fun (x1,x2) -> () ; (iter_arg_label x1) ; (iter_expression x2))) x2)  | Pcl_let(x1,x2,x3) -> () ; (iter_rec_flag x1) ; ((iter_list iter_value_binding) x2) ; (iter_class_expr x3)  | Pcl_constraint(x1,x2) -> () ; (iter_class_expr x1) ; (iter_class_type x2)  | Pcl_extension(x) -> iter_extension x

and iter_class_structure = fun r1 -> () ; iter_pattern r1.pcstr_self  ; (iter_list iter_class_field) r1.pcstr_fields 
and iter_class_field = fun r1 -> () ; iter_class_field_desc r1.pcf_desc  ; (fun _ -> ()) r1.pcf_loc  ; iter_attributes r1.pcf_attributes 
and iter_class_field_desc c1 =
  match c1 with
  | Pcf_inherit(x1,x2,x3) -> () ; (iter_override_flag x1) ; (iter_class_expr x2) ; ((iter_option (fun _ -> ())) x3)  | Pcf_val(x1,x2,x3) -> () ; ((iter_loc (fun _ -> ())) x1) ; (iter_mutable_flag x2) ; (iter_class_field_kind x3)  | Pcf_method(x1,x2,x3) -> () ; ((iter_loc (fun _ -> ())) x1) ; (iter_private_flag x2) ; (iter_class_field_kind x3)  | Pcf_constraint(x1,x2) -> () ; (iter_core_type x1) ; (iter_core_type x2)  | Pcf_initializer(x) -> iter_expression x
  | Pcf_attribute(x) -> iter_attribute x
  | Pcf_extension(x) -> iter_extension x

and iter_class_field_kind c1 =
  match c1 with
  | Cfk_virtual(x) -> iter_core_type x
  | Cfk_concrete(x1,x2) -> () ; (iter_override_flag x1) ; (iter_expression x2)
and iter_class_declaration c1 = (iter_class_infos iter_class_expr) c1
and iter_module_type = fun r1 -> () ; iter_module_type_desc r1.pmty_desc  ; (fun _ -> ()) r1.pmty_loc  ; iter_attributes r1.pmty_attributes 
and iter_module_type_desc c1 =
  match c1 with
  | Pmty_ident(x) -> (iter_loc iter_longident) x
  | Pmty_signature(x) -> iter_signature x
  | Pmty_functor(x1,x2,x3) -> () ; ((iter_loc (fun _ -> ())) x1) ; ((iter_option iter_module_type) x2) ; (iter_module_type x3)  | Pmty_with(x1,x2) -> () ; (iter_module_type x1) ; ((iter_list iter_with_constraint) x2)  | Pmty_typeof(x) -> iter_module_expr x
  | Pmty_extension(x) -> iter_extension x
  | Pmty_alias(x) -> (iter_loc iter_longident) x

and iter_signature c1 = (iter_list iter_signature_item) c1
and iter_signature_item = fun r1 -> () ; iter_signature_item_desc r1.psig_desc  ; (fun _ -> ()) r1.psig_loc 
and iter_signature_item_desc c1 =
  match c1 with
  | Psig_value(x) -> iter_value_description x
  | Psig_type(x1,x2) -> () ; (iter_rec_flag x1) ; ((iter_list iter_type_declaration) x2)  | Psig_typext(x) -> iter_type_extension x
  | Psig_exception(x) -> iter_extension_constructor x
  | Psig_module(x) -> iter_module_declaration x
  | Psig_recmodule(x) -> (iter_list iter_module_declaration) x
  | Psig_modtype(x) -> iter_module_type_declaration x
  | Psig_open(x) -> iter_open_description x
  | Psig_include(x) -> iter_include_description x
  | Psig_class(x) -> (iter_list iter_class_description) x
  | Psig_class_type(x) -> (iter_list iter_class_type_declaration) x
  | Psig_attribute(x) -> iter_attribute x
  | Psig_extension(x1,x2) -> () ; (iter_extension x1) ; (iter_attributes x2)
and iter_module_declaration = fun r1 -> () ; (iter_loc (fun _ -> ())) r1.pmd_name  ; iter_module_type r1.pmd_type  ; iter_attributes r1.pmd_attributes  ; (fun _ -> ()) r1.pmd_loc 
and iter_module_type_declaration = fun r1 -> () ; (iter_loc (fun _ -> ())) r1.pmtd_name  ; (iter_option iter_module_type) r1.pmtd_type  ; iter_attributes r1.pmtd_attributes  ; (fun _ -> ()) r1.pmtd_loc 
and iter_open_description = fun r1 -> () ; (iter_loc iter_longident) r1.popen_lid  ; iter_override_flag r1.popen_override  ; (fun _ -> ()) r1.popen_loc  ; iter_attributes r1.popen_attributes 
and iter_include_infos : 'a. ('a -> unit) -> 'a include_infos -> unit = fun iter_a r1 -> () ; iter_a r1.pincl_mod  ; (fun _ -> ()) r1.pincl_loc  ; iter_attributes r1.pincl_attributes 
and iter_include_description c1 = (iter_include_infos iter_module_type) c1
and iter_include_declaration c1 = (iter_include_infos iter_module_expr) c1
and iter_with_constraint c1 =
  match c1 with
  | Pwith_type(x1,x2) -> () ; ((iter_loc iter_longident) x1) ; (iter_type_declaration x2)  | Pwith_module(x1,x2) -> () ; ((iter_loc iter_longident) x1) ; ((iter_loc iter_longident) x2)  | Pwith_typesubst(x) -> iter_type_declaration x
  | Pwith_modsubst(x1,x2) -> () ; ((iter_loc (fun _ -> ())) x1) ; ((iter_loc iter_longident) x2)
and iter_module_expr = fun r1 -> () ; iter_module_expr_desc r1.pmod_desc  ; (fun _ -> ()) r1.pmod_loc  ; iter_attributes r1.pmod_attributes 
and iter_module_expr_desc c1 =
  match c1 with
  | Pmod_ident(x) -> (iter_loc iter_longident) x
  | Pmod_structure(x) -> iter_structure x
  | Pmod_functor(x1,x2,x3) -> () ; ((iter_loc (fun _ -> ())) x1) ; ((iter_option iter_module_type) x2) ; (iter_module_expr x3)  | Pmod_apply(x1,x2) -> () ; (iter_module_expr x1) ; (iter_module_expr x2)  | Pmod_constraint(x1,x2) -> () ; (iter_module_expr x1) ; (iter_module_type x2)  | Pmod_unpack(x) -> iter_expression x
  | Pmod_extension(x) -> iter_extension x

and iter_structure c1 = (iter_list iter_structure_item) c1
and iter_structure_item = fun r1 -> () ; iter_structure_item_desc r1.pstr_desc  ; (fun _ -> ()) r1.pstr_loc 
and iter_structure_item_desc c1 =
  match c1 with
  | Pstr_eval(x1,x2) -> () ; (iter_expression x1) ; (iter_attributes x2)  | Pstr_value(x1,x2) -> () ; (iter_rec_flag x1) ; ((iter_list iter_value_binding) x2)  | Pstr_primitive(x) -> iter_value_description x
  | Pstr_type(x1,x2) -> () ; (iter_rec_flag x1) ; ((iter_list iter_type_declaration) x2)  | Pstr_typext(x) -> iter_type_extension x
  | Pstr_exception(x) -> iter_extension_constructor x
  | Pstr_module(x) -> iter_module_binding x
  | Pstr_recmodule(x) -> (iter_list iter_module_binding) x
  | Pstr_modtype(x) -> iter_module_type_declaration x
  | Pstr_open(x) -> iter_open_description x
  | Pstr_class(x) -> (iter_list iter_class_declaration) x
  | Pstr_class_type(x) -> (iter_list iter_class_type_declaration) x
  | Pstr_include(x) -> iter_include_declaration x
  | Pstr_attribute(x) -> iter_attribute x
  | Pstr_extension(x1,x2) -> () ; (iter_extension x1) ; (iter_attributes x2)
and iter_value_binding = fun r1 -> () ; iter_pattern r1.pvb_pat  ; iter_expression r1.pvb_expr  ; iter_attributes r1.pvb_attributes  ; (fun _ -> ()) r1.pvb_loc 
and iter_module_binding = fun r1 -> () ; (iter_loc (fun _ -> ())) r1.pmb_name  ; iter_module_expr r1.pmb_expr  ; iter_attributes r1.pmb_attributes  ; (fun _ -> ()) r1.pmb_loc 
let rec iter_toplevel_phrase c1 =
  match c1 with
  | Ptop_def(x) -> iter_structure x
  | Ptop_dir(x1,x2) -> () ; ((fun _ -> ()) x1) ; (iter_directive_argument x2)
and iter_directive_argument c1 =
  match c1 with
  | Pdir_none -> ()
  | Pdir_string(x) -> (fun _ -> ()) x
  | Pdir_int(x1,x2) -> () ; ((fun _ -> ()) x1) ; ((iter_option (fun _ -> ())) x2)  | Pdir_ident(x) -> iter_longident x
  | Pdir_bool(x) -> (fun _ -> ()) x

