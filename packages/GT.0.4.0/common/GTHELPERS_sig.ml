(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** Signature of AST building functions that are required to support a new backend *)


module type S = sig

type loc
type class_structure
type case
type class_declaration
type lab_decl
type module_declaration
type module_type_declaration
type type_arg

val loc_from_caml: Ppxlib.location -> loc
val noloc: loc
val named_type_arg : loc:loc -> string -> type_arg
val typ_arg_of_core_type : Ppxlib.core_type -> type_arg

(** for Pat *)
module rec Pat :
  sig
    type t
    val any : loc:loc -> t
    val unit: loc:loc -> t
    val var : loc:loc -> string -> t
    val access2: loc:loc -> string -> string -> t
    val alias:  loc:loc -> t -> string -> t
    val sprintf : loc:loc -> ('a, unit, string, t) format4 -> 'a
    val of_longident : loc:loc -> Ppxlib.longident -> t
    val constr : loc:loc -> string -> t list -> t
    val constr_record : loc:loc -> string -> (string*t) list -> t
    val constraint_ : loc:loc -> t -> Typ.t -> t
    val variant: loc:loc -> string -> t list -> t
    val tuple:   loc:loc -> t list -> t
    val record:  loc:loc -> (Ppxlib.longident * t) list -> t
    val record1: loc:loc -> Ppxlib.longident -> t
    (* #lident *)
    val type_:  loc:loc -> Ppxlib.longident -> t
  end
and Exp :
  sig
    type t
    val from_caml: Ppxlib.expression -> t
    val ident : loc:loc -> string -> t
    val of_longident: loc:loc -> Ppxlib.longident -> t
    val sprintf : loc:loc -> ('a, unit, string, t) format4 -> 'a
    val access  : loc:loc -> string -> string -> t

    val unit : loc:loc -> t
    val int_const : loc:loc -> int -> t
    val string_const : loc:loc -> string -> t
    val record : loc:loc -> (Ppxlib.longident * t) list -> t
    val record1: loc:loc -> Ppxlib.longident -> t -> t

    val app : loc:loc -> t -> t -> t
    val app_lab  : loc:loc -> t -> string -> t -> t
    val app_list : loc:loc -> t -> t list -> t

    (* TODO: rename this to `field` or something *)
    val acc      : loc:loc -> t -> Ppxlib.longident -> t

    val field : loc:loc -> t -> Ppxlib.longident -> t
    (* val function_: loc:loc -> (Pat.t * t) list -> t *)
    val fun_ : loc:loc -> Pat.t -> t -> t
    val fun_list   : loc:loc -> Pat.t list -> t -> t
    val fun_list_l : loc:loc -> (string * Exp.t) list -> t -> t
    val match_  : loc:loc -> t -> case list -> t
    val object_ : loc:loc -> class_structure -> t
    val record :  loc:loc -> (Ppxlib.longident * t) list -> t
    val send : loc:loc -> t -> string -> t
    val new_ : loc:loc -> Ppxlib.longident -> t

    val variant:   loc:loc -> string -> t list -> t
    val construct: loc:loc -> Ppxlib.longident -> t list -> t
    val tuple:     loc:loc -> t list -> t
    val let_one:   loc:loc -> ?rec_:bool -> Pat.t -> t -> t -> t
    val let_:      loc:loc -> ?rec_:bool -> (Pat.t * t) list -> t -> t

    val assert_false: loc:loc -> t
    val objmagic_unit: loc:loc -> t
    val failwith_: loc:loc -> string -> t
    val true_ : loc:loc -> t
    val false_: loc:loc -> t
    val list  : loc:loc -> t list -> t
    (* val new_type: loc:loc -> string -> t -> t *)
    val constraint_: loc:loc -> t -> Typ.t -> t
  end
and Typ :
  sig
    type t
    val from_caml: Ppxlib.core_type -> t
    val use_tdecl: Ppxlib.type_declaration -> t
    val of_type_arg: loc:loc -> type_arg -> t
    val of_longident : loc:loc -> Ppxlib.longident -> t
    val access2 : loc:loc -> string -> string -> t
    val sprintf : loc:loc -> ('a, unit, string, t) format4 -> 'a
    val ident : loc:loc -> string -> t

    val var : loc:loc -> string -> t
    val any : loc:loc -> t
    val unit : loc:loc -> t
    val string : loc:loc -> t
    val constr : loc:loc -> Ppxlib.longident -> t list -> t
    val pair   : loc:loc -> t -> t -> t

    val tuple : loc:loc -> t list -> t
    val class_ : loc:loc -> Ppxlib.longident -> t list -> t
    val object_ : loc:loc -> Ppxlib.closed_flag -> (string * t) list -> t
    val arrow : loc:loc -> t -> t -> t
    val chain_arrow : loc:loc -> t list -> t
    val variant : loc:loc -> ?is_open:bool -> Ppxlib.row_field list -> t
    val variant_of_t : loc:loc -> t -> t
    val alias : loc:loc -> t -> string -> t
    val poly  : loc:loc -> string list -> t -> t
    val map: onvar:(string -> t option) -> t -> t
    (* Inherit type using [> t ] with optional as annotation *)
    val openize: loc:loc -> ?as_:string -> t -> t

    val to_type_arg_exn: t -> type_arg
    val to_type_arg: t -> type_arg option
  end
and Cf :
  sig
    type t
    val method_concrete : loc:loc -> string -> Exp.t -> t
    val method_virtual  : loc:loc -> string -> Typ.t -> t
    val inherit_:         loc:loc -> ?as_:(string option) -> Cl.t -> t
    val constraint_:      loc:loc -> Typ.t -> Typ.t -> t
  end
and Cty : sig
  type t
  val arrow: loc:loc -> Typ.t -> t -> t
  val constr:loc:loc -> Ppxlib.longident -> Typ.t list -> t
end
and Ctf : (* class_sig_item *)
  sig
    type t
    val inherit_   : loc:loc -> Cty.t -> t
    val method_    : loc:loc -> ?virt:bool -> string -> Typ.t -> t
    val constraint_: loc:loc -> Typ.t -> Typ.t -> t
  end
and Str :
  sig
    type t
    val of_tdecls : loc:loc -> Ppxlib.type_declaration -> t
    val single_value : loc:loc -> Pat.t -> Exp.t -> t
    val values: loc:loc -> ?rec_flag:Ppxlib.rec_flag -> Vb.t list -> t
    val of_vb: loc:loc  -> ?rec_flag:Ppxlib.rec_flag -> Vb.t -> t
    val class_single :
      loc:loc ->
      name:string ->
      ?virt:bool ->
      ?wrap:(Cl.t -> Cl.t) ->
      params:type_arg list -> Cf.t list -> t
    val tdecl : loc:loc -> name:string -> params:string list -> Typ.t -> t
    val tdecl_abstr: loc:loc -> string -> string option list -> t
    val tdecl_record: loc:loc -> name:string -> params:string list ->
      lab_decl list -> t
    val of_class_declarations: loc:loc -> class_declaration list -> t
    (* val functor1 : loc:loc -> string -> param:string -> Sig.t list -> t list -> t *)
    val simple_gadt : loc:loc -> name:string -> params_count:int ->
      (string * Typ.t) list -> t
    val module_  : loc:loc -> string -> Me.t -> t
    val modtype  : loc:loc -> module_type_declaration -> t
    val include_ : loc:loc -> Me.t -> t
  end
and Sig :
  sig
    type t
    val of_tdecls : loc:loc -> Ppxlib.type_declaration -> t
    val value : loc:loc -> name:string -> Typ.t -> t
    val class_: loc:loc -> name:string ->
      params: type_arg list ->
      ?virt:bool ->
      ?wrap:(Cty.t -> Cty.t) ->
      Ctf.t list ->
      t
    val functor1: loc:loc -> string -> param:string -> t list -> t list -> t
    val simple_gadt : loc:loc -> name:string -> params_count:int ->
      (string * Typ.t) list -> t
    val tdecl_abstr: loc:loc -> string -> string option list -> t
    val module_: loc:loc -> module_declaration -> t
    val modtype: loc:loc -> module_type_declaration -> t
  end
and Me : sig
  type t
  val structure: loc:loc -> Str.t list -> t
  val ident: loc:loc -> Longident.t -> t
  val apply: loc:loc -> t -> t -> t
  (* val functor_ : loc:loc -> string -> Mt.t option -> t -> t *)
end
and Mt : sig
  type t
  val ident: loc:loc -> Longident.t -> t
  val signature: loc:loc -> Sig.t list -> t
  (* val functor_:  loc:loc -> string -> t option -> t -> t *)
  val with_: loc:loc -> t -> WC.t list -> t
end
and WC : sig
  type t
  val typ : loc:loc -> params:string list -> string -> Typ.t -> t
end
and Cl :    (* class_expr *)
  sig
    type t
    val fun_:     loc:loc -> Pat.t -> t -> t
    val fun_list: loc:loc -> Pat.t list -> t -> t
    val constr :  loc:loc -> Longident.t -> Typ.t list -> t
    val apply  :  loc:loc -> t -> Exp.t list -> t
  end

and Vb :
  sig
    type t
  end

val class_declaration: loc:loc ->
  name:string ->
  ?virt:bool ->
  ?wrap:(Cl.t -> Cl.t) ->
  params:type_arg list -> Cf.t list -> class_declaration

val value_binding: loc:loc -> pat:Pat.t -> expr:Exp.t -> Vb.t
val case: lhs:Pat.t -> rhs:Exp.t -> case
val class_structure : self:Pat.t -> fields:Cf.t list -> class_structure
val lab_decl: loc:loc -> string -> bool -> Typ.t -> lab_decl
val module_declaration: loc:loc -> name:string -> Mt.t -> module_declaration
val module_type_declaration: loc:loc -> name:string -> Mt.t option -> module_type_declaration
val use_new_type: loc:loc -> string -> Exp.t -> Exp.t

(* if argument is polymorphic variant type then make it open *)
val openize_poly: loc:loc ->  Typ.t -> Typ.t

(* val closize_poly: Typ.t -> Typ.t *)

val prepare_param_triples : loc:loc ->
  extra:Typ.t ->
  ?inh:(loc:loc -> string -> Typ.t) ->
  ?syn:(loc:loc -> string -> Typ.t) ->
  ?default_inh:  Typ.t ->
  ?default_syn:  Typ.t ->
  string list -> Typ.t list

val typ_vars_of_typ : Typ.t -> string list

end
