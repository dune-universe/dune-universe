(* Time-stamp: <modified the 29/08/2019 (at 14:41) by Erwan Jahier> *)


(** (Raw) Abstract syntax tree of source Lustre Core programs. *)

open Lxm


(**********************************************************************************)
type clock_exp =
  | Base
  | NamedClock of Lv6Id.clk srcflagged

(**********************************************************************************)
(** [type_exp] is used to type flow, parameters, constants. *)
type type_exp = type_exp_core srcflagged
and
  type_exp_core =
  | Bool_type_exp
  | Int_type_exp
  | Real_type_exp
  | Named_type_exp of Lv6Id.idref 
  | Array_type_exp of (type_exp * val_exp)


and node_info = {
  name    : Lv6Id.t;
  static_params : static_param srcflagged list;
  vars    : node_vars option;  (* aliased node may have no i/o decl *)
  (* consts  : ICI A FAIRE *)
  loc_consts : (Lxm.t * const_info) list;
  def     : node_def;
  has_mem : bool;
  is_safe : bool; (* safe <=> no side-effect are performed *)
}

and static_param =
  | StaticParamType  of Lv6Id.t
  | StaticParamConst of Lv6Id.t * type_exp
  | StaticParamNode  of
      (Lv6Id.t * var_info srcflagged list * var_info srcflagged list * has_mem_flag * is_safe_flag)

and node_vars = {
  inlist  : Lv6Id.t list;
  outlist : Lv6Id.t list;
  loclist : Lv6Id.t list option; (* abstract/ext node have no body *)
  vartable: var_info_table;
}
and var_info_table = (Lv6Id.t, var_info srcflagged) Hashtbl.t
and var_info = {
  var_nature : var_nature;
  var_name   : Lv6Id.t;
  var_number : int;
  var_type   : type_exp;
  var_clock  : clock_exp 
}
and var_nature =
  | VarInput
  | VarOutput
  | VarLocal

and node_def = 
  | Extern
  | Abstract
  | Body of node_body
  | Alias of by_pos_op srcflagged

and node_body = {
  asserts : (val_exp srcflagged) list;
  eqs     : (eq_info srcflagged) list;
}
and has_mem_flag = bool
and is_safe_flag = bool


and eq_info = (left_part list * val_exp)

and left_part = 
  | LeftVar of (Lv6Id.t srcflagged)
  | LeftField of (left_part * (Lv6Id.t srcflagged))
  | LeftArray of (left_part * (val_exp srcflagged))  
  | LeftSlice of (left_part * (slice_info srcflagged))

and slice_info = {
  si_first : val_exp ;
  si_last  : val_exp ;
  si_step  : val_exp option ;
}

and by_pos_op =
(* zeroaire *)
  | Predef_n of AstPredef.op srcflagged
  | CALL_n of node_exp srcflagged (* e.g., a_node<<xx>> *)
  | IDENT_n  of Lv6Id.idref (* constant or variable *)

  | PRE_n
  | ARROW_n
  | FBY_n
  | CURRENT_n
      
  | WHEN_n of clock_exp
  | TUPLE_n
  | WITH_n of val_exp * val_exp * val_exp

  | CONCAT_n
  | HAT_n
  | ARRAY_n
  | STRUCT_ACCESS_n of Lv6Id.t

  | ARRAY_ACCES_n of val_exp
  | ARRAY_SLICE_n of slice_info   

(************************************************)
(* Info associées aux expressions               *)
(************************************************)
(* Vision "fonctionnelle" des val_exp :         *)
(* Une exp. est une application d'operation :   *)
(* - avec passage par position, auquel cas les  *)
(* opérandes sont des val_exp                   *)
(* - avec passage par nom, auquel cas les       *)
(* opérandes sont des Lv6Id.t * val_exp         *)
(************************************************)
(* and val_exp = by_pos_op srcflagged * operands *)

and val_exp = 
  | CallByPos  of (by_pos_op  srcflagged  * operands) 
  | CallByName of (by_name_op srcflagged  * (Lv6Id.t srcflagged * val_exp) list)
  | Merge_n of val_exp srcflagged * (Lv6Id.idref srcflagged * val_exp) list
  | Merge_bool_n of val_exp srcflagged * val_exp * val_exp
   
and operands = Oper of val_exp list
(* Virer cet Oper ? Non, sinon ca boucle... *)

and by_name_op =
  | STRUCT_n of Lv6Id.idref
  | STRUCT_WITH_n of Lv6Id.idref * Lv6Id.idref
  | STRUCT_anonymous_n
      (* for backward compatibility with lv4 *)

and node_exp = 
      (Lv6Id.idref * (static_arg srcflagged list))
	
(** Params statiques effectifs :
    - val_exp (pour les constantes)
    - type_exp (pour les types)
    - node_exp (pour les node)
    - ident : a résoudre, peut etre const, type ou node 
*)
and static_arg =
  | StaticArgLv6Id of Lv6Id.idref
  | StaticArgConst of val_exp
  | StaticArgType  of type_exp

  | StaticArgNode  of by_pos_op
(*   | StaticArgFunc  of node_exp *)


(**********************************************************************************)

(** constant *)

and const_info = 
  | ExternalConst  of (Lv6Id.t * type_exp * val_exp option)
  | EnumConst      of (Lv6Id.t * type_exp)
  | DefinedConst   of (Lv6Id.t * type_exp option * val_exp)

(** Type *)

type field_info = {
  fd_name  : Lv6Id.t ;
  fd_type  : type_exp ;
  fd_value : val_exp option
}
type struct_type_info = {
  st_name    : Lv6Id.t ;
  st_flist   : Lv6Id.t list; (* field name list *)
  st_ftable  : (Lv6Id.t, field_info srcflagged)  Hashtbl.t 
}
type type_info =
  | ExternalType of (Lv6Id.t)
  | AliasedType  of (Lv6Id.t * type_exp)
  | EnumType     of (Lv6Id.t * Lv6Id.t srcflagged list)
  | StructType   of struct_type_info
  | ArrayType    of (Lv6Id.t * type_exp * val_exp)

(** Operator *)

type item_ident =
  | ConstItem of Lv6Id.t
  | TypeItem  of Lv6Id.t
  | NodeItem  of Lv6Id.t * static_param srcflagged list
      
type item_info =
    ConstInfo of const_info
  | TypeInfo  of type_info
  | NodeInfo  of node_info

(* to be used for error msgs only...*)
let rec string_of_type_exp x = 
  match x.it with
    | Bool_type_exp -> "bool"
    | Int_type_exp  -> "int"
    | Real_type_exp -> "real"
    | Named_type_exp id -> (Lv6Id.string_of_idref false id)
    | Array_type_exp (te, _sz) -> (string_of_type_exp te) ^ "^ ..."
       

let string_of_var_nature = function
  | VarInput -> "input"
  | VarOutput -> "output"
  | VarLocal -> "local"


let lxm_of_val_exp = function
  | CallByPos(op,_)  -> op.src
  | CallByName(op,_) -> op.src
  | Merge_n(ve,_)  -> ve.src
  | Merge_bool_n(id,_,_) -> id.src

