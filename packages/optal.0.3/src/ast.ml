open Common_types

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Range

type bool_binop =
  | And
  | Or

type constant =
  | Cint of int
  | Cfloat of float

type pattern_expr =
  | Pconst of constant * Loc.t
  | Pvar of id * Loc.t
  | Ptuple of pattern_expr list * Loc.t

type expr =
  | Const of constant * Loc.t
  | Var of id * Loc.t
  | Sum of pattern list * expr option * expr * Loc.t
  | Binop of binop * expr * expr * Loc.t
  | Field of expr * id * Loc.t
  | Access of expr * expr * Loc.t
  | Last of expr * Loc.t
  | IndexOf of expr * Loc.t
  | Max of expr * Loc.t
  | Etuple of expr list * Loc.t
  | Cmp of cmp * expr * expr * Loc.t
  | Bool_binop of bool_binop * expr * expr * Loc.t
  | Not of expr * Loc.t
  | Eobject of (id * expr) list * Loc.t
  | Str of id * Loc.t

and pattern =
  { p_expr : pattern_expr list;
    p_id : expr;
    p_loc : Loc.t }

type dvar =
  { dvar_typ : typ;
    dvar_id : id;
    dvar_index : expr list;
    dvar_loc : Loc.t }

let expr_loc = function
  | Const (_,l)
  | Var (_,l)
  | Sum (_,_,_,l)
  | Binop (_,_,_,l)
  | Field (_,_,l)
  | Access (_,_,l)
  | Last (_,l)
  | Max (_,l)
  | IndexOf (_,l)
  | Etuple (_,l)
  | Cmp (_,_,_,l)
  | Bool_binop (_,_,_,l)
  | Eobject (_,l)
  | Not (_,l) -> l
  | Str (_,l) -> l

type alias =
  { alias_id : id;
    alias_typ : typ option;
    alias_exp : expr;
    alias_loc : Loc.t }

type indexing =
  { index_id : string;
    index_index : expr list;
    index_loc : Loc.t }

type declaration =
  | Dvar  of dvar
  | Alias of alias
  | Indexing of indexing

type objective =
  { obj_dir : direction;
    obj_expr : expr }

type constr =
  { constr_patterns: pattern list;
    constr_restrict: expr option;
    constr_expr : expr list;
    constr_label : string option;
    constr_loc : Loc.t }

type file =
  { declarations : declaration list;
    objective : objective;
    constraints : constr list; }

