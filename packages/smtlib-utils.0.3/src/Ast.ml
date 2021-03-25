(* This file is free software. See file "license" for more details. *)

(** {1 Trivial AST for parsing} *)

let pp_to_string pp x = Format.asprintf "%a@?" pp x

type var = string
type ty_var = string

(** Polymorphic types *)
type ty =
  | Ty_bool
  | Ty_real
  | Ty_app of ty_var * ty list
  | Ty_arrow of ty list * ty

type typed_var = var * ty

type arith_op =
  | Leq
  | Lt
  | Geq
  | Gt
  | Add
  | Minus
  | Mult
  | Div

(** {2 AST: S-expressions with locations} *)
type term =
  | True
  | False
  | Const of string
  | Arith of arith_op * term list
  | App of string * term list
  | HO_app of term * term (* higher-order application *)
  | Match of term * match_branch list
  | If of term * term * term
  | Let of (var * term) list * term
  | Is_a of string * term (* tester: is-constructor(term) *)
  | Fun of typed_var * term
  | Eq of term * term
  | Imply of term * term
  | And of term list
  | Or of term list
  | Not of term
  | Distinct of term list
  | Cast of term * ty (* type cast *)
  | Forall of (var * ty) list * term
  | Exists of (var * ty) list * term
  | Attr of term * (string * string) list (* term + attributes *)

and match_branch =
  | Match_default of term
  | Match_case of string * var list * term

type cstor = {
  cstor_ty_vars: ty_var list;
  cstor_name: string;
  cstor_args: (string * ty) list; (* selector+type *)
}

type 'arg fun_decl = {
  fun_ty_vars: ty_var list;
  fun_name: string;
  fun_args: 'arg list;
  fun_ret: ty;
}

type fun_def = {
  fr_decl: typed_var fun_decl;
  fr_body: term;
}

type funs_rec_def = {
  fsr_decls: typed_var fun_decl list;
  fsr_bodies: term list;
}

type prop_literal = string * bool

type statement = {
  stmt: stmt;
  loc: Loc.t option;
}

and stmt =
  | Stmt_set_logic of string
  | Stmt_set_info of string * string
  | Stmt_set_option of string list
  | Stmt_decl_sort of string * int (* arity *)
  | Stmt_decl of ty fun_decl
  | Stmt_fun_def of fun_def
  | Stmt_fun_rec of fun_def
  | Stmt_funs_rec of funs_rec_def
  | Stmt_data of ((string * int) * cstor list) list
  | Stmt_assert of term
  | Stmt_get_assertions
  | Stmt_get_assignment
  | Stmt_get_info of string
  | Stmt_get_model
  | Stmt_get_option of string
  | Stmt_get_proof
  | Stmt_get_unsat_assumptions
  | Stmt_get_unsat_core
  | Stmt_get_value of term list
  | Stmt_check_sat
  | Stmt_check_sat_assuming of prop_literal list
  | Stmt_pop of int
  | Stmt_push of int
  | Stmt_reset
  | Stmt_reset_assertions
  | Stmt_exit

(** {2 Errors} *)

exception Parse_error of Loc.t option * string

let () = Printexc.register_printer
    (function
      | Parse_error (loc, msg) ->
        let pp out () =
          Format.fprintf out "parse error at %a:@ %s" Loc.pp_opt loc msg
        in
        Some (pp_to_string pp ())
      | _ -> None)

let parse_error ?loc msg = raise (Parse_error (loc, msg))
let parse_errorf ?loc msg = Format.ksprintf (parse_error ?loc) msg

(** {2 Constructors} *)

let ty_bool = Ty_bool
let ty_app s l = Ty_app (s,l)
let ty_const s = ty_app s []
let ty_real = Ty_real
let ty_arrow_l args ret = if args=[] then ret else Ty_arrow (args, ret)
let ty_arrow a b = ty_arrow_l [a] b

let true_ = True
let false_ = False
let const s = Const s
let app f l = App (f,l)
let ho_app a b = HO_app (a,b)
let ho_app_l a l = List.fold_left ho_app a l
let match_ u l = Match (u,l)
let if_ a b c = If(a,b,c)
let fun_ v t = Fun (v,t)
let fun_l = List.fold_right fun_
let let_ l t = Let (l,t)
let eq a b = Eq (a,b)
let imply a b = Imply(a,b)
let is_a c t = Is_a (c,t)
let and_ l = And l
let or_ l = Or l
let distinct l = Distinct l
let cast t ~ty = Cast (t, ty)
let forall vars f = match vars with [] -> f | _ -> Forall (vars, f)
let exists vars f = match vars with [] -> f | _ -> Exists (vars, f)
let attr t l = match l with [] -> t | _ -> Attr (t, l)
let rec not_ t = match t with
  | Forall (vars,u) -> exists vars (not_ u)
  | Exists (vars,u) -> forall vars (not_ u)
  | _ -> Not t

let arith op l = Arith (op,l)

let _mk ?loc stmt = { loc; stmt }

let mk_cstor ~vars name l : cstor = { cstor_ty_vars=vars; cstor_name=name; cstor_args=l }
let mk_fun_decl ~ty_vars f args ret =
  { fun_ty_vars=ty_vars; fun_name=f;
    fun_args=args; fun_ret=ret; }
let mk_fun_rec ~ty_vars f args ret body =
  { fr_decl=mk_fun_decl ~ty_vars f args ret; fr_body=body; }

let decl_sort ?loc s ~arity = _mk ?loc (Stmt_decl_sort (s, arity))
let decl_fun ?loc ~tyvars f ty_args ty_ret =
  let d = {fun_ty_vars=tyvars; fun_name=f; fun_args=ty_args; fun_ret=ty_ret} in
  _mk ?loc (Stmt_decl d)
let fun_def ?loc fr = _mk ?loc (Stmt_fun_def fr)
let fun_rec ?loc fr = _mk ?loc (Stmt_fun_rec fr)
let funs_rec ?loc decls bodies = _mk ?loc (Stmt_funs_rec {fsr_decls=decls; fsr_bodies=bodies})
let data ?loc l = _mk ?loc (Stmt_data l)
let data_zip ?loc decls cstors =
  if List.length decls <> List.length cstors then (
    parse_errorf ?loc
      "declare-datatypes: mismatched lengths (%d types, %d lists of cstors)"
      (List.length decls) (List.length cstors)
  );
  _mk ?loc (Stmt_data (List.combine decls cstors))
let assert_ ?loc t = _mk ?loc (Stmt_assert t)
let check_sat ?loc () = _mk ?loc Stmt_check_sat
let check_sat_assuming ?loc l = _mk ?loc @@ Stmt_check_sat_assuming l
let exit ?loc () = _mk ?loc Stmt_exit
let set_logic ?loc l = _mk ?loc @@ Stmt_set_logic l
let set_info ?loc a b = _mk ?loc @@ Stmt_set_info (a,b)
let get_info ?loc s = _mk ?loc @@ Stmt_get_info s
let set_option ?loc l = _mk ?loc @@ Stmt_set_option l
let get_option ?loc s = _mk ?loc @@ Stmt_get_option s
let push ?loc s = _mk ?loc @@ Stmt_push s
let pop ?loc s = _mk ?loc @@ Stmt_pop s
let get_proof ?loc () = _mk ?loc @@ Stmt_get_proof
let get_model ?loc () = _mk ?loc @@ Stmt_get_model
let get_assertions ?loc () = _mk ?loc @@ Stmt_get_assertions
let get_assignment ?loc () = _mk ?loc @@ Stmt_get_assignment
let get_value ?loc l = _mk ?loc @@ Stmt_get_value l
let get_unsat_core ?loc () = _mk ?loc @@ Stmt_get_unsat_core
let get_unsat_assumptions ?loc () = _mk ?loc @@ Stmt_get_unsat_assumptions
let reset ?loc () = _mk ?loc Stmt_reset
let reset_assertions ?loc () = _mk ?loc Stmt_reset_assertions

let loc t = t.loc
let view t = t.stmt

let fpf = Format.fprintf

let pp_list pp out l =
  let rec aux l = match l with
  | x::((_::_) as l) ->
    pp out x;
    Format.fprintf out "@ ";
    aux l
  | x::[] -> pp out x
  | [] -> ()
  in
  aux l

let pp_str out s = Format.pp_print_string out s

let pp_tyvar = pp_str

let rec pp_ty out (ty:ty) = match ty with
  | Ty_bool -> pp_str out "Bool"
  | Ty_real -> pp_str out "Real"
  | Ty_app (s,[]) -> pp_str out s
  | Ty_app (s,l) -> Format.fprintf out "(@[<hv1>%s@ %a@])" s (pp_list pp_ty) l
  | Ty_arrow (args,ret) ->
    fpf out "(@[=>@ %a@ %a@])" (pp_list pp_ty) args pp_ty ret

let str_of_arith = function
  | Leq -> "<="
  | Lt -> "<"
  | Geq -> ">="
  | Gt -> ">"
  | Add -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"

let pp_arith out a = Format.pp_print_string out (str_of_arith a)

let _lvl_top = 0

let rec pp_term lvl out (t:term) =
  let lvl_q = 10 in
  let lvl_let = 20 in
  let lvl_match = 25 in
  let lvl_and = 30 in
  let lvl_or = 31 in
  let lvl_not = 32 in
  let lvl_app = 50 in

  let self = pp_term lvl in
  let self' lvl' = pp_term lvl' in
  let self_a = self' lvl_app in
  let pp_binding out (v,t) = fpf out "(@[%s@ %a@])" v (self' lvl_let) t in
  let fpf' lvl' out fmt =
    if lvl <> lvl' then (
      fpf out "(@["
    ) else (
      fpf out "("
    );
    Format.kfprintf
      (fun out -> if lvl <> lvl' then fpf out "@])" else fpf out ")")
      out fmt
  in
  match t with
  | True -> pp_str out "true"
  | False -> pp_str out "false"
  | Arith (op,l) ->
    Format.fprintf out "(@[<hv>%a@ %a@])" pp_arith op (pp_list self_a) l
  | Const s -> pp_str out s
  | App (f,l) -> fpf' lvl_app out "%s@ %a" f (pp_list self_a) l
  | HO_app (a,b) -> fpf' lvl_app out "@@@ %a@ %a" (self' lvl_app) a (self' lvl_app) b
  | Match (lhs,cases) ->
    let pp_case out = function
      | Match_default rhs -> fpf out "(@[<1>_@ %a@])" (self' lvl_match) rhs
      | Match_case (c,[],rhs) ->
        fpf out "(@[<1>%s@ %a@])" c (self' _lvl_top) rhs
      | Match_case (c,vars,rhs) ->
        fpf out "(@[<1>(@[%s@ %a@])@ %a@])"
          c (pp_list pp_str) vars (self' _lvl_top) rhs
    in
    fpf' lvl_match out "match@ %a@ (@[<v>%a@])" (self' _lvl_top) lhs
      (pp_list pp_case) cases
  | If (a,b,c) ->
    fpf' lvl_app out "ite %a@ %a@ %a" self_a a self_a b self_a c
  | Fun (v,body) ->
    fpf' lvl_q out "lambda @ (%a)@ %a" pp_typed_var v (self' lvl_q) body
  | Let (l,t) ->
    fpf' lvl_let out "let@ (@[%a@])@ %a" (pp_list pp_binding) l (self' lvl_let) t
  | Is_a (c,t) -> fpf out "(@[(@[_ is@ %s@])@ %a@])" c self t
  | Eq (a,b) -> fpf out "(@[=@ %a@ %a@])" self a self b
  | Imply (a,b) ->
    fpf' lvl_or out "=>@ %a@ %a" (self' lvl_or) a (self' lvl_or) b
  | And l -> fpf' lvl_and out "and@ %a" (pp_list @@ self' lvl_and) l
  | Or l -> fpf' lvl_or out "or@ %a" (pp_list @@ self' lvl_or) l
  | Not t -> fpf' lvl_not out "not@ %a" (self' lvl_not) t
  | Distinct l -> fpf out "(@[distinct@ %a@])" (pp_list self) l
  | Cast (t, ty) -> fpf out "(@[<hv>as@ @[%a@]@ @[%a@]@])" self t pp_ty ty
  | Forall (vars,f) ->
    fpf' lvl_q out "forall@ (@[%a@])@ %a" (pp_list pp_typed_var) vars (self' lvl_q) f
  | Exists (vars,f) ->
    fpf' lvl_q out "exists@ (@[%a@])@ %a" (pp_list pp_typed_var) vars (self' lvl_q) f
  | Attr (t, l) ->
    let pp_attr out (x,y) = Format.fprintf out "%s %s" x y in
    fpf' lvl_app out "! @[%a@] %a" (self' lvl_app) t (pp_list pp_attr) l
and pp_typed_var out (v,ty) =
  fpf out "(@[%s@ %a@])" v pp_ty ty

let pp_term out t = pp_term _lvl_top out t

let pp_par pp_x out (ty_vars,x) = match ty_vars with
  | [] -> pp_x out x
  | _ ->
    fpf out "(@[par (@[%a@])@ (%a)@])" (pp_list pp_tyvar) ty_vars pp_x x

let pp_fun_decl pp_arg out fd =
  fpf out "%s@ (@[%a@])@ %a"
    fd.fun_name (pp_list pp_arg) fd.fun_args pp_ty fd.fun_ret

let pp_fr out fr =
  fpf out "@[<2>%a@ %a@]" (pp_fun_decl pp_typed_var) fr.fr_decl pp_term fr.fr_body

let pp_prop_lit out (s,b) =
  if b then fpf out "%s" s else fpf out "(not %s)" s

let pp_stmt out (st:statement) = match view st with
  | Stmt_set_info (a,b) -> fpf out "(@[set-info@ %a@ %a@])" pp_str a pp_str b
  | Stmt_set_logic s -> fpf out "(@[set-logic@ %a@])" pp_str s
  | Stmt_set_option l -> fpf out "(@[set-option@ %a@])" (pp_list pp_str) l
  | Stmt_decl_sort (s,n) -> fpf out "(@[declare-sort@ %s %d@])" s n
  | Stmt_assert t -> fpf out "(@[assert@ %a@])" pp_term t
  | Stmt_decl d ->
    fpf out "(@[declare-fun@ %a@])"
      (pp_par (pp_fun_decl pp_ty)) (d.fun_ty_vars,d)
  | Stmt_fun_def fr ->
    fpf out "(@[<2>define-fun@ %a@])"
      (pp_par pp_fr) (fr.fr_decl.fun_ty_vars, fr)
  | Stmt_fun_rec fr ->
    fpf out "(@[<2>define-fun-rec@ %a@])"
      (pp_par pp_fr) (fr.fr_decl.fun_ty_vars, fr)
  | Stmt_funs_rec fsr ->
    let pp_decl' out d = fpf out "(@[<2>%a@])" (pp_fun_decl pp_typed_var) d in
    fpf out "(@[<hv2>define-funs-rec@ (@[<v>%a@])@ (@[<v>%a@])@])"
      (pp_list pp_decl') fsr.fsr_decls (pp_list pp_term) fsr.fsr_bodies
  | Stmt_data l ->
    let pp_decl out (name,n) = fpf out "(@[%s %d@])" name n in
    let pp_cstor_arg out (sel,ty) = fpf out "(@[%s %a@])" sel pp_ty ty in
    let pp_cstor_raw out c =
      if c.cstor_args = []
      then fpf out "(%s)" c.cstor_name
      else fpf out "(@[<1>%s@ %a@])" c.cstor_name (pp_list pp_cstor_arg) c.cstor_args
    in
    let pp_cstor out c =
      pp_par pp_cstor_raw out (c.cstor_ty_vars, c)
    in
    let pp_cstors_l out cstors = fpf out "(@[<hv1>%a@])" (pp_list pp_cstor) cstors in
    let decls, cstors_l = List.split l in
    fpf out "(@[<hv2>declare-datatypes@ (@[%a@])@ (@[<v>%a@])@])"
      (pp_list pp_decl) decls
      (pp_list pp_cstors_l) cstors_l
  | Stmt_check_sat -> fpf out "(check-sat)"
  | Stmt_check_sat_assuming l ->
    fpf out "(@[<hv>check-sat-assuming@ %a@])" (pp_list pp_prop_lit) l
  | Stmt_reset -> fpf out "(reset)"
  | Stmt_reset_assertions -> fpf out "(reset-assertions)"
  | Stmt_get_assertions -> fpf out "(get-assertions)"
  | Stmt_get_assignment -> fpf out "(get-assignment)"
  | Stmt_get_proof -> fpf out "(get-proof)"
  | Stmt_get_info s -> fpf out "(@[get-info@ %s@])" s
  | Stmt_get_option s -> fpf out "(@[get-option@ %s@])" s
  | Stmt_get_model -> fpf out "(get-model)"
  | Stmt_get_unsat_core -> fpf out "(get-unsat-core)"
  | Stmt_get_unsat_assumptions -> fpf out "(get-unsat-assumptions)"
  | Stmt_get_value l -> fpf out "(@[get-value@ %a@])" (pp_list pp_term) l
  | Stmt_push n -> fpf out "(push %d)" n
  | Stmt_pop n -> fpf out "(pop %d)" n
  | Stmt_exit -> fpf out "(exit)"
