open Common_types
open Ast
open Lp
open Value

type error =
  | Not_linear
  | Type_error of (vtyp list) * vtyp
  | Bool_not_impossible of btyp
  | Bool_op_impossible of bool_binop * btyp * btyp
  | Operation_impossible of string * vtyp * vtyp
  | Comparison_impossible of cmp * vtyp * vtyp
  | Not_a_linear_constraint of cmp
  | Out_of_bound of int
  | General of string

exception Error of error * Loc.t

let error loc fmt =
  Printf.kprintf (fun s -> raise (Error (General s,loc))) fmt

let string_of_op = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Range -> "range"

let rec equal_value loc val1 val2 =
  match val1, val2 with
  | Vint i, Vint j -> i = j
  | Vstring s1, Vstring s2 -> s1 = s2
  | Varray a1, Varray a2 ->
    ( Array.length a1 = Array.length a2 )
    && List.for_all2 (equal_value loc) (Array.to_list a1) (Array.to_list a2)
  | Vobj o1, Vobj o2 ->
    let l1 = Object.bindings o1 in
    let l2 = Object.bindings o2 in
    List.for_all2 (fun (s1,v1) (s2,v2) -> s1 = s2 && equal_value loc v1 v2) l1 l2
  | _, _ ->
    raise (Error (Operation_impossible ("=", vtyp val1, vtyp val2), loc))

let eval_cmp op exp1 exp2 loc =
  let cmp_exp exp1 exp2 =
    let expr, cst = Expr.to_list (Expr.sub exp1 exp2) in
    match op with
    | Eq | Geq | Leq ->
      Constraints
        [{ label = None;
           expr = expr;
           bound_type = op;
           bound = -. cst }]
    | Neq | Gt | Lt ->
      raise (Error (Not_a_linear_constraint op, loc))
  in
  let cmp_cst exp1 exp2 =
    assert(Expr.is_constant exp1);
    assert(Expr.is_constant exp2);
    let (_,f1), (_,f2) = exp1, exp2 in
    let b = match op with
      | Eq -> f1 = f2
      | Neq -> f1 <> f2
      | Geq -> f1 >= f2
      | Leq -> f1 <= f2
      | Gt -> f1 > f2
      | Lt -> f1 < f2
    in
    Bool b
  in
  match exp1, exp2 with
  | Vlp_exp exp1, Vlp_exp exp2 when
      Expr.is_constant exp1 && Expr.is_constant exp2 ->
    cmp_cst exp1 exp2
  | Vlp_exp exp1, Vlp_exp exp2 -> cmp_exp exp1 exp2
  | Vlp_exp exp1, Vint i2  when
      Expr.is_constant exp1 ->
    cmp_cst exp1 (Expr.cst (float i2))
  | Vlp_exp exp1, Vint i2 -> cmp_exp exp1 (Expr.cst (float i2))
  | Vint i1, Vlp_exp exp2  when
      Expr.is_constant exp2 ->
    cmp_cst (Expr.cst (float i1)) exp2
  | Vint i1, Vlp_exp exp2 -> cmp_exp (Expr.cst (float i1)) exp2
  | Vint i1, Vint i2 ->
    let b = match op with
      | Eq -> i1 = i2
      | Neq -> i1 <> i2
      | Geq -> i1 >= i2
      | Leq -> i1 <= i2
      | Gt -> i1 > i2
      | Lt -> i1 < i2
    in
    Bool b
  | Vstring s1, Vstring s2 ->
    let b = match op with
      | Eq -> s1 = s2
      | Neq -> s1 <> s2
      | Geq | Leq | Lt | Gt ->
        raise (Error (Comparison_impossible (op,vtyp exp1,vtyp exp2),loc))
    in
    Bool b
  | _, _ ->
    match op with
    | Eq -> Bool (equal_value loc exp1 exp2)
    | _ -> raise (Error (Comparison_impossible (op,vtyp exp1,vtyp exp2),loc))

let eval_bool_binop op v1 v2 loc =
  match v1, v2 with
  | Constraints c1, Constraints c2 ->
    let cl = match op with
      | And -> c1 @ c2
      | _ ->
        (* maybe some other cases are possible *)
        raise (Error (Bool_op_impossible (op,btyp v1,btyp v2),loc)) in
    Constraints cl
  | Bool b1, Bool b2 ->
    let b = match op with
      | And -> b1 && b2
      | Or -> b1 || b2
    in
    Bool b
  | Bool b, Constraints c
  | Constraints c, Bool b ->
    match op with
    | Or ->
      if b then Bool true
      else Constraints c
    | And ->
      if b then Constraints c
      else Bool false

let rec match_pattern env pat value =
  match pat, value with
  | Pconst (Cint i, _), Vint j ->
    if i = j
    then Some env
    else None
  | Pconst (Cint _, loc), _ ->
    raise (Error (Type_error ([Tint],vtyp value),loc))
  | Pconst (Cfloat f, loc), _ ->
    error loc "float %f in pattern" f
  | Pvar (id,loc), _ ->
    begin match Env.get_val env id with
      | Some v ->
        if equal_value loc v value
        then Some env
        else None
      | None -> Some (Env.bind_var env id value loc) end
  | Ptuple (pl,loc), Varray a ->
    let len = List.length pl in
    if len <> Array.length a
    then error loc "pattern size (%i) do not match array length (%i)" len (Array.length a);
    let aux env pat value =
      match env with
      | None -> None
      | Some env -> match_pattern env pat value in
    List.fold_left2 aux (Some env) pl (Array.to_list a)
  | Ptuple (_,loc), _ ->
    raise (Error (Type_error ([Tarr],vtyp value),loc))

let fold_pattern env pat arr f acc =
  let aux acc v =
    match match_pattern env pat v with
    | None -> acc
    | Some env -> f env acc
  in
  Array.fold_left aux acc arr

let op_lp_exp op v1 v2 (t1,t2) loc =
  let v = match op with
    | Add -> Expr.add v1 v2
    | Sub -> Expr.sub v1 v2
    | Mul ->
      begin try Expr.mul v1 v2 with
        | Expr.Not_linear -> raise (Error (Not_linear,loc)) end
    | Div ->
      begin try Expr.div v1 v2 with
        | Expr.Not_linear -> raise (Error (Not_linear,loc)) end
    | Range ->
      raise (Error (Operation_impossible (string_of_op op, t1, t2), loc))
  in
  Vlp_exp v

let range start stop =
  Array.init (max 0 (stop - start + 1)) (fun i -> Vint (i + start))

let op_val op e1 e2 loc =
  let typs = vtyp e1, vtyp e2 in
  match e1,e2 with
  | Vlp_exp v1, Vlp_exp v2 -> op_lp_exp op v1 v2 typs loc
  | Vlp_exp v1, Vint v2 ->
    op_lp_exp op v1 (Expr.cst (float_of_int v2)) typs loc
  | Vint v1, Vlp_exp v2 ->
    op_lp_exp op (Expr.cst (float_of_int v1)) v2 typs loc
  | Vint v1, Vint v2 ->
    begin match op with
      | Add -> Vint (v1 + v2)
      | Sub -> Vint (v1 - v2)
      | Mul -> Vint (v1 * v2)
      | Div ->
        raise (Error (Operation_impossible (string_of_op op,vtyp e1,vtyp e2),loc))
      | Range -> Varray (range v1 v2)
    end
  | e1, e2 ->
    raise (Error (Operation_impossible (string_of_op op,vtyp e1,vtyp e2),loc))

let rec eval_expr env = function
  | Const (c,_) ->
    begin match c with
      | Cint i -> Vint i
      | Cfloat f -> Vlp_exp (Expr.cst f) end
  | Binop (op,e1,e2,loc) ->
    let e1' = eval_expr env e1 in
    let e2' = eval_expr env e2 in
    op_val op e1' e2' loc
  | Var (id,loc) -> Env.find_val env id loc
  | Sum (pattern,restrict,body,_) ->
    eval_sum env pattern restrict body
  | Access (v, index, loc) ->
    let v = eval_expr env v in
    begin match v with
      | Varray arr ->
        let index = eval_expr_int env index in
        begin try arr.(index) with _ ->
          raise (Error (Out_of_bound index,loc)) end
      | Vobj o ->
        let key = eval_expr env index in
        Object.find key o loc
      | _ ->
        raise (Error (Type_error ([Tarr;Tobj],vtyp v),loc))
    end
  | Field (e,field,loc) ->
    let v = eval_expr_obj env e in
    Object.find (Vstring field) v loc
  | Last (e, _) ->
    let v = eval_expr_array env e in
    Vint (Array.length v - 1)
  | Max (e, loc) ->
    let to_int = function
      | Vint i -> i
      | v -> raise (Error (Type_error ([Tint],vtyp v),loc))
    in
    let elts = Array.map to_int (eval_expr_set env e) in
    Array.sort (fun x y -> y - x) elts;
    if Array.length elts = 0
    then failwith "max on empty array: todo real error"
    else Vint elts.(0)
  | IndexOf (e, loc) ->
    begin match eval_expr env e with
      | Varray arr ->
        Varray (range 0 (Array.length arr - 1))
      | Vobj o ->
        Varray
          (Array.map (fun (key,_) -> key)
             (Array.of_list (Object.bindings o)))
      | v ->
        raise (Error (Type_error ([Tarr;Tobj],vtyp v),loc))
    end
  | Etuple (el, _) ->
    Varray (Array.of_list (List.map (eval_expr env) el))
  | Eobject (fields, _) ->
    let obj = List.fold_left (fun acc (id, expr) ->
        Object.add (Vstring id) (eval_expr env expr) acc (expr_loc expr))
        Object.empty fields in
    Vobj obj
  | Cmp _
  | Bool_binop _
  | Not _ as bool_exp ->
    begin match eval_bool_expr env bool_exp with
      | Bool true -> Vint 1
      | Bool false -> Vint 0
      | Constraints _ ->
        error (expr_loc bool_exp)
          "expression cannot evaluate to a boolean"
    end
  | Str (id, _) ->
    Vstring id

and eval_sum env pattern restrict expr =
  let rec aux env acc = function
    | [] ->
      let kept =
        (* it could be a good idea to evaluate restrict as soon as all
           its free variables are bounded, this could get a big speedup

           Another possibility whould be to allow evaluating partially
           bound expressions and if an unbound variable is used, return
           the empty set. This could help with expressions like

           sum(a in ta, b in tb: a > 3 & b < 2) ...

           here we could evaluate as soon as possible and if a is <= 3
           then we can brutaly cut the branch *)
        match restrict with
        | None -> true
        | Some restrict -> match eval_bool_expr env restrict with
          | Bool b -> b
          | Constraints _ ->
            error (expr_loc restrict) "not a boolean"
      in
      if kept
      then op_val Add acc (eval_expr env expr) (expr_loc expr)
      else acc

    | pattern :: pattern_list ->
      match pattern.p_expr with
      | [] -> aux env acc pattern_list
      | pat::q ->
        let pattern' = { pattern with p_expr = q } in
        let patterns = pattern' :: pattern_list in
        let f env acc = aux env acc patterns in
        fold_pattern env pat (eval_expr_set env pattern.p_id) f acc
  in
  aux env (Vint 0) pattern

  and eval_expr_lp env expr =
    match eval_expr env expr with
    | Vlp_exp e -> e
    | Vint i -> Expr.cst (float i) (* autorized ?*)
    | e -> raise (Error (Type_error ([Texp],vtyp e),expr_loc expr))

  and eval_expr_array env expr =
    match eval_expr env expr with
    | Varray a -> a
    | e -> raise (Error (Type_error ([Tarr],vtyp e),expr_loc expr))

  and eval_expr_int env expr =
    match eval_expr env expr with
    | Vint i -> i
    | e -> raise (Error (Type_error ([Tint],vtyp e),expr_loc expr))

  (* and eval_expr_string env expr = *)
  (*   match eval_expr env expr with *)
  (*   | Vstring s -> s *)
  (*   | e -> raise (Error (Type_error ([Tstr],vtyp e),expr_loc expr)) *)

  and eval_expr_obj env expr =
    match eval_expr env expr with
    | Vobj o -> o
    | e -> raise (Error (Type_error ([Tobj],vtyp e),expr_loc expr))

  and eval_expr_set env expr =
    match eval_expr env expr with
    | Varray a -> a
    | Vobj o -> Array.map snd (Array.of_list (Object.bindings o))
    | e -> raise (Error (Type_error ([Tarr;Tobj],vtyp e),expr_loc expr))

  and eval_bool_expr env = function
    | Cmp (op,e1,e2,loc) ->
      eval_cmp op (eval_expr env e1) (eval_expr env e2) loc
    | Bool_binop (op, be1, be2, loc) ->
      eval_bool_binop op (eval_bool_expr env be1) (eval_bool_expr env be2) loc
    | Not (be, loc) ->
      begin match eval_bool_expr env be with
        | Constraints _ as v ->
          (* TODO: it is probably possible on linear expression
             comparing with >= or <= on integers and maybe with foating point
             (adding epsilon ?) *)
          raise (Error (Bool_not_impossible (btyp v),loc))
        | Bool b -> Bool (not b)
      end
    | e -> error (expr_loc e) "not a boolean expression"

let eval_obj_expr env e = Expr.to_list (eval_expr_lp env e)

let rec eval_expr_pattern env expr restrict acc = function
  | [] ->
    let kept =
      match restrict with
      | None -> true
      | Some restrict -> match eval_bool_expr env restrict with
        | Bool b -> b
        | Constraints _ ->
          error (expr_loc restrict) "not a boolean"
    in
    if kept
    then (eval_bool_expr env expr) :: acc
    else acc
  | t::q ->
    match t.p_expr with
    | [] -> eval_expr_pattern env expr restrict acc q
    | p1::pq ->
      let t' = { t with p_expr = pq } in
      let l' = t'::q in
      let aux env acc = eval_expr_pattern env expr restrict acc l' in
      fold_pattern env p1 (eval_expr_set env t.p_id) aux acc

let rec flatten_constraints = function
  | [] -> Bool true
  | Bool true :: q -> flatten_constraints q
  | Bool false :: _ -> Bool false
  | Constraints l :: q ->
    match flatten_constraints q with
    | Bool true -> Constraints l
    | Bool false -> Bool false
    | Constraints l' -> Constraints (l @ l')

let eval_constr env c =
  let constraints = List.map
      (fun expr -> eval_expr_pattern env expr
          c.constr_restrict [] c.constr_patterns)
      c.constr_expr in
  flatten_constraints (List.flatten constraints)

let make_Vobject (l:(value * value) list) loc : value Object.t =
  let aux o (k,v) = Object.add k v o loc in
  List.fold_left aux Object.empty l

let make_Varray l loc =
  let l = List.sort (fun (i,_) (j,_) -> compare i j) l in
  let rec check i = function
    | [] -> true
    | (Vint t,_)::q -> (i = t) && check (i+1) q
    | (v, _)::_ -> raise (Error (Type_error ([Tstr],vtyp v),loc))
  in
  if check 0 l
  then Array.of_list (List.map snd l)
  else error loc "not consecutive indices"

let rec string_of_index loc = function
  | Vstring s -> s
  | Vint i -> string_of_int i
  | Varray a ->
    let s = String.concat ","
        (Array.to_list (Array.map (string_of_index loc) a)) in
    Printf.sprintf "Array(%s)" s
  | Vobj o ->
    let l = List.map (fun (id,v) ->
        Printf.sprintf "%s:%s" (string_of_index loc id) (string_of_index loc v))
        (Object.bindings o) in
    let s = String.concat "," l in
    Printf.sprintf "Object(%s)" s
  | Vlp_exp _ as v ->
    raise (Error (Type_error ([Tstr;Tint;Tarr;Tobj],vtyp v),loc))

let rec declare_variables env typ loc parts = function
  | [] ->
    let full_id = String.concat "_" (List.rev parts) in
    let env = Env.add_typ env full_id typ loc in
    (env, Vlp_exp (Expr.atom full_id 1.))
  | (a,loc)::remaining_indexes ->
    let aux index (env,l) =
      let str = string_of_index loc index in
      let env, value = declare_variables env typ loc
          (str::parts) remaining_indexes in
      env,(index,value)::l in
    let (env,l) = Array.fold_right aux a (env,[]) in
    let v = match l with
      | [] -> error loc "empty list"
      | (Vint _, _) :: _ ->
        Varray (make_Varray l loc)
      | ((Vstring _|Vobj _|Varray _),_) :: _ ->
        Vobj (make_Vobject l loc)
      | ((Vlp_exp _ as v),_) :: _ ->
        raise (Error (Type_error ([Tstr;Tint;Tarr;Tobj],vtyp v),loc))
    in
    (env,v)

(* change the kind of index of a value. i.e.
   if val is of type 'int array' and ind is of type '(int array) array'
   indexing val[ind]
   redeclare val as an object indexed by int arrays *)

let rec map_index env value = function
  | [] -> value
  | (t,loc) :: q ->
    match value with
    | Varray content ->
      begin match t with
        | Varray new_index ->
          if not (Array.length content = Array.length new_index)
          then error loc "Index remaping with different array size: original %i new %i" (Array.length content) (Array.length new_index);
          let content = List.map
              (fun v -> map_index env v q) (Array.to_list content) in
          let l = List.map2 (fun i c -> i,c) (Array.to_list new_index) content in
          Vobj (make_Vobject l loc)
        | _ -> raise (Error (Type_error ([Tarr],vtyp t),loc))
      end
    | Vobj content ->
      begin match t with
        | Vobj new_index ->
          let content = Object.map (fun v -> map_index env v q) content in
          let l =
            Object.bindings
              (Object.merge (fun _ v1 v2 -> match v1, v2 with
                 | Some v1, Some v2 ->
                   Some (v1,v2)
                 | None, _ | _, None ->
                   error loc "Index remaping between objects \
                              with different index") new_index content) in
          let l = List.map snd l in
          Vobj (make_Vobject l loc)
        | _ -> raise (Error (Type_error ([Tobj],vtyp t),loc))
      end
    | _ ->
      raise (Error (Type_error ([Tarr;Tobj],vtyp value),loc))

let add_decl env = function
  | Dvar decl ->
    let index = List.map
        (fun exp -> eval_expr_set env exp, expr_loc exp) decl.dvar_index in
    let (env,value) = declare_variables env decl.dvar_typ decl.dvar_loc
        [decl.dvar_id] index in
    Env.bind_var env decl.dvar_id value decl.dvar_loc
  | Alias alias ->
    let res = eval_expr env alias.alias_exp in
    begin match alias.alias_typ with
      | None ->
        Env.bind_var env alias.alias_id res alias.alias_loc
      | Some typ ->
        match vtyp res with
        | Texp | Tint ->
          Env.add env alias.alias_id res typ alias.alias_loc
        | typ ->
          raise (Error (Type_error ([Texp;Tint],typ),expr_loc alias.alias_exp))
    end
  | Indexing indexing ->
    let loc = indexing.index_loc in
    let index = List.map
        (fun exp -> eval_expr env exp, expr_loc exp) indexing.index_index in
    let remaped =
      map_index env (Env.find_val env indexing.index_id loc) index in
    Env.replace_var env indexing.index_id remaped


let load_decl init_env file =
  List.fold_left add_decl
    init_env
    file.declarations

let types_sections env =
  let ints = Env.int_typ env in
  match ints with
  | [] -> []
  | _ -> [Integer, ints]

(* generate bounds 0 <= x <= 1 for every bool variable *)
let bool_bounds env =
  let bools = Env.bool_typ env in
  List.map (fun var -> { var; lower_bound = 0.; upper_bound = 1. }) bools

let objective_expr env e bounds =
  (* the constant can be ignored for objective *)
  let expr, cst = eval_obj_expr env e in
  match expr with
    | [] ->
      (* when the objective is a constant, build a non empty constraint
         representing the constant. *)
      let var = "constant_one" in
      [ var, cst ],
      { var; lower_bound = 1.; upper_bound = 1. } :: bounds
    | _ ->
      expr, bounds


let to_lp init_env file =
  let env = load_decl init_env file in
  let constraints = List.map (eval_constr env) file.Ast.constraints in
  let constraints =
    match flatten_constraints constraints with
    | Bool true -> []
    | Bool false -> failwith "unsolvable: todo real error"
    | Constraints l -> l
  in
  let constraints, bounds = extract_bounds constraints in
  let objective_expr, bounds =
    objective_expr env file.Ast.objective.obj_expr bounds in
  let bounds = bool_bounds env @ bounds in
  { objective = file.Ast.objective.obj_dir;
    objective_expr;
    objective_label = None;
    constraints; bounds = merge_bounds bounds;
    types = types_sections env }

(* Error reporting *)

open Format

let print_cmp ppf cmp =
  let s = match cmp with
    | Eq -> "=="
    | Neq -> "!="
    | Geq -> ">="
    | Leq -> "<="
    | Gt -> ">"
    | Lt -> "<"
  in
  pp_print_string ppf s

let print_typ ppf t =
  let s = match t with
    | Tstr -> "string"
    | Tarr -> "array"
    | Tint -> "int"
    | Tobj -> "object"
    | Texp -> "linear expression" in
  pp_print_string ppf s

let rec print_typ_list ppf = function
  | [] -> assert false
  | [t] -> print_typ ppf t
  | t::q ->
    print_typ ppf t;
    fprintf ppf " or ";
    print_typ_list ppf q

let print_bool_op ppf op =
  let s = match op with
    | And -> "&&"
    | Or -> "||"
  in
  pp_print_string ppf s

let print_btyp ppf t =
  let s = match t with
    | Tconstr -> "linear constraint"
    | Tbool -> "boolean" in
  pp_print_string ppf s

let report_error ppf = function
  | Not_linear ->
    fprintf ppf "Expression not linear"
  | Type_error (expected, got) ->
    fprintf ppf "Type error: %a expected, got %a"
      print_typ_list expected
      print_typ got
  | Bool_not_impossible t ->
    fprintf ppf "Boolean not impossible on type %a"
      print_btyp t
  | Bool_op_impossible (op, t1, t2) ->
    fprintf ppf "Boolean operation %a impossible between type %a and %a"
      print_bool_op op
      print_btyp t1
      print_btyp t2
  | Comparison_impossible (cmp, t1, t2) ->
    fprintf ppf "Comparison %a impossible between type %a and %a"
      print_cmp cmp
      print_typ t1
      print_typ t2
  | Operation_impossible (s, t1, t2) ->
    fprintf ppf "Operation %s impossible between type %a and %a"
      s
      print_typ t1
      print_typ t2
  | Out_of_bound index ->
    fprintf ppf "Index out of bound %i" index
  | Not_a_linear_constraint cmp ->
    fprintf ppf "Not a linear constraint %a"
      print_cmp cmp
  | General s ->
    fprintf ppf "%s" s
