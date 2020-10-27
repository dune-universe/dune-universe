open Common_types

type error =
  | Not_a_number
  | Unsolvable_problem of cmp * float
  | Not_a_linear_constraint of cmp

exception Error of error

type label = string

type expr = (Expr.var*float) list

type constr =
  { label : label option;
    expr : expr;
    bound_type : cmp;
    bound : float; }

type bound =
  { var : Expr.var;
    lower_bound : float;
    upper_bound : float; }

type section =
  | General
  | Integer
  | Binary

type t =
  { objective : direction;
    objective_expr : expr; (* might change to accept quadratic objective *)
    objective_label : label option;
    constraints : constr list;
    bounds : bound list;
    types : (section * Expr.var list) list; }

(* functorize on the output ? *)

(* TODO: check if string_of_float output is effectively accepted by
   lp solvers potential problems: 1e10 *)

(***** recognize constraints that are variable bounds ******)

type constraint_kind =
  | C of constr
(*  | B of bound *)
  | N (* None -> drop *)
  | F of cmp * float (* failure: f = 0 *)

let classify_constraint c =
  match c.label, c.expr with
  | Some _, _ ->
    C c (* annotations can only be kept on constraint *)
  | None, [] ->
    begin match classify_float c.bound with
      | FP_zero -> N
      | _ ->
        begin match c.bound_type with
          | Eq -> F (c.bound_type, c.bound)
          | Leq ->
            if c.bound > 0.
            then N
            else F (c.bound_type, c.bound)
          | Geq ->
            if c.bound < 0.
            then N
            else F (c.bound_type, c.bound)
          | Neq | Lt | Gt ->
            (* This should not happen: normaly prevented by previous
               generate *)
            raise (Error (Not_a_linear_constraint c.bound_type))
        end
    end
  (* We do not convert to bounds now
     | None, [var,v] ->
      let b = c.bound /. v in
      (* this is not correct with integer variables: an integer variable cannot
         have non integer bounds *)
    let lb, ub = match c.bound_type with
      | Eq -> b, b
      | Leq -> neg_infinity, b
      | Geq -> b, infinity
    in
    B { var; lower_bound = lb; upper_bound = ub }
*)
  | None, _ ->
    C c

(* is it of any interest ?
let extract_bounds l =
  let aux (const_l,bound_l) c = match classify_constraint c with
    | C c -> (c :: const_l,bound_l)
    | B b -> (const_l,b :: bound_l)
    | N -> (const_l,bound_l)
    | F (cmp,f) -> raise (Error (Unsolvable_problem (cmp,f)))
  in
  List.fold_left aux ([],[]) l
*)

let extract_bounds l = (l,[])

(****** merge bounds ******)

let default_bound var = { var; lower_bound = neg_infinity; upper_bound = infinity }

let intersect_bounds b1 b2 =
  assert(b1.var = b2.var);
  { var = b1.var;
    lower_bound = max b1.lower_bound b2.lower_bound;
    upper_bound = min b1.upper_bound b2.upper_bound; }

let merge_bounds bounds =
  let t = Hashtbl.create 10 in
  let add b =
    let curr =
      try Hashtbl.find t b.var
      with Not_found -> default_bound b.var in
    Hashtbl.replace t b.var (intersect_bounds b curr)
  in
  List.iter add bounds;
  Hashtbl.fold (fun _ v l -> v::l) t []

(****** printing *******)

let print_float f =
  match classify_float f with
  | FP_zero -> "0"
  | FP_infinite ->
    if f > 0.
    then "+Inf"
    else "-Inf"
  | FP_nan ->
    raise (Error Not_a_number)
  | _ -> string_of_float f

(* assumes that expr is not empty *)
let print_expr buf expr =
  let aux (var, coef) =
    if coef = 1. (* this isn't a computed value so equallity is right *)
    then ()
    else
      (Buffer.add_string buf (print_float coef);
       Buffer.add_char buf ' ');
    Buffer.add_string buf var
  in
  let rec f = function
    | [] -> assert false
    | [c] -> aux c
    | t1::(((_,c2)::_) as q) ->
      aux t1;
      (if c2 < 0.
       then Buffer.add_string buf " "
       else Buffer.add_string buf " + ");
      f q
  in
  f expr

let print_objective buf file =
  let obj_s = match file.objective with
    | Minimize -> "Minimize\n"
    | Maximize -> "Maximize\n" in
  Buffer.add_string buf obj_s;
  Buffer.add_string buf "  ";
  begin match file.objective_label with
    | None -> ()
    | Some s ->
      Buffer.add_string buf s;
      Buffer.add_string buf ": ";
  end;
  print_expr buf file.objective_expr;
  Buffer.add_string buf "\n\n"

let cmp_to_string = function
  | Eq -> "="
  | Geq -> ">="
  | Leq -> "<="
  | Neq -> "!="
  | Gt -> ">"
  | Lt -> "<"

let print_constraint buf constr =
  match classify_constraint constr with
  | N -> ()
  | F (cmp,f) ->
    raise (Error (Unsolvable_problem (cmp,f)))
  | C constr ->
    Buffer.add_string buf "  ";
    begin match constr.label with
      | None -> ()
      | Some s ->
        Buffer.add_string buf s;
        Buffer.add_string buf ": ";
    end;
    assert(constr.expr <> []);
    print_expr buf constr.expr;
    Buffer.add_char buf ' ';
    Buffer.add_string buf (cmp_to_string constr.bound_type);
    Buffer.add_char buf ' ';
    Buffer.add_string buf (print_float constr.bound);
    Buffer.add_char buf '\n'

let print_constraints buf constrs =
  Buffer.add_string buf "Subject to\n";
  (match constrs with
   | [] -> (* avoid empty constraint section *)
     Buffer.add_string buf "  dummy_var = 0\n"
   | _ -> List.iter (print_constraint buf) constrs);
  Buffer.add_string buf "\n"

(* TODO: refaire avec les flotants
let ignore_infinity = function
  | None -> None
  | Some x when x = infinity -> None
  | x -> x

let ignore_zero = function
  | None -> None
  | Some x when classify_float x = FP_zero -> None
  | x -> x

let print_bound buf bound =
  match ignore_zero bound.lower_bound, ignore_infinity bound.upper_bound with
  | None, None -> ()
  | Some lb, None ->
    Buffer.add_string buf "  ";
    Buffer.add_string buf bound.var;
    Buffer.add_string buf " >= ";
    Buffer.add_string buf (print_float lb);
    Buffer.add_string buf "\n"
  | None, Some ub ->
    Buffer.add_string buf "  ";
    Buffer.add_string buf bound.var;
    Buffer.add_string buf " <= ";
    Buffer.add_string buf (print_float ub);
    Buffer.add_string buf "\n"
  | Some lb, Some ub ->
    Buffer.add_string buf "  ";
    Buffer.add_string buf (print_float lb);
    Buffer.add_string buf " <= ";
    Buffer.add_string buf bound.var;
    Buffer.add_string buf " <= ";
    Buffer.add_string buf (print_float ub);
    Buffer.add_string buf "\n"
*)

let print_bound buf bound =
  Buffer.add_string buf "  ";
  Buffer.add_string buf (print_float bound.lower_bound);
  Buffer.add_string buf " <= ";
  Buffer.add_string buf bound.var;
  Buffer.add_string buf " <= ";
  Buffer.add_string buf (print_float bound.upper_bound);
  Buffer.add_string buf "\n"


let print_bounds buf bounds =
  match bounds with
  | [] -> ()
  | _ ->
    Buffer.add_string buf "Bounds\n";
    List.iter (print_bound buf) bounds;
    Buffer.add_string buf "\n"

let string_of_section = function
  | General -> "General"
  | Integer -> "Integer"
  | Binary -> "Binary"

let print_type_section buf (section, vars) =
  Buffer.add_string buf (string_of_section section);
  List.iter (fun s ->
    Buffer.add_string buf "\n  ";
    Buffer.add_string buf s)
    vars;
  Buffer.add_string buf "\n\n"

let print_file buf file =
  print_objective buf file;
  print_constraints buf file.constraints;
  print_bounds buf file.bounds;
  List.iter (print_type_section buf) file.types;
  Buffer.add_string buf "End\n"

let output_file oc file =
  let b = Buffer.create 100 in
  print_file b file;
  Buffer.output_buffer oc b

(* Error reporting *)

open Format

(* TODO: add labels to errors *)

let report_error ppf = function
  | Not_a_number ->
    fprintf ppf "nan encountered@."
  | Unsolvable_problem (cmp,f) ->
    fprintf ppf "Unsolvable problem: 0. %s %f@."
      (cmp_to_string cmp) f
  | Not_a_linear_constraint cmp ->
    fprintf ppf "Constraint is not linear: %s. \
                 Please report this, this is probably a bug@."
      (cmp_to_string cmp)
