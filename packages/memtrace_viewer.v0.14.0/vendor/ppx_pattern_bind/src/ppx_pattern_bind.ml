open Base
open Ppxlib
open Ast_builder.Default

let pexp_let ~loc rec_ bindings e =
  match bindings with
  | [] -> e
  | _ :: _ -> pexp_let ~loc rec_ bindings e
;;

type extension =
  | Map
  | Bind

let string_of_extension = function
  | Map -> "map"
  | Bind -> "bind"
;;

let gen_symbol_prefix = "__pattern_syntax"

let name_expr expr =
  (* to avoid duplicating non-value expressions *)
  match expr.pexp_desc with
  | Pexp_ident _ -> [], expr
  | _ ->
    let loc = { expr.pexp_loc with loc_ghost = true } in
    let var = gen_symbol ~prefix:gen_symbol_prefix () in
    [ value_binding ~loc ~pat:(pvar ~loc var) ~expr ], evar ~loc var
;;

let replace_variable ~f =
  let replacer =
    object
      inherit Ast_traverse.map as super

      method! pattern p =
        let p = super#pattern p in
        let loc = { p.ppat_loc with loc_ghost = true } in
        match p.ppat_desc with
        | Ppat_var v ->
          (match f v with
           | `Rename tmpvar -> ppat_var ~loc { txt = tmpvar; loc = v.loc }
           | `Remove -> ppat_any ~loc)
        | Ppat_alias (sub, v) ->
          (match f v with
           | `Rename tmpvar -> ppat_alias ~loc sub { txt = tmpvar; loc = v.loc }
           | `Remove -> sub)
        | _ -> p
    end
  in
  replacer#pattern
;;

let variables_of =
  object
    inherit [string Ppxlib.loc list] Ast_traverse.fold as super

    method! pattern p acc =
      let acc = super#pattern p acc in
      match p.ppat_desc with
      | Ppat_var var -> var :: acc
      | Ppat_alias (_, var) -> var :: acc
      | _ -> acc
  end
;;

let pattern_variables pattern =
  List.dedup_and_sort
    ~compare:(fun x y -> String.compare x.txt y.txt)
    (variables_of#pattern pattern [])
;;

let error_if_invalid_pattern extension pattern =
  let finder =
    object
      inherit Ast_traverse.iter as super

      method! pattern p =
        super#pattern p;
        match p.ppat_desc with
        | Ppat_unpack _ ->
          Location.raise_errorf
            ~loc:p.ppat_loc
            "%%pattern_%s cannot be used with (module ..) patterns."
            (string_of_extension extension)
        | Ppat_exception _ ->
          Location.raise_errorf
            ~loc:p.ppat_loc
            "%%pattern_%s cannot be used with exception patterns."
            (string_of_extension extension)
        | _ -> ()
    end
  in
  finder#pattern pattern
;;

let warning_attribute ~loc str =
  attribute
    ~loc
    ~name:(Loc.make ~loc "ocaml.warning")
    ~payload:(PStr [ pstr_eval ~loc (estring ~loc str) [] ])
;;

(* Translations for match%pattern_bind


   {[
     match%pattern_bind e with
     | A x -> render_a x
     | B (y, z) -> render_b (y, z)
   ]}

   ===>

   {[
     let exp = e in
     match%bind
       match%map exp with
       | A _ -> 0
       | B (_, _) -> 1
     with
     | 0 ->
       let x =
         match%map exp with
         | A x -> x
         | _ -> assert false
       in
       render_a x
     | 1 ->
       let y =
         match%map exp with
         | B (y, _) -> y
         | _ -> assert false
       and z =
         match%map exp with
         | B (_, z) -> z
         | _ -> assert false
       in
       render_b (y, z)
     | _ -> assert false
   ]}

   and match%pattern_map is the same thing where the inner [lets] like
   [let y = .. and z = ..] are let%map.
*)

type pat_exh =
  { pat : pattern
  ; assume_is_exhaustive : bool
  }

(*
   match%map exp with
   | B (var, _) -> var
   | _ -> assert false
*)
let project_bound_var ~loc exp ~pat:{ pat; assume_is_exhaustive } var =
  let project_the_var =
    (* We use a fresh var name because the compiler conflates all definitions with the
       name * location, for the purpose of emitting warnings. *)
    let tmpvar = gen_symbol_prefix ^ "_" ^ var.txt in
    let pattern =
      replace_variable pat ~f:(fun v ->
        if String.equal v.txt var.txt then `Rename tmpvar else `Remove)
    in
    case ~lhs:pattern ~guard:None ~rhs:(evar ~loc tmpvar)
  in
  let catch_all_case =
    if assume_is_exhaustive
    then []
    else [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:[%expr assert false] ]
  in
  Ppx_let_expander.expand
    ~modul:None
    Map
    (pexp_match ~loc exp (project_the_var :: catch_all_case))
;;

let project_bound_vars ~loc exp ~lhs =
  let variables = pattern_variables lhs.pat in
  List.map variables ~f:(fun var ->
    value_binding
      ~loc
      ~pat:(ppat_var ~loc:var.loc var)
      ~expr:(project_bound_var ~loc exp ~pat:lhs var))
;;

let projected_vars_rhs extension ~loc ~bindings ~rhs =
  let loc = { loc with loc_ghost = true } in
  let let_ = pexp_let ~loc Nonrecursive bindings rhs in
  match extension with
  | Bind -> let_
  | Map ->
    (match bindings with
     | [] -> [%expr return [%e rhs]]
     | _ :: _ -> Ppx_let_expander.expand ~modul:None Map let_)
;;

(*
   {[
     | 1 ->
       let y =
         match%map exp with
         | B (y, _) -> y
         | _ -> assert false
       and z =
         match%map exp with
         | B (_, z) -> z
         | _ -> assert false
       in
       render_b (y, z)
   ]}
*)
let expand_case extension exp (idx, match_case) ~assume_lhs_is_exhaustive =
  let loc = { match_case.pc_lhs.ppat_loc with loc_ghost = true } in
  let rhs =
    projected_vars_rhs
      extension
      ~loc
      ~bindings:
        (project_bound_vars
           ~loc
           exp
           ~lhs:
             { pat = match_case.pc_lhs; assume_is_exhaustive = assume_lhs_is_exhaustive })
      ~rhs:match_case.pc_rhs
  in
  case ~lhs:(pint ~loc idx) ~guard:None ~rhs
;;

let case_number_cases ~loc extension exp indexed_cases =
  let assume_lhs_is_exhaustive = List.length indexed_cases <= 1 in
  List.map indexed_cases ~f:(expand_case extension exp ~assume_lhs_is_exhaustive)
  @ [ case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr assert false] ]
;;

(*
   match%map exp with
   | A x -> 0
   | B (y, z) -> 1
*)
let case_number ~loc exp indexed_cases =
  Ppx_let_expander.expand
    ~modul:None
    Map
    { (pexp_match
         ~loc
         exp
         (List.map indexed_cases ~f:(fun (idx, case) ->
            { case with pc_rhs = eint ~loc idx })))
      with
        pexp_attributes = (* Unused variable warnings *)
          [ warning_attribute ~loc "-26-27" ]
    }
;;

let expand_match extension ~loc expr cases =
  let loc = { loc with loc_ghost = true } in
  List.iter cases ~f:(fun { pc_lhs; pc_guard; _ } ->
    error_if_invalid_pattern extension pc_lhs;
    match pc_guard with
    | None -> ()
    | Some v ->
      (* We tried to support this, but ending up reverting (in 189712731a6): it seems
         hard/impossible to have the desired warning and performance. *)
      Location.raise_errorf
        ~loc:v.pexp_loc
        "%%pattern_%s cannot be used with `when`."
        (string_of_extension extension));
  let expr_binding, expr = name_expr expr in
  let indexed_cases = List.mapi cases ~f:(fun idx case -> idx, case) in
  let case_number = case_number ~loc expr indexed_cases in
  let case_number_cases = case_number_cases ~loc extension expr indexed_cases in
  pexp_let
    ~loc
    Nonrecursive
    expr_binding
    (Ppx_let_expander.expand
       ~modul:None
       Bind
       (Merlin_helpers.hide_expression (pexp_match ~loc case_number case_number_cases)))
;;

(* Translations for let%pattern_bind

   let%pattern_bind (x, y, _) = e1
   and { z; _} = e2
   in exp

   ===>

   let v1 = e1
   and v2 = e2
   in
   let x = let%map (x, _, _) = v1 in x
   and y = let%map (_, y, _) = v1 in y
   and z = let%map { z; _} = v2 in z
   in
   exp
*)

let project_bound_vars_list l =
  List.concat_map l ~f:(fun vb ->
    let loc = { vb.pvb_loc with loc_ghost = true } in
    project_bound_vars
      ~loc
      vb.pvb_expr
      ~lhs:{ pat = vb.pvb_pat; assume_is_exhaustive = true })
;;

let expand_let extension ~loc vbs exp =
  List.iter vbs ~f:(fun vb -> error_if_invalid_pattern extension vb.pvb_pat);
  let bindings, vbs =
    List.unzip
      (List.map vbs ~f:(fun vb ->
         let b, expr = name_expr vb.pvb_expr in
         b, { vb with pvb_expr = expr }))
  in
  let with_projections =
    let loc = { loc with loc_ghost = true } in
    let bindings =
      List.map (project_bound_vars_list vbs) ~f:(fun vb ->
        { vb with pvb_expr = Merlin_helpers.hide_expression vb.pvb_expr })
    in
    (* For [let%pattern_bind], we don't bind on the match case, so nothing constrains
       [let_] to be an incremental. We used to generate [if false then return (assert
       false) else let_] to compensate, but that causes problems with the defunctorized
       interface of incremental, as [return] takes an extra argument. [if false then map
       (assert false) ~f:Fn.id else let_] avoids that but causes type errors in bonsai
       where they sort of abuse this preprocessor by using this with this thing that's
       not a monad (see legacy_api.ml). *)
    projected_vars_rhs extension ~loc ~bindings ~rhs:exp
  in
  pexp_let ~loc Nonrecursive (List.concat bindings) with_projections
;;

let expand extension ~loc expr =
  match expr.pexp_desc with
  | Pexp_let (rec_flag, vbs, exp) ->
    (match rec_flag with
     | Nonrecursive -> ()
     | Recursive ->
       Location.raise_errorf
         ~loc
         "%%pattern_%s cannot be used with 'let rec'"
         (string_of_extension extension));
    expand_let extension ~loc vbs exp
  | Pexp_match (expr, cases) -> expand_match extension ~loc expr cases
  | _ ->
    Location.raise_errorf
      ~loc
      "'%%pattern_%s can only be used with 'let' and 'match'"
      (string_of_extension extension)
;;

let extension extension =
  Extension.declare
    ("pattern_" ^ string_of_extension extension)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ expr -> expand extension ~loc expr)
;;

let () =
  Driver.register_transformation
    "pattern_bind"
    ~extensions:[ extension Bind; extension Map ]
;;
