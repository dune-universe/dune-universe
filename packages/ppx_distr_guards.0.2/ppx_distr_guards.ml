open Migrate_parsetree

open Ast_409
let ocaml_version = Versions.ocaml_409

let mapper _config _cookies =
  let open Ast_mapper in
  let open Ast_helper in
  let open Parsetree in
  let case_to_cases m = function
    | { pc_lhs;
        pc_guard = Some guard;
        pc_rhs
      } ->
        (* collect or-patterns into list *)
        (* TODO fold over all constructors to support or-patterns in other patterns *)
        let rec f a p = match p.ppat_desc with
          | Ppat_or (p1, p2) -> f (f a p2) p1
          | _ -> p :: a
        in
        let ps = f [] pc_lhs in
        let e = m.expr m pc_rhs in
        List.map (fun p -> Exp.case p ~guard e) ps
    | x -> [default_mapper.case m x]
  in
  let distr_mapper = { default_mapper with
    cases = fun m xs -> List.map (case_to_cases m) xs |> List.flatten }
  in
  { default_mapper with
    expr = fun m expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "distr"; _ }, PStr [{ pstr_desc = Pstr_eval (e,a); _ }]); _ } ->
          m.expr distr_mapper { e with pexp_attributes = expr.pexp_attributes @ a }
      | x -> default_mapper.expr m x;
  }

let () =
  Driver.register ~name:"distr_guards" ocaml_version mapper

(*
ocamlc -dparsetree test.ml
ocamlbuild -package compiler-libs.common ppx_distr_guards.native
ocamlc -dsource -ppx ./ppx_distr_guards.native test.ml
*)
