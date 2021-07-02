open Ppxlib
(* migration to ppxlib: https://github.com/vogler/ppx_distr_guards/pull/1 *)

let rec case_to_cases ~ctxt case =
  match case with
  | { pc_lhs; pc_guard = Some guard; pc_rhs } ->
      (* collect or-patterns into list *)
      (* TODO fold over all constructors to support or-patterns in other patterns *)
      let rec f a p =
        match p.ppat_desc with Ppat_or (p1, p2) -> f (f a p2) p1 | _ -> p :: a
      in
      let ps = f [] pc_lhs in
      List.map (fun p -> Ast_helper.Exp.case p ~guard pc_rhs) ps
  | x -> [ { x with pc_rhs = expand ~ctxt x.pc_rhs } ]

and expand ~ctxt e =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let map cases = cases |> List.map (case_to_cases ~ctxt) |> List.flatten in
  let open Ast_helper.Exp in
  match e.pexp_desc with
  | Pexp_function cases -> function_ ~loc (map cases)
  | Pexp_match (e, cases) -> match_ ~loc e (map cases)
  | Pexp_try (e, cases) -> try_ ~loc e (map cases)
  | _ -> e

let extension =
  Extension.V3.declare "distr" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let rule = Ppxlib.Context_free.Rule.extension extension

let () = Driver.register_transformation ~rules:[ rule ] "distr_guards"

(*
  show parse tree: ocamlc -dparsetree test.ml
  show result of ppx rewriter: dune exec ./standalone.exe test.ml
  if you want to build without dune: ocamlfind ocamlc ppx_distr_guards.ml -package ppxlib -linkpkg standalone.ml -o standalone.exe
*)
