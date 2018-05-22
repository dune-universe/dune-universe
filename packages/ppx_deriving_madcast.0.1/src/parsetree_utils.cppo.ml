
open Parsetree
open Ast_helper

let rec equal_core_type t t' =
  equal_core_type_desc t.ptyp_desc t'.ptyp_desc

and equal_core_type_desc t t' =
  match (t, t') with
  | ( Ptyp_any               , Ptyp_any                  ) -> false
  | ( Ptyp_var v             , Ptyp_var v'               ) -> v = v'
  | ( Ptyp_arrow (l, t1, t2) , Ptyp_arrow (l', t1', t2') ) -> l = l' && equal_core_type t1 t1' && equal_core_type t2 t2'
  | ( Ptyp_tuple tl          , Ptyp_tuple tl'            ) -> List.for_all2 equal_core_type tl tl'
  | ( Ptyp_constr (i, tl)    , Ptyp_constr (i', tl')     ) -> i.txt = i'.txt && List.for_all2 equal_core_type tl tl'

  | ( Ptyp_object _          , Ptyp_object _             )
  | ( Ptyp_class _           , Ptyp_class _              )
  | ( Ptyp_alias _           , Ptyp_alias _              )
  | ( Ptyp_variant _         , Ptyp_variant _            )
  | ( Ptyp_poly _            , Ptyp_poly _               )
  | ( Ptyp_package _         , Ptyp_package _            )
  | ( Ptyp_extension _       , Ptyp_extension _          ) -> assert false

  | _ -> false

module SSet = Set.Make(String)

let variables_of_core_type t =
  let rec variables_of_core_type acc t =
    match t.ptyp_desc with
    | Ptyp_any -> acc
    | Ptyp_var x -> SSet.add x acc

    | Ptyp_arrow (_, t, t') -> variables_of_core_type (variables_of_core_type acc t) t'

    | Ptyp_tuple tl
    | Ptyp_constr (_, tl) -> List.fold_left variables_of_core_type acc tl

    | _ -> assert false
  in
  let set = variables_of_core_type SSet.empty t in
  SSet.fold (fun x acc -> x :: acc) set []

let universal_closure_of_core_type t =
  Typ.poly (List.map
#if OCAML_VERSION < (4, 05, 0)
                 (fun x -> x)
#else
                 Location.mknoloc
#endif
              (variables_of_core_type t))
    t
