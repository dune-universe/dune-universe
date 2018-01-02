(* Extension of modules under Ast_<ver> *)

open Migrate_parsetree.Ast_405
open Utils
open List

module Longident = struct
  include Longident

  open Format
    
  let rec format ppf = function
    | Lident n -> pp_print_string ppf n
    | Ldot (p, name) -> fprintf ppf "%a.%s" format p name
    | Lapply (p1, p2) -> fprintf ppf "%a(%a)" format p1 format p2

  let to_string l = ksprintf (fun x -> x) "%a" format l
end

module Ident = struct
  include Ident

  open Format
    
  let format ppf id = pp_print_string ppf id.name
  let format_verbose ppf id = fprintf ppf "%s/%d" id.name id.stamp
end

module Path = struct
  include Path

  open Format

  let rec format ppf = function
    | Pident id -> Ident.format ppf id
    | Pdot (p, name, _n) -> fprintf ppf "%a.%s" format p name
    | Papply (p1, p2) -> fprintf ppf "%a(%a)" format p1 format p2

  let rec format_verbose ppf = function
    | Pident id -> Ident.format_verbose ppf id
    | Pdot (p, name, n) -> fprintf ppf "%a.%s__%d" format_verbose p name n
    | Papply (p1, p2) -> fprintf ppf "%a(%a)" format_verbose p1 format_verbose p2

  let to_string l = ksprintf (fun x -> x) "%a" format l
end
  
module Location = struct
  include Location
  let format = print_loc
  let merge t1 t2 = { t1 with loc_end = t2.loc_end }
end

module XParsetree = struct
  (* We cannot include Parsetree since it lacks implementation *)
  open Parsetree
    
  let iter_core_type f ty = match ty.ptyp_desc with
      Ptyp_any | Ptyp_var _ -> ()
    | Ptyp_arrow (_, ty1, ty2) -> f ty1; f ty2
    | Ptyp_tuple l      
    | Ptyp_constr (_, l)
    | Ptyp_class (_, l) -> iter f l
    | Ptyp_alias (ty, _) -> f ty
    | Ptyp_object(s_a_cty_l, _) ->
        iter (fun (_, _, cty) -> f cty) s_a_cty_l
    | Ptyp_variant (rfs, _, _) ->
        iter (function
          | Rtag (_, _, _, l) -> iter f l
          | Rinherit t -> f t) rfs
    | Ptyp_poly (_, t) -> f t
    | Ptyp_package (_, l_cty_s) ->
        iter (fun (_, t) -> f t) l_cty_s
    | Ptyp_extension _ -> ()

  module LongidentSet = Set.Make(struct type t = Longident.t let compare = compare end)

  (* referred constrs and classes *)   
  let constrs_in_core_type_ ty =
    let s = ref LongidentSet.empty in
    let add l = s := LongidentSet.add l !s in
    let rec f ty =
      begin match ty.ptyp_desc with
      | Ptyp_constr ({txt}, _) -> add txt
      | Ptyp_class ({txt}, _) -> add txt
      | _ -> ()
      end;
      iter_core_type f ty
    in
    f ty;
    !s

  let constrs_in_core_type ty =
    LongidentSet.elements & constrs_in_core_type_ ty

  let constrs_in_type_declaration td =
    constrs_in_core_type
    & Ast_helper.Typ.tuple
    & concat_map (fun (ty1, ty2, _) -> [ty1; ty2]) td.ptype_cstrs
    @ begin match td.ptype_kind with
      | Ptype_abstract -> []
      | Ptype_variant cds ->
          concat_map (fun cd -> 
            (match cd.pcd_args with
            | Pcstr_tuple ctys -> ctys
            | Pcstr_record lds -> map (fun x -> x.pld_type) lds)
            @ Option.to_list cd.pcd_res) cds
      | Ptype_record ldl ->
          map (fun ld -> ld.pld_type) ldl
      | Ptype_open -> []
      end
    @ Option.to_list td.ptype_manifest

  let sccs (es : ('v * 'v list) list) : 'v list list =
    match es with
    | [] -> []
    | _ -> 
    let rec f cntr vns s p sccs (v : 'v * 'v list) =
      let (v_, w_s) = v in
      let vns = (v_,cntr) :: vns in
      let s = v :: s in
      let p = (v,cntr) :: p in
      let cntr = cntr + 1 in
      let cntr, vns, s, p, sccs =
        fold_left (fun (cntr, vns, s, p, sccs) w_ ->
          let w = w_, assoc w_ es in
          match assoc_opt w_ vns with
          | None -> f cntr vns s p sccs w
          | Some n ->
              let rec pop = function
                | ((_,n')::_ as p) when n' <= n -> p
                | _::vns -> pop vns
                | [] -> assert false
              in
              cntr, vns, s, pop p, sccs) (cntr, vns, s, p, sccs) w_s
      in
      match p with
      | [] -> assert false
      | ((v'_,_),_) :: p when v_ = v'_ ->
          let rec pop scc = function
            | (v'_,_)::s ->
                if v_ = v'_ then (v'_::scc), s
                else pop (v'_::scc) s
            | _ -> assert false
          in
          let scc, s = pop [] s in
          cntr, vns, s, p, scc::sccs
      | _ -> cntr, vns, s, p, sccs
    in
    let _, _, _, _, sccs = f 0 [] [] [] [] (List.hd es) in
    sccs

  let group_type_declarations tds =
    let names = List.map (fun td -> td.ptype_name.txt) tds in
    let alist = List.map (fun td -> td.ptype_name.txt, td) tds in
    let mutually_defined td =
      filter_map (function Longident.Lident s when List.mem s names -> Some s | _ -> None)
      & constrs_in_type_declaration td
    in
    let graph, nonrecs = List.partition_map (fun td ->
      match mutually_defined td with
      | [] -> `Right td.ptype_name.txt
      | ns -> `Left (td.ptype_name.txt, ns)) tds
    in
    let groups = sccs graph in
    (List.map (List.map (flip List.assoc alist)) groups,
     List.map (flip List.assoc alist) nonrecs)
      
  let is_gadt type_decl = match type_decl.ptype_kind with
    | Ptype_variant constrs -> List.exists (fun c -> c.pcd_res <> None) constrs
    | _ -> false
end

let raise_errorf = Location.raise_errorf
type 'a loc = 'a Location.loc = { txt : 'a; loc : Location.t }
