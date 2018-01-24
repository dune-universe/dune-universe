open Ast_405

open Parsetree
open Asttypes
open Ppxx.Utils
open Ppxx.Helper
open Longident

let seed = 
  String.sub 
    (Digest.to_hex (Digest.string (String.concat " " (Array.to_list Sys.argv))))
    0
    8

let create_id = 
  let cntr = ref 0 in
  fun prefix -> 
    incr cntr; 
    Printf.sprintf "%s_%s_%d" prefix seed !cntr

class top = object (self)

  inherit Ast_mapper_class_405.mapper as super

  val mutable top = true
  val mutable tops = []

  method! structure sitems = 
    let is_top = top in
    top <- false;
    let res = super#structure sitems in
    top <- is_top;
    if is_top then List.rev tops @ res
    else res
  
  method! structure_item sitem =
    let loc = sitem.pstr_loc in
    match sitem.pstr_desc with
    | Pstr_extension (({txt="top"}, PStr xs), _attrs) ->
        let mname = create_id "Top" in
        tops <- 
          super#structure 
          & Str.module_ ~loc 
            (Mb.mk ~loc (at ~loc mname)
               (Mod.structure ~loc xs))
          :: tops;
        Str.open_ ~loc (Opn.mk ~loc (at ~loc & Lident mname))
    | _ -> super#structure_item sitem

  method! expr e = 
    let loc = e.pexp_loc in
    let others, top_attrs = 
      List.partition_map (function 
        | ({txt="top"},x) -> `Right x
        | y -> `Left y) e.pexp_attributes
    in
    let e = { e with pexp_attributes = others } in
    let untop e =
      match e.pexp_desc with
      | Pexp_let (rf, vbs, e) ->
          let mname = create_id "Top" in
          let loc = e.pexp_loc in
          tops <- 
            Str.module_ ~loc 
              (Mb.mk ~loc (at ~loc mname)
                (Mod.structure ~loc [ self#structure_item & Str.value ~loc rf vbs ]))
            :: tops;
          Exp.open_ ~loc (at ~loc (Lident mname)) & super#expr e
      | Pexp_letmodule ((_ as a), me, e) ->
          let mname = create_id "Top" in
          let loc = e.pexp_loc in
          tops <- 
            Str.module_ ~loc 
              (Mb.mk ~loc (at ~loc mname) me)
            :: tops;
          Exp.letmodule ~loc a (Mod.ident ~loc & at ~loc (Lident mname)) & super#expr e
      | _ -> assert false
    in
    match top_attrs with
    | _::_ ->
        let vname = create_id "top" in
        (* let %top vname = e in vname *)
        untop
        & Exp.(let_ ~loc Nonrecursive 
                 [Vb.mk ~loc (Pat.var' ~loc vname) e] 
                 (var ~loc vname))
    | [] ->
        match e.pexp_desc with
        | Pexp_extension ({txt="top"}, PStr [{pstr_desc= Pstr_eval (e, _)}]) -> 
            untop e
        | Pexp_extension ({txt="top"}, _) -> assert false
        | _ -> super#expr e
end
