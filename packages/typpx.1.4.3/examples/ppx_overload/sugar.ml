open Ast_helper
open Parsetree
open Asttypes
open Ast_mapper

let extend super = 
  let structure_item self sitem = match sitem.pstr_desc with
    | Pstr_extension ( ({txt="overloaded"}, PStr [{pstr_desc= Pstr_primitive vd}] ), _attr ) ->
       (* val%overloaded x : t  =>  external x : t = "%OVERLOADED" *)
       Str.primitive ~loc:sitem.pstr_loc { vd with pval_prim = [ "%OVERLOADED" ] }
    | _ -> super.structure_item self sitem
  in    
  let signature_item self sitem = match sitem.psig_desc with
    | Psig_extension ( ({txt="overloaded"}, PSig [{psig_desc= Psig_value vd}] ), _attr ) ->
       (* val%overloaded x : t  =>  external x : t = "%OVERLOADED" *)
       Sig.value ~loc:sitem.psig_loc { vd with pval_prim = [ "%OVERLOADED" ] }
    | _ -> super.signature_item self sitem
  in    
  { super with signature_item; structure_item }
