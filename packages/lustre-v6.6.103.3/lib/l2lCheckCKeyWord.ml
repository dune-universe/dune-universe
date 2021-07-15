(* Time-stamp: <modified the 29/08/2019 (at 15:12) by Erwan Jahier> *)

open Lic
 
let _dbg =  (Lv6Verbose.get_flag "C")

(********************************************************************************)


  
module StringSet = 
  Set.Make(struct
             type t = string
             let compare = compare
           end)

let c_kw_list = [
  "asm"; "auto"; "break"; "case"; "cdecl"; "char"; "const"; "continue";
  "default"; "do"; "double"; "else"; "enum"; "extern"; "far"; "float"; "for";
  "goto"; "huge"; "if"; "inline"; "int"; "interrupt"; "long"; "near"; "pascal";
  "register"; "restrict"; "return"; "short"; "signed"; "sizeof"; "static";
  "struct"; "switch"; "typedef"; "union"; "unsigned"; "void"; "volatile";
  "while"; "_Bool"; "_Complex"; "_Imaginary"] 

                  

                
let c_kw = StringSet.of_list c_kw_list

let (check_var_info : Lic.var_info -> unit) =
  fun v ->   
  if StringSet.mem v.var_name_eff c_kw then
	 let msg = Printf.sprintf "'%s' is a reserved C word. You cannot use it as a Lustre ident *and* use the C code generator, sorry" v.var_name_eff
    in
     raise (Lv6errors.Global_error msg)
	
  
let (check_type :Lic.type_ -> unit) =
  fun  _typ ->
    (*  (match typ.Named_type_exp with
	|id_pack -> ()
	|id_id ->) *)
    ()
(*    List.iter check_var_info typ.named_type_exp.id_id
*)  
let (check_const : Lic.const -> unit) =
  fun  _const ->
  (*  List.iter check_var_info const.CallByName.STRUCT_n.id_id
  *)
()	 
 (*   (match const.CallByName.STRUCT_n with 
    |id_pack -> ()
    |id_id -> 
      List.iter Check_var_info id_id
    )
 *)
(*  | CallByName of (by_name_op srcflagged  * (Lv6Id.t srcflagged * val_exp) list) *)

  
let (check_node : Lic.node_exp -> unit) =
  fun node ->
    List.iter check_var_info  node.inlist_eff;
    List.iter check_var_info  node.outlist_eff;
    (match node.loclist_eff with
    | None -> () 
    | Some l ->   List.iter check_var_info l)  

(********************************************************************************)

(* exported *)

let (doit :  LicPrg.t -> unit) =
  fun inprg -> 
    LicPrg.iter_nodes (fun _ n -> check_node n) inprg;
    LicPrg.iter_types  (fun _ t -> check_type t) inprg;
    LicPrg.iter_consts (fun _ c -> check_const c) inprg;
