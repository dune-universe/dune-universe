(* Time-stamp: <modified the 29/08/2019 (at 15:23) by Erwan Jahier> *)

(**
Sous-module pour AstTab 

AstTabSymbol.t =
   tout ce qui concerne la résolution des idents "simples" (snas le pack::)
   dans un contexte particulier.
   Essentiellement, un ident simple qui apparait dans un contexte
   est soit une reference locale (donc au pack courant)
   soit une reference a un pack "importé" (via "uses", i.e. le open de caml !)
*)
open Lxm
open AstCore
open Lv6errors

let dbg = (Lv6Verbose.get_flag "ast")

(* get trace of raise Global_error in debug mode *)
let do_raise_global_error msg =
   Lv6Verbose.printf ~flag:dbg "#DBG: up to raise global error:\n   %s\n" msg;
   raise (Global_error msg)

let do_raise_compile_error (lxm,msg) =
   Lv6Verbose.printf ~flag:dbg "#DBG: up to raise compile error:\n    %s: %s\n" (Lxm.details lxm) msg;
   raise (Compile_error (lxm,msg))


type 'a elt =
  | Local of 'a
  | Imported of Lv6Id.long * static_param srcflagged list

type t = {
  st_consts: (Lv6Id.t , (Lv6Id.pack_name * const_info srcflagged elt)) Hashtbl.t ;
  st_types : (Lv6Id.t , (Lv6Id.pack_name * type_info  srcflagged elt)) Hashtbl.t ;
  st_nodes : (Lv6Id.t , (node_info  srcflagged) elt) Hashtbl.t ;
} 

(* Création/initialisation d'une symbol_tab *)
let create () = 
  let consts_tbl = Hashtbl.create 50
  and types_tbl  = Hashtbl.create 50
  and nodes_tbl  = Hashtbl.create 50
  in
    {
      st_consts = consts_tbl;
      st_types  = types_tbl;
      st_nodes  = nodes_tbl;
    }

let find_type (this: t) (id: Lv6Id.t) lxm =
  try snd (Hashtbl.find (this.st_types) id)
  with Not_found -> 
    raise (Compile_error(lxm, "unknown type (" ^ (Lv6Id.to_string id)^")"))

let find_pack_of_type (this: t) (id: Lv6Id.t) lxm =
   try
      let res = fst (Hashtbl.find (this.st_types) id) in
      Lv6Verbose.printf ~flag:dbg
         "#DBG: AstTabSymbol.find_pack_of_type %s -> %s\n" id res;
      res
  with Not_found -> 
    raise (Compile_error(lxm, "unknown type (" ^ (Lv6Id.to_string id)^")"))

  
let find_const (this: t) (id: Lv6Id.t) lxm = 
  try snd (Hashtbl.find (this.st_consts) id)
  with Not_found -> 
    raise (Unknown_constant(lxm,  (Lv6Id.to_string id)))

let find_pack_of_const (this: t) (id: Lv6Id.t) lxm = 
   try
      let res = fst (Hashtbl.find (this.st_consts) id) in
      Lv6Verbose.printf ~flag:dbg
         "#DBG: AstTabSymbol.find_pack_of_const  %s -> %s\n" id res;
      res
  with Not_found -> 
    raise (Unknown_constant(lxm,  (Lv6Id.to_string id)))


let find_node (this: t) (id: Lv6Id.t) lxm =
  try Hashtbl.find (this.st_nodes) id
  with Not_found -> 
    if Lxm.line lxm = 0 && Lxm.cend lxm = 0 then
      (* A hack to print a nicer error msg when the node asked in the 
	      command-line is not found in the input files*)
      do_raise_global_error ("Can not find node " ^ (Lv6Id.to_string id))
      (* raise (Global_error("Can not find node " ^ (Lv6Id.to_string id))) *)
    else
      let all_nodes = 
        Hashtbl.fold (fun n _ acc -> (Lv6Id.to_string n)::acc) this.st_nodes [] 
      in
      let msg = "unknown node: " ^ (Lv6Id.to_string id)^"\n" ^ 
        "*** known nodes are: " ^ (String.concat ", " all_nodes) ^ "\n"
      in
      do_raise_compile_error (lxm, msg)
      (* raise (Compile_error(lxm, msg)) *)


(* Manip de AstTabSymbol.t *)
let add_import_const (this: t) (pn:Lv6Id.pack_name) (id: Lv6Id.t) (aid: Lv6Id.long) =
  Hashtbl.replace (this.st_consts) id (pn, Imported (aid, []))

let add_import_type (this: t) (id: Lv6Id.t) (aid: Lv6Id.long) =
  Hashtbl.replace (this.st_types) id (Lv6Id.pack_of_long aid, Imported (aid, []))

let add_import_node (this: t) (id: Lv6Id.t) (aid: Lv6Id.long) 
    (params:static_param srcflagged list) =
  Hashtbl.replace (this.st_nodes) id (Imported (aid, params))

let add_const (this: t) (pn:Lv6Id.pack_name) (n: Lv6Id.t) 
    (cix: (const_info  srcflagged)) =
  Hashtbl.replace this.st_consts n (pn, Local cix)

let add_type (this: t) pn (n: Lv6Id.t) (tix: type_info srcflagged) = (
  Hashtbl.replace this.st_types n (pn, Local tix) ;
  (* cas particulier des types enums *)
  match tix.it with
      EnumType (_, ecl) -> (
	     let tname = Lxm.str tix.src in
	     let treat_enum_const ec = (
	       let te = Named_type_exp { Lv6Id.id_pack = None; Lv6Id.id_id = tname} in
	       let tex = Lxm.flagit te tix.src in
	       let ci = EnumConst (ec.it, tex) in
	       add_const this pn ec.it (Lxm.flagit ci (ec.src));
	       add_const this pn ec.it (Lxm.flagit ci (ec.src))
	     ) in
	     List.iter treat_enum_const ecl
      )
    | _ -> ()
)

let add_node (this: t) (n: Lv6Id.t) (oix: node_info  srcflagged) =
  Hashtbl.add this.st_nodes n (Local oix)
 

(* let iter_types  this f = Hashtbl.iter f this.st_types *)
let iter_types  this f = Hashtbl.iter (fun id (_pn,ti) -> f id ti) this.st_types
let iter_consts this f = Hashtbl.iter (fun id (_pn,ci) -> f id ci) this.st_consts
let iter_nodes  this f = Hashtbl.iter f this.st_nodes
(* let iter_consts2 this f = Hashtbl.iter f this.st_consts *)


