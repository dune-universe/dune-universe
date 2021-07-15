(* Time-stamp: <modified the 29/08/2019 (at 15:24) by Erwan Jahier> *)

(** 
    Table des infos sources : une couche au dessus de AstV6 pour mieux
    ranger les packages et les modèles et faciliter la résolution des
    identificateurs.

    1) expansion des modèles

    2) pour chaque package instancié, 2 tables de symboles :

    - une pour la vision "exportée"
    - une pour la vision interne. Chaque table de symbole, 3 "espaces"
    de noms (par nature d'items, type/const/node)

    Ces tables sont destinées à résoudre les références simples, elle
    associent à une string :

    - la definition syntaxique de l'item associé s'il est local
    - l'identificateur absolu (package+nom) si il est externe
*)

open Lxm
open AstV6
open AstCore
open Lv6errors

let dbg = (Lv6Verbose.get_flag "ast")


(** Package manager

    Un package manager (pack_mng) contient les infos ``source'' du
    package + DEUX tables de symboles, correspondant aux deux contextes
    possibles de compilation :

    - compilation du provide
    - compilation du body

    En effet, un identificateur de type, de constante ou de noeud
    n'est pas interprété de la même manière suivant qu'il apparaît
    dans la partie provide ou body.

    Il contient aussi une table des items exportés pour faciliter le
    traitement des "use" du package. C'est une correspondance 

        nature + nom simple -> nom complet 

    (c.a.d. ??? + AstV6.item_ident -> Lv6Id.long)
*)

type pack_mng = {
  (* le lexeme associé au package? *)
  pm_lxm : Lxm.t;
  (* le source brut *)
  pm_raw_src : AstV6.pack_info;
  (* le source expansé *)
  pm_actual_src : AstV6.pack_given;
  (* table "brute" des items provided *)
  (* pour les "user" du pack *)
  pm_user_items : (AstCore.item_ident, Lv6Id.long Lxm.srcflagged) Hashtbl.t;
  (* les tables de symboles pour compil ultérieure *)
  pm_body_stab : AstTabSymbol.t;
  (* la table pour provide n'est créée que si besoin ... *)
  pm_provide_stab : AstTabSymbol.t option;
}

(** TYPE PRINCIPAL : t

    Packages et modèles sont rangés dans des tables, ce qui permet
    notamment de traiter les erreurs de multi-déclarations
    (st_raw_mod_tab et st_raw_pack_tab)

    Les instances de modeles sont traitées pour n'avoir plus que des
    ``pack_given'' (i.e. pack avec provide + body)

    À chaque package (éventuellement expansé) est associé un manager
    pour faciliter l'accès à ses infos (pack_mng)
*)
type t = {
  (* liste + tables des sources bruts *)
  st_list : AstV6.pack_or_model list ;
  st_raw_mod_tab  : (Lv6Id.t , model_info srcflagged) Hashtbl.t ;
  st_raw_pack_tab : (Lv6Id.pack_name , pack_info srcflagged) Hashtbl.t ;
  (* table des managers de packs *)
  st_pack_mng_tab : (Lv6Id.pack_name , pack_mng)  Hashtbl.t; 
}

(* exported *)
let (pack_list:t -> Lv6Id.pack_name list) =
  fun this -> 
    Hashtbl.fold (fun n _p l -> n::l) this.st_pack_mng_tab []


(* exported *)
let (pack_body_env: t -> Lv6Id.pack_name -> AstTabSymbol.t) =
  fun this p -> 
    try  
      (Hashtbl.find this.st_pack_mng_tab p).pm_body_stab
    with Not_found -> 
      print_string ("*** Error: can not find package '" ^ 
		                 (Lv6Id.pack_name_to_string p) ^ "' in the following packages: ");
      Hashtbl.iter 
	     (fun pn _pm -> print_string ("\n***\t '"^(Lv6Id.pack_name_to_string pn)^ "'"))
	     this.st_pack_mng_tab;
      print_string "\n";
      flush stdout;
      exit 2


(* exported *)
let pack_prov_env (this: t) (p: Lv6Id.pack_name)
  : AstTabSymbol.t option =
  try (Hashtbl.find this.st_pack_mng_tab p).pm_provide_stab
  with Not_found -> 
(*       let msg =  *)
(*         ("\n*** Could not find package " ^(Lv6Id.pack_name_to_string p) ^  *)
(*            " in the package table" ) *)
(*       in *)
	None  
(*         raise(Compile_error(lxm, msg))   *)
      


(** Insert an item in the lexeme table. Raise [Compile_error] if already defined. *)
let put_in_tab 
    (what: string)
    (tab : ('a, 'b Lxm.srcflagged) Hashtbl.t)
    (key : 'a)
    (value : 'b Lxm.srcflagged) 
    =
  try 
    let plxm = (Hashtbl.find tab key).src in
    let msg =
      Printf.sprintf "%s already declared in %s" what (Lxm.position plxm) 
    in
      raise (Lv6errors.Compile_error (value.src, msg))
  with 
      Not_found -> Hashtbl.add tab key value


(****************************************************************************
init de la table des items provided (pour les users)
****************************************************************************)
let init_user_items (this: pack_mng) = (
  let pname = Lv6Id.pack_name_of_string (Lxm.str this.pm_lxm) in

  (* Exportation D'une const_info *)
  let export_const (s:Lv6Id.t) (xci: AstCore.const_info srcflagged) =
	 Lv6Verbose.printf ~flag:dbg "       export const %s\n" (Lv6Id.to_string s);
	 put_in_tab "const" this.pm_user_items
	   (ConstItem s)
	   (Lxm.flagit (Lv6Id.make_long pname s) xci.src)
  in
  
  (* Exportation D'un type_info *)
  let export_type (s: Lv6Id.t) (xti: AstCore.type_info srcflagged) = 
    ( match (xti.it) with
	   | EnumType (_, ecl) -> (
	    (* Cas particulier des types enums *)
	    (* on exporte les constantes ... *)
	     let treat_enum_const ec = 
	       let s = ec.it in
	       let lxm = ec.src in
		    Lv6Verbose.printf ~flag:dbg "       export enum const %s\n" (Lv6Id.to_string s);
		    put_in_tab "const" this.pm_user_items
		      (ConstItem s)
		      (Lxm.flagit (Lv6Id.make_long pname s) lxm)
	     in
	     List.iter treat_enum_const ecl
	   )
	   | ExternalType _
	   | AliasedType _ 
	   | StructType _ 
	   | ArrayType _ 
	     -> ()
    );
    Lv6Verbose.printf ~flag:dbg "       export type %s\n" (Lv6Id.to_string s);
    put_in_tab "type" this.pm_user_items
      (TypeItem s)
      (Lxm.flagit (Lv6Id.make_long pname  s) xti.src)
  in

  (* Exportation D'un node_info *)
  let export_node (s: Lv6Id.t) (xoi: AstCore.node_info srcflagged) = 
    Lv6Verbose.printf ~flag:dbg "       export node %s\n" (Lv6Id.to_string s);
    put_in_tab "node" this.pm_user_items
      (NodeItem (s,xoi.it.static_params))
      (Lxm.flagit (Lv6Id.make_long pname s) xoi.src)
  in

  let pg = this.pm_actual_src in

  match pg.pg_provides with
    | None ->
	  (* On Exporte Tout Tel Quel *)
	   Hashtbl.iter export_type  pg.pg_body.pk_type_table ;
	   Hashtbl.iter export_const pg.pg_body.pk_const_table ;
	   Hashtbl.iter export_node  pg.pg_body.pk_node_table ;
    | Some spflg ->
	  (* On Exporte Les Provides *)
	   let treat_prov x = 
	     let lxm = x.src in
	     let s = Lxm.id lxm in
	     match (x.it) with
		      TypeInfo xti -> export_type s (Lxm.flagit xti lxm)
		    | ConstInfo xci -> export_const s (Lxm.flagit xci lxm)
		    | NodeInfo xoi -> export_node s (Lxm.flagit xoi lxm)
	   in
	   List.iter treat_prov spflg
)

(*
Création/initialisation d'un pack_mng :
On prépare juste la table des items provided
pour pouvoir traiter les éventuels "use" des autres pack.
Les tables de symboles sont créées plus tard.
*)
let create_pack_mng 
    (pdata : AstV6.pack_info srcflagged)
    (pgiven : AstV6.pack_given)
    = (
      (* la table pm_provide_stab n'est créée que si besoin *)
  let ppstab = match pgiven.pg_provides with
	   None -> None 
	 | Some _ -> Some (AstTabSymbol.create ())
  in
  let res =
	 {
	   pm_lxm = pdata.src ;
	   pm_raw_src = pdata.it;
	   pm_actual_src = pgiven;
	   pm_user_items = Hashtbl.create 50;
	   pm_provide_stab = ppstab;
	   pm_body_stab = AstTabSymbol.create ();
	 } 
  in
  init_user_items res;
  res
)



(****************************************************************************
CREATION
-----------------------------------------------------------------------------
Se fait en plusieurs passes :

1) mise en place des tables "raw" mod et pack (string -> source pack/mod)
2) instanciations éventuelle des packs (voir AstInstanciateModel)
   et initialisation des pack_mng (en particulier des infos pour les users)
3) pour chaque pack, création des symbol_table contextuelles
   (pour la partie provide et pour la partie body)
****************************************************************************)

let rec (create  : AstV6.pack_or_model list -> t) =
  fun sl -> 
    (* liste + tables des sources bruts *)
    let res = {
      st_list = sl ;
      st_raw_mod_tab = Hashtbl.create 50;
      st_raw_pack_tab = Hashtbl.create 50; 
      st_pack_mng_tab = Hashtbl.create 50; 
    } 
    in
    Lv6Verbose.printf ~flag:dbg "*** AstTab.create pass 1\n";
    (* passe 1 *)
    init_raw_tabs res sl ;
    (* passe 2 *)
    Lv6Verbose.printf ~flag:dbg "*** AstTab.create pass 2\n";
    let init_pack_mng pname pdata = (
	   Lv6Verbose.printf ~flag:dbg "    init pack %s\n" (Lv6Id.pack_name_to_string pname);
	   let pg = AstInstanciateModel.f res.st_raw_mod_tab pdata in
	   Hashtbl.add res.st_pack_mng_tab 
	     pname
	     (create_pack_mng pdata pg)
    ) in
	 Hashtbl.iter init_pack_mng res.st_raw_pack_tab ;
	 (* passe 3 *)
	 Lv6Verbose.printf ~flag:dbg "*** AstTab.create pass 3\n";
	 Hashtbl.iter (init_pack_mng_stabs res) res.st_pack_mng_tab ;
	 (* resultat *)
	 Lv6Verbose.printf ~flag:dbg "*** AstTab.create done\n";
	 res
and
    (***** PASSE 1 *****)
    (* init des tables string -> mod ou pack *)
    init_raw_tabs (this : t) (sl : AstV6.pack_or_model list) =
  (* on itère pour chaque pack_or_model : *)
  let treat_ns ns = 
    match ns with
	   (* cas d'un package *)
	   | AstV6.NSPack  pi ->
	     let lxm = pi.Lxm.src in
	     let nme = (Lv6Id.pack_name_of_string (Lxm.str lxm)) in
	     put_in_tab "package" this.st_raw_pack_tab nme pi
	   	 
	   | AstV6.NSModel mi -> (* cas d'un modele *)
	     let lxm = mi.Lxm.src in
	     let nme = (Lxm.id lxm) in
	     put_in_tab "model" this.st_raw_mod_tab nme mi
  in
  List.iter treat_ns sl
and
    (***** PASSE 3 *****)
    (* Essentiellement le remplissage des champs de pack_mng : 

       pm_provide_stab : AstTabSymbol.t
       table qui permettra de résoudre les refs simples
       à l'intérieur de la partie provides.

       pm_body_stab : AstTabSymbol.t ;
       table qui permettra de résoudre les refs simples
       à l'intérieur de la partie body.

       N.B. s'il n'y a pas de provides explicite, on construit
       une unique table qui sert pour les deux !

       Comment ça marche :
       - on traite en premier les éventuels "use", (= open de ocaml)
       - puis les déclarations locales qui peuvent éventuellement
       masquer les précédentes (warning ?)
    *)
    init_pack_mng_stabs (this: t) (pname: Lv6Id.pack_name) (pm: pack_mng) = (
  let pg = pm.pm_actual_src in

  Lv6Verbose.printf ~flag:dbg "   init symbol tables for pack %s\n"
    (Lv6Id.pack_name_to_string pname);
  (* ON COMMENCE PAR TRAITER LE PG_USES *)
  let treat_uses (px:Lv6Id.pack_name srcflagged) = (
    let pname = px.it in
    let lxm = px.src in
    let pum = 
	   try Hashtbl.find this.st_pack_mng_tab pname
	   with Not_found -> raise(Compile_error(lxm, "unknown package"))
    in
    let fill_used_item
	     (ii: AstCore.item_ident)
	     (iks: Lv6Id.long Lxm.srcflagged) =
	   (match ii with
	     | ConstItem n -> (
	       AstTabSymbol.add_import_const pm.pm_body_stab px.it n iks.it;
	       match pm.pm_provide_stab with
		        Some pt -> AstTabSymbol.add_import_const pt px.it n iks.it
		      | None -> ()
	     )
	     | TypeItem n -> (
	       AstTabSymbol.add_import_type pm.pm_body_stab n iks.it;
	       match pm.pm_provide_stab with
		        Some pt -> AstTabSymbol.add_import_type pt n iks.it
		      | None -> ()
	     )
	     | NodeItem (n,sparams) -> (
	       AstTabSymbol.add_import_node pm.pm_body_stab n iks.it sparams;
	       match pm.pm_provide_stab with
		        Some pt -> AstTabSymbol.add_import_node pt n iks.it sparams
		      | None -> ()
	     )
	   ) 
    in
	 Hashtbl.iter fill_used_item pum.pm_user_items
  )
  in
  List.iter treat_uses pg.pg_uses ;

  (* PUIS LES DECLARATION LOCALES *)
  (* ... dans le body : *)
  Hashtbl.iter (AstTabSymbol.add_type pm.pm_body_stab pname) pg.pg_body.pk_type_table;
  Hashtbl.iter (AstTabSymbol.add_const pm.pm_body_stab pname) pg.pg_body.pk_const_table;
  Hashtbl.iter (AstTabSymbol.add_node pm.pm_body_stab)	 pg.pg_body.pk_node_table;
  (* ... dans le provide : *)
  match pg.pg_provides with
	 | None -> () 
	 | Some spflg -> (
	   let pptab = match pm.pm_provide_stab with
		    Some pt -> pt
	     | None -> assert false
	   in
	   let treat_prov x = 
	     let lxm = x.src in
	     let s = Lxm.id lxm in
		  match (x.it) with
		    | TypeInfo xti  -> AstTabSymbol.add_type pptab pname s (Lxm.flagit xti lxm) 
		    | ConstInfo xci -> AstTabSymbol.add_const pptab pname s (Lxm.flagit xci lxm)
		    | NodeInfo xoi  -> AstTabSymbol.add_node pptab s (Lxm.flagit xoi lxm)
	   in
	   List.iter treat_prov spflg
	 )
)


(****************************************************************************
Associations :
--------------
- Lv6Id.t -> Lv6Id.long * AstCore.xxxx_info

****************************************************************************)


(* exported *)
let (dump : t -> unit) =
  fun x -> 
    (* let p = Lv6Verbose.print_string ~level:3 in *)
    let p = prerr_string in
    p "*** « Syntax table dump:\n";
    
    p " \t - Package or model list:\n\t\t";
    (* st_list : AstV6.pack_or_model list ; *)
    List.iter 
	   (fun pm -> p (AstV6.pack_or_model_to_string pm); p "\n\t\t") 
	   x.st_list ;
    
    p "\n\t - Raw model table: ";
    (* st_raw_mod_tab  : (Lv6Id.t , model_info srcflagged) Hashtbl.t ; *)
    Hashtbl.iter 
	   (fun id _mi -> p ((Lv6Id.to_string id) ^ " "))
	   x.st_raw_mod_tab;
    
    p "\n\t - Raw Package table: ";
    (* st_raw_pack_tab : (Lv6Id.pack_name , pack_info srcflagged) Hashtbl.t ; *)
    Hashtbl.iter 
	   (fun pn _pi -> p ((Lv6Id.pack_name_to_string pn) ^ " ")) 
	   x.st_raw_pack_tab;
    
    p "\n\t - Package manager table: ";
    (* st_pack_mng_tab : (Lv6Id.pack_name , pack_mng)  Hashtbl.t;  *)
    Hashtbl.iter 
	   (fun pn _pm -> p ((Lv6Id.pack_name_to_string pn) ^ " ")) 
	   x.st_pack_mng_tab;
    
    p "\nEnd of Syntax table dump. »\n"


