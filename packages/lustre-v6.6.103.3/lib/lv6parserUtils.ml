(* Time-stamp: <modified the 29/08/2019 (at 14:46) by Erwan Jahier> *)

(** *)

(**********************************************************************************)



open Lxm
open AstCore
open AstPredef

(*
qq types intermédiares utilisés au cours du parse (mly)
pour y voir + clair ...
regle générale : pour un non-terminal Toto, type sx_Toto 
*)

type sx_VarDeclList = (((Lxm.t list) * type_exp) list  * AstCore.clock_exp) list
type sx_Params = sx_VarDeclList
type sx_LocalVars = sx_VarDeclList


let (build_node_var : var_info srcflagged list -> var_info srcflagged list -> 
     var_info srcflagged list option -> node_vars) =
  fun invars outvars locvars_opt -> 
    let get_var_name vif = vif.it.var_name in
    {
      inlist  = List.map get_var_name invars;
      outlist = List.map get_var_name outvars;
      loclist = (
        match locvars_opt with
          | None -> None
          | Some locvars -> Some (List.map get_var_name locvars)
      );
      vartable = 
        let tbl = Hashtbl.create 0 in
        let add_var_in_tbl vif = Hashtbl.add tbl vif.it.var_name vif in
        List.iter add_var_in_tbl invars;
        List.iter add_var_in_tbl outvars;
        (match locvars_opt with
          | None -> ()
          | Some locvars -> List.iter add_var_in_tbl locvars
        );
        tbl;
    }

(* Une collection de "meta fonctions" pour faciliter la vie *)


(*------------------------------------------------------
flat_flagged_list
--------------------------------------------------------
Entrée :
--------
- inlist : ('a list * 'b) list
- makeitem : 'a -> 'b -> c' 
--------------------------------------------------------
Sortie :
--------
- outlist : c' list
--------------------------------------------------------
Effets de bords :
---------------
- aucun en interne
- makeitem est appelée de gauche à droite
--------------------------------------------------------
Exemple :
-----------------
flat_flagged_list [ ([a1;a2;a3], b1) ; ([a4;a5], b2) ] f
<=>
let c1 = (f a1 b1) in
let c2 = (f a2 b1) in
let c3 = (f a3 b1) in
let c4 = (f a4 b2) in
let c5 = (f a5 b2) in
[ c1; c2; c3; c4; c5 ]
------------------------------------------------------*)
let flat_flagged_list 
    (inlist:   ('a list * 'b) list)
    (makeitem: 'a -> 'b -> 'c) 
    = (
      (*g: concatene les 'c list*)
      let g (cl: 'c  list) ((al: 'a list) , (b: 'b)) = (
        (*f: fabrique un 'c *)
        let f (a: 'a) = makeitem a b in 
          List.append cl (List.map f al) 
      ) in 
        (*on folde g sur inlist*)
        List.fold_left g [] inlist
    )

let _ = assert (
  (flat_flagged_list 
     [ (["a1";"a2";"a3"], "b1") ; (["a4";"a5"], "b2") ] 
     (fun a b -> a ^ "-" ^ b))
  = 
    ["a1-b1"; "a2-b1"; "a3-b1"; "a4-b2"; "a5-b2"]
  )

(*------------------------------------------------------
flat_twiced_flagged_list
--------------------------------------------------------
même principe mais avec deux niveaux de flags :

let mk a b c = (a,b,c)

let toto =
[
([ ([1;2;3], "a"); ([4;5], "b") ], "X") ;
([ ([6], "c"); ([7;8], "d"); ([9], "e")  ], "Y") ;
( [ ([10], "f") ]   , "Z")
]

  let l = flat_twice_flagged_list toto mk
  ------------------------------------------------------*)
let flat_twice_flagged_list 
    (inlist:   (('a list * 'b) list * 'c) list )
    (makeitem: 'a -> 'b -> 'c -> 'd ) 
    = (
      let g (dl: 'd list) ((albl: ('a list * 'b) list), (c: 'c)) = (
        let h (dl: 'd list) ((al: 'a list), (b: 'b)) = (
          let f (a: 'a) = makeitem a b c in
            List.append dl (List.map f al)
        ) in
          List.fold_left h dl albl
      ) in
        List.fold_left g [] inlist
    )


(**********************************************************************************)
(* Interface avec AstV6 *)

(* we store ident names in a table to be able to generated fresh var
   name that won't clash afterwards *)
let (name_table : (string, unit) Hashtbl.t) = 
  Hashtbl.create 0 

let idref_of_lxm lxm =
  let name = (Lxm.str lxm) in
    if name.[0] = '_' then (
      Hashtbl.add name_table name ());
    try Lxm.flagit (Lv6Id.idref_of_string name) lxm
    with _ ->
      print_string  ("Lv6parser.idref_of_lxm" ^(Lxm.str lxm));
      assert false

(**********************************************************************************)
(** Traitement des listes d'idents avec valeur éventuelle
    (constantes, champs de struct etc...)
*)
let (lexeme_to_ident_flagged: Lxm.t -> Lv6Id.t Lxm.srcflagged) = 
  fun x -> {it = (Lxm.id x); src = x }

let (lexeme_to_val_exp_flagged: Lxm.t -> val_exp Lxm.srcflagged) = 
  fun x -> 
    let idref = idref_of_lxm x in
    let ve = CallByPos({ it = IDENT_n idref.it ; src=x },Oper []) in
    {it = ve; src = x }

let (lexeme_to_pack_name_flagged:Lxm.t -> Lv6Id.pack_name  Lxm.srcflagged) = 
  fun x -> {it = (Lv6Id.pack_name_of_string (Lxm.str x)); src = x }


let (make_merge_bool_op : Lxm.t -> val_exp -> val_exp ->  val_exp) =
  fun enum_clk vet vef ->
    Merge_bool_n(lexeme_to_val_exp_flagged enum_clk, vet, vef) 

type bool_or_idref = Bool of bool | Idref of Lv6Id.idref 
(** Utilitaries to build [val_exp]  *)
let make_merge_op (enum_clk:Lxm.t) (l:(bool_or_idref * Lxm.t * val_exp) list)  = 
  match l with
    | [(Bool true ,_,vet); (Bool false,_,vef)]
    | [(Bool false,_,vef); (Bool true ,_,vet)] -> 
      make_merge_bool_op enum_clk vet vef
    | _ -> 
      let l = List.map
        (fun (b_or_idref,lxm,ve) ->
          match b_or_idref with
            | Idref idref -> flagit idref lxm, ve
            | Bool true   
            | Bool false ->
              raise (
                Lv6errors.Compile_error (enum_clk, "The merge mixes booleans and enums"))
        )
        l
      in
      Merge_n(lexeme_to_val_exp_flagged enum_clk, l)

let save_make_merge_op (enum_clk:Lxm.t) (l:(Lv6Id.idref srcflagged * val_exp) list)  = 
  let l = List.map (fun (idref,ve) -> idref,ve) l in
  Merge_n(lexeme_to_val_exp_flagged enum_clk, l) 



let make_predef_posop lxm op =
   let op = flagit op lxm in
   {src = lxm ; it = Predef_n (op) }

let leafexp lxm op =
   CallByPos({src = lxm ; it = op }, Oper [])

let leafexp_predef lxm op =
   let op = flagit op lxm in
   CallByPos({src = lxm ; it = Predef_n (op) }, Oper [])

let unexp lxm op e1 =
   CallByPos( {src = lxm ; it = op }, Oper [e1] )    

let unexp_predef lxm op e1 =
   let op = flagit op lxm in
   CallByPos( {src = lxm ; it = Predef_n (op) }, Oper [e1] )
            
let binexp lxm op e1 e2 =
   CallByPos( {src = lxm ; it = op }, Oper [e1 ; e2] ) 

let binexp_predef lxm op e1 e2 =
   let op = flagit op lxm in
   CallByPos( {src = lxm ; it = Predef_n (op) }, Oper [e1 ; e2] ) 

let ternexp lxm op e1 e2 e3 = CallByPos( {src = lxm ; it = op }, Oper [e1 ; e2; e3] )

let ternexp_predef lxm op e1 e2 e3 =
   let op = flagit op lxm in
   CallByPos( {src = lxm ; it = Predef_n (op) }, Oper [e1 ; e2; e3] )

let naryexp lxm op elst =
   CallByPos( {src = lxm ; it = op }, Oper elst )

let naryexp_predef lxm op elst =
   let op = flagit op lxm in
   CallByPos( {src = lxm ; it = Predef_n (op) }, Oper elst )


let bynameexp lxm op nelst = CallByName( {src = lxm ; it = op } , nelst )



(**********************************************************************************)
(** add_info
-----------------------------------------------------------------------
Rôle :
        proc générique pour mettre une info 'a dans
         une table (Lv6Id.t, 'a srcflagged).

Effets de bord :
        erreur de compil si déjà utilisé
*)
let (add_info : (Lv6Id.t, 'a srcflagged) Hashtbl.t -> 
      string -> (* une string en cas d'erreur   *)
      Lxm.t ->  (* le lexeme en question        *)
      'a ->     (* l'info en question           *)
      unit) = 
  fun htbl kindof lxm info -> 
    try
      let x  = Hashtbl.find htbl (Lxm.id lxm) in 
        raise (
          Lv6errors.Compile_error ( 
            lxm, 
            Printf.sprintf "bad %s declaration, ident already linked at %s" kindof 
              (Lxm.position x.src)
          )
        )
    with Not_found ->
      Hashtbl.add htbl (Lxm.id lxm) { src = lxm ; it  = info }
        

(**********************************************************************************)
(* local tables used to store (via [add_info], see above) intermediary results 

   Most of the function below (treat_<something>) returns unit but modifies
   one or several of those tables.
*)

let (const_table:(Lv6Id.t, const_info srcflagged) Hashtbl.t) = Hashtbl.create 50 
let (type_table :(Lv6Id.t, type_info  srcflagged) Hashtbl.t) = Hashtbl.create 50
let (node_table :(Lv6Id.t, node_info  srcflagged) Hashtbl.t) = Hashtbl.create 50
let (def_list : item_ident list ref) = ref []

  
(**********************************************************************************)


(* Listes d'idents typés et (éventuellement) valués *)
type id_valopt = (Lxm.t * type_exp * val_exp option)

(* Pas de valeur : le type distribue sur une liste d'ident *)
let id_valopt_list_of_id_list (idlist : Lxm.t list) (texp : type_exp) = 
  let treat_id (id : Lxm.t) = (id, texp, None) in 
    List.map treat_id idlist

(* Avec valeur : il ne doit y avoir qu'un seul ident *) 
let id_valopt_of_id_val (id : Lxm.t) (texp : type_exp) (vexp : val_exp) = (* -> unit *)
  (id, texp, Some vexp)


let make_external_const_list lst typ = (* -> (lxm * const_info) list *)
  let f = function lxm -> (lxm,  (ExternalConst ((Lxm.id lxm), typ, None)))
  in List.map f lst

let make_defined_const lxm typ exp = (* -> (lxm * const_info) *)
	(lxm, (DefinedConst ((Lxm.id lxm) , typ, exp)))

let treat_const_decl_list clst =
  let f = function (lxm, cinfo) ->
    add_info const_table "constant" lxm cinfo;
    def_list := (ConstItem (Lxm.id lxm)) :: !def_list
  in 
    List.iter f clst

(* treat type decls *)

let (make_struct_type_info :  Lxm.t -> id_valopt list (* la liste des champs *) -> 
      struct_type_info) =
  fun typlxm flexlist -> 
    (* On anticipe la construction de la table de champs *)
    let ftab = Hashtbl.create 50 in
    let (put_in_ftab : (Lxm.t * type_exp * val_exp option) -> Lv6Id.t) =
      (* Traitement d'un champ élémentaire *)
      fun (lx, ty, va) -> 
        (* fabrique le field_info *)
        let lxstr = Lxm.id lx in
        let fi = { fd_name = lxstr ; fd_type = ty ; fd_value = va } in
          (* le range dans ftab *)
          add_info ftab "field" lx fi;
          lxstr (* renvoie juste le nom du champs *)
    in
    let flst = List.map put_in_ftab flexlist in
      { st_name = Lxm.id typlxm ; st_flist = flst ; st_ftable = ftab }

let treat_type_decl (typlxm, typinfo) = (
	let typstr = Lxm.id typlxm in
	add_info type_table "type" typlxm typinfo ;
	def_list := (TypeItem typstr) :: !def_list
)
  

(**********************************************************************************)
(********************************************)
(* Déclarations de vars et params de noeuds *)
(********************************************)
(*
Un peu coton à cause des types, clocks,
et de la syntaxe laxiste sur la distribution
de ces flags dans les déclarations de variables !
On utilise un artifice local pour
homogénéiser le traitements de listes de vars :
- clocked_ids list
*) 
type typed_ids = (Lxm.t list * type_exp)    
type clocked_ids = (typed_ids list * clock_exp)

let (clocked_ids_to_var_infos : var_nature -> 
      (((Lxm.t list) * type_exp) list * AstCore.clock_exp) list -> 
      var_info srcflagged list) =
  fun vnat vdefs ->
    let i = ref 0 in
    let makevar lxm te ce =
      let res =
        Lxm.flagit 
          {
            var_nature = vnat;
            var_name = (Lxm.id lxm);
            var_number = !i;
            var_type = te;
            var_clock = ce;
          }
          lxm
      in
        incr i;
        res
    in
      flat_twice_flagged_list vdefs makevar


(**********************************************************************************)
(*
2010/07/02
les déclarations locales comportent :
- une liste de vars * une liste de consts
*)
let treat_node_decl
   (is_unsafe: bool)
   (has_memory: bool)
   (nlxm: Lxm.t)
   (statics: static_param srcflagged list)
   (indefs: clocked_ids list)
   (outdefs: clocked_ids list)
   (locdecls: clocked_ids list * (Lxm.t * const_info) list)
   (asserts: (val_exp srcflagged) list)
   (eqs: (eq_info srcflagged) list)
: unit =
    let (locdefs, locconsts) = locdecls in
    let vtable = Hashtbl.create 50 in
    let rec (treat_vars : clocked_ids list -> var_nature -> var_info srcflagged list) =
      (* Procedure de traitement des in, out ou loc, paramétrée par la [var_nature] *)
      fun vdefs nat -> 
        let i = ref 0 in
          match vdefs with
            | [] -> []
            | (tids, ck)::reste ->
                let put_var_in_table (lxm: Lxm.t) (ty: type_exp) =
                  let vinfo = {
                    var_nature = nat; var_name = (Lxm.id lxm); 
                    var_type = ty; var_clock = ck; var_number = !i
                  }
                  in
                    incr i;
                    add_info vtable "variable" lxm vinfo;
                    Lxm.flagit vinfo lxm 
                in
                  (flat_flagged_list tids put_var_in_table) 
                  @ (treat_vars reste nat)
    in
    let invars  = treat_vars indefs  VarInput 
    and outvars = treat_vars outdefs VarOutput 
    and locvars = treat_vars locdefs VarLocal 
    in 
    let vars = build_node_var invars outvars (Some locvars) in
    let nstr = Lxm.id nlxm in
    let ninfo = {
      name = nstr;
      static_params = statics;
      vars    = Some vars;
      loc_consts  = locconsts;
      def     = Body { asserts = asserts ; eqs  = eqs };
      has_mem = has_memory;
      is_safe = not is_unsafe;
    }
    in
      add_info node_table "node" nlxm ninfo;
      def_list := (NodeItem (nstr,statics)) :: !def_list


(**********************************************************************************)
let (treat_node_alias : bool -> bool -> Lxm.t -> static_param srcflagged list -> 
      (var_info srcflagged list * var_info srcflagged list) option -> 
      node_exp srcflagged -> unit) = 
  fun is_unsafe has_memory nlxm statics node_profile value -> 
    let nstr = Lxm.id nlxm in
    let vars = 
      match node_profile with
        | None -> None
        | Some (invars,outvars) -> Some (build_node_var invars outvars None)
    in
    let ninfo = {
      name = nstr;
      static_params = statics;
      vars    = vars;
      loc_consts  = [];
      def     = Alias (flagit (CALL_n value) value.src);
      has_mem = has_memory;
      is_safe = not is_unsafe ;
    }
    in
      add_info node_table "(alias) node" nlxm ninfo;
      def_list := (NodeItem (nstr,statics)) :: !def_list
 
  

(**********************************************************************************)
(* Traitement d'un noeud abstrait *)


(* cf the profile of [treat_abstract_node] *)
let treat_abstract_or_extern_node_do
   (is_unsafe: bool)
   (has_memory: bool)
   (lxm: Lxm.t)
   (statics: static_param srcflagged list)
   (inpars: sx_Params)
   (outpars: sx_Params)
   (is_abstract: bool)
: node_info =
  let (invars, outvars : var_info srcflagged list * var_info srcflagged list) = 
    clocked_ids_to_var_infos VarInput  inpars,
    clocked_ids_to_var_infos VarOutput outpars 
  in
  let vars = build_node_var invars outvars None in
  let xn = {
    name = Lxm.id lxm;
    static_params = statics;
    vars    = Some vars;
    loc_consts  = [];
    def     = if is_abstract then Abstract else Extern;
    has_mem = has_memory;
    is_safe = not is_unsafe;
  }
  in
    xn

let treat_abstract_node
   (is_unsafe: bool)
   (has_memory: bool)
   (lxm: Lxm.t)
   (statics: static_param srcflagged list)
   (inpars: sx_Params)
   (outpars: sx_Params)
: item_info Lxm.srcflagged =
    Lxm.flagit 
      (NodeInfo (treat_abstract_or_extern_node_do is_unsafe has_memory lxm statics inpars outpars true))
      lxm

  
(**********************************************************************************)
(* external nodes never have static params *)
let treat_external_node
   (is_unsafe: bool)
   (has_memory: bool)
   (ext_nodelxm: Lxm.t)
   (inpars: sx_Params)
   (outpars: sx_Params)
: unit =
   let statics = [] in (* no static args for external node (for now at least) *)
   let ninfo = 
     treat_abstract_or_extern_node_do (* external nodes look like abstract nodes indeed *)
       is_unsafe has_memory ext_nodelxm statics inpars outpars false
   in
   add_info node_table "(extern) node" ext_nodelxm ninfo ;
      def_list := (NodeItem (Lxm.id (ext_nodelxm),statics)) :: !def_list
  
(**********************************************************************************)
let (threat_slice_start : Lxm.t -> val_exp -> val_exp option -> slice_info srcflagged) =
  fun lxm last step ->
    let str = Lxm.str lxm in
    let int_to_val_exp  istr =
      try 
        ignore (int_of_string istr);
        let ic = flagit (ICONST_n (Lv6Id.of_string(istr))) lxm in
        CallByPos(flagit (Predef_n (ic)) lxm, Oper [])
      with _ ->
        CallByPos(flagit (IDENT_n (Lv6Id.idref_of_string(istr))) lxm, Oper [])
    in
      match Str.split (Str.regexp (Str.quote "..")) str with
        | [first] ->
            let slice_info = 
              { 
                si_first = int_to_val_exp first; 
                si_last = last; 
                si_step = step
              }
            in 
              flagit slice_info lxm
        | _ -> assert false


let (make_ident : Lxm.t -> pragma list -> Lxm.t) =
  fun lxm pl -> 
    if pl = [] then lxm else Lxm.add_pragma lxm pl

(**********************************************************************************)

let (make_clock_exp : Lv6Id.idref -> Lxm.t -> clock_exp) =
  fun str v_lxm -> 
    NamedClock( Lxm.flagit (Lv6Id.long_of_idref str, (Lxm.id v_lxm)) v_lxm)
