(* Time-stamp: <modified the 29/08/2019 (at 15:19) by Erwan Jahier> *)

(** Define the Data Structure representing Compiled programs. By
    compiled we mean that constant are propagated, packages are
    instanciated, recursive node are inlined, etc.
    
    lic = lustre internal code

    Basically it is Lustre with all the suggar removed.
*)

(**
        Définition des structures de données utilisée pour la compil,
        plus des utilitaires pour les messages d'erreurs, de bug etc.
        N.B. on utilise beaucoup l'adjectif "effectif", qui signifie
        simplement "correct" (c'est bizzare mais c'est ainsi.

        REMARQUE GENERALE :

        D'une manière générale, la compil d'une entité syntaxique
        "toto" est implémentée par une fonction check_toto, qui
        prend en entrée (entr'autre) un toto et renvoie un
        toto.

        TYPES DE DONNEES :

        - type_ :
        dénotation de type effectif, implémente l'équivalence des types,
        construit à partir d'une type_exp.

        - const :
        dénotation de constante effective,
        construit à partir d'une val_exp => IL S'AGIT DE LA REPRESENTATION
        INTERNE DES CONSTANTES STATIQUES 

        - var_info :
        déclaration de variable,
        construit à partir de var_info.

        - val :
        union entre const et var_info.

        - slice_info :
        dénotation de tranche de tableau,
        construit à partir de slice_info.

        - left :
        version compilée de left_part

        - eq_info :
        version compilée de eq_info 


        - node_exp :
        dénotation d'opération,
        peut être  predef ou utilisateur,
        construit à partir de node_exp.

        - static_arg :
        déclaration d'un static arg

        - pack_env :
        la "grosse" structure de données qui gère la compilation
        des packages => implémentée dans CheckGlobal pour la partie type/const/function
        (initialisation) et dans CheckNode pour la partie node/template qui
        est faite à la demande.

        UTILITAIRES :

        - type_of_const : renvoie le type d'une const
        - string_of_type : pretty-print d'un type 
        - string_of_const : pretty-print d'une const 
        - string_of_node_key : pretty-print d'un node_key
        _ string_of_slice :

----------------------------------------------------------------------*)

open Lv6errors 
open Printf 
open Lxm
(* open AstCore *)
let _dbg = (Lv6Verbose.get_flag "lazyc")

(*---------------------------------------------------------------------
  Type : type
  -----------------------------------------------------------------------
  Dénotation de type immédiat : l'équivalence sémantique des types
  EST l'équivalence structurelle des types. 
  Par rapport à une type_exp :
  - pas d'alias
  - taille des tableaux résolues
  ----------------------------------------------------------------------*)

type type_ =
  | Bool_type_eff
  | Int_type_eff
  | Real_type_eff
  | External_type_eff of Lv6Id.long
  | Abstract_type_eff of Lv6Id.long * type_ 

  (*   | Alias_type_eff of Lv6Id.long *)

  | Enum_type_eff     of Lv6Id.long * (Lv6Id.long list)
  | Array_type_eff    of type_ * int
  | Struct_type_eff   of 
      Lv6Id.long * (Lv6Id.t * (type_ * const option)) list
  | TypeVar of type_var
(* [Overload] is like [Any], except that it can only be [int] or [real] *)
and type_var =
  | Any
  | AnyNum

(* pascal : A VIRER A MOYEN TERME ! 
   R1 : euh... a voir. Pour l'instant, ca ne sert plus car Pascal a
   débranché la verif de type lors de l'instanciation de noeud. J'en
   aurais peut-etre besoin le jour où j'y rebrancherai.
*)
and node_profile = (Lv6Id.t * type_) list * (Lv6Id.t * type_) list
and profile = type_ list * type_ list

and slice_info = {
  (* Dénotation de tranche de tableau correcte :
      si A est le tableau d'entrée, alors S est le tableau
      de sortie avec :
      S[i] = A[first + i*step] pour i = 0 .. width
  *)
  se_first : int;
  se_last : int;
  se_step : int;
  se_width : int; (* -> size? *)
}

and left =
  (* Version checkée des left_part
      (les idents, les index et les tranches sont résolus)

      N.B. On conserve aussi le type effectif de chaque noeud
      bien qu'il soit possible de le retrouver. 

      N.B. On garde aussi l'info source des idents au cas ou.*)
  | LeftVarLic  of (var_info * Lxm.t)
  | LeftFieldLic of (left * Lv6Id.t * type_)
  | LeftArrayLic of (left * int * type_) (* XXX should be called LeftArrayIndexLic? *)
  | LeftSliceLic of (left * slice_info * type_)


and eq_info = left list * val_exp

and val_exp =
    { ve_core : val_exp_core ; 
      ve_typ : type_ list ;
      (* An empty list means that its type has not been computed (EvalType.f) yet.
         a cleaner solution would be to define two versions of val_exp: one with
         type info, and one without. But it is a big mutually recursive thing,
         and doing that would be a little bit heavy...
         XXX why not an option type? because of tuples?
      *)
      ve_clk : clock list;
      ve_src : Lxm.t
    (* ditto *)
    }
(* CallByPosLic est (sans doute ?)
    le BON endroit pour stocker l'information de 'matches',
    i.e. est-ce qu'un 'type_matches' a été nécessaire
    pour typer l'appel de l'opérateur ?
*)
and val_exp_core =
  | CallByPosLic  of (by_pos_op srcflagged * val_exp list)
  | CallByNameLic of (by_name_op srcflagged * (Lv6Id.t srcflagged * val_exp) list)
  | Merge of val_exp * (const srcflagged * val_exp) list

and by_name_op =
  | STRUCT of Lv6Id.long
  | STRUCT_with of Lv6Id.long * Lv6Id.t (* XXX devrait etre une expression !!! *)
  | STRUCT_anonymous

and by_pos_op =
  | PREDEF_CALL of node_key srcflagged
  | CALL of node_key srcflagged
  | CONST_REF of Lv6Id.long
  | CONST of const
  | VAR_REF of Lv6Id.t

  | PRE
  | ARROW
  | FBY
  | CURRENT of Lv6Id.long option (* hold the clock constructor; the clock var is 
                                     provided via the args (val_exp list) *)
  (* nb : we have an option type because we know the clock after
     clock checking only *)

  | WHEN of clock
  | TUPLE

  | CONCAT
  | HAT of int
  | ARRAY
  | STRUCT_ACCESS of Lv6Id.t

  (* those are different from [by_pos_op] *)

  | ARRAY_ACCES of int
  | ARRAY_SLICE of slice_info


(*---------------------------------------------------------------------
  Type : const
  -----------------------------------------------------------------------
  Dénotation de constante immédiate
  N.B. les const "portent" leur type :
  - il est implicite pour bool, int, real, 
  - explicite pour extern, enum et struct
  - pour array c'est le TYPE DES ELEMENTS QU'ON TRIMBALE 
  VOIR => type_of_const
  ----------------------------------------------------------------------*)
and const =
    (* type predef *)
    Bool_const_eff of bool
  | Int_const_eff of string
  | Real_const_eff of string
  (* type atomique non predef : on précise le type *)
  | Extern_const_eff of (Lv6Id.long * type_)
  | Abstract_const_eff of (Lv6Id.long * type_ * const * bool)
  (* if the abstract const is extern (i.e., defined as an extern
     in the provided part), then the bool flag is set to
     true. 
  *)
  | Enum_const_eff   of (Lv6Id.long * type_)
  (* type_ tructure : liste (champ,valeur) + type_ structure *)
  | Struct_const_eff of ((Lv6Id.t * const) list * type_)
  (* type_ tableau : liste des valeurs + type_ des elts + taille 
     Is it really a good idea to live both with 
        - constant arrays (and struct), i.e.,  Array_const_eff
        - array of constants, i.e., ARRAY(const)
     ? 
   *)
  | Array_const_eff of (const list * type_) (* type of the const element *)
  | Tuple_const_eff of const list
(*---------------------------------------------------------------------
  Type: val   
  -----------------------------------------------------------------------
  Une constante ou une variable
  => item de la table des symboles de valeurs
  ----------------------------------------------------------------------*)
(*---------------------------------------------------------------------
  Type: var_info      
  -----------------------------------------------------------------------
  Info associée à un ident de variable
  ----------------------------------------------------------------------*)
(* ICI à completer/modifier sans doute *)
and var_info = {
  var_name_eff   : Lv6Id.t;
  var_nature_eff : AstCore.var_nature;
  var_number_eff : int;
  var_type_eff   : type_;
  var_clock_eff : id_clock;
}
and id_clock = Lv6Id.t * clock
(* 
   A pair made of an ident and its clock.

   The ident is used to relate arguments and parameters
   when clocking node calls (in order to be able to transmit
   the clocking constraints between the node I/O, in EvalClock.f).
*)
and clock =
  | BaseLic
  | ClockVar of int (* to deal internally with polymorphic clocks (i.e., constants) *)
  | On of (Lv6Id.long * Lv6Id.t * type_) * clock 
(* - The clock constructor (holding the clock value), 
   - the clock variable
   - the type of the clock variable (enum or bool)
   - the clock of the clock
   
   e.g., in 
   On(clk_constructor, clk_var, clk_typ, sub_clk )
   sub_clk is the clock of clock clk_var
*)

(**********************************************************************************)
(** [node_exp] correspond à une instance de template (ou, cas
    limite, de noeud sans param statique).

    La clé est un couple ident/liste d'arguments statiques effectifs

    N.B. une horloge formelle est soit None (base) soit l'index d'une
    entrée (0..nb entrées-1). Les formal-clocks sont créées au cours du
    type-checking (et pas du clock-checking)

*) 
and node_exp = {
  node_key_eff : node_key;
  inlist_eff   : var_info list;
  outlist_eff  : var_info list;
  loclist_eff  : var_info list option; (* None => extern or abstract *)
  def_eff      : node_def;
  has_mem_eff  : bool;
  is_safe_eff  : bool;
  lxm          : Lxm.t;
(* is_polym_eff : bool *)
}

and type_matches = (type_var * type_) list

and node_def =
  | ExternLic
  | MetaOpLic
  | AbstractLic of node_exp option (* None if extern in the provide part *)
  | BodyLic of node_body

and node_body = {
  asserts_eff : (val_exp srcflagged) list;
  eqs_eff     : (eq_info srcflagged) list;
}

(* key used  for type, constant, and clock tables *)
and item_key = Lv6Id.long
and node_key = item_key * static_arg list
and static_arg =
  (* may be a tuple *)
  | ConstStaticArgLic of (Lv6Id.t * const)
  | TypeStaticArgLic of (Lv6Id.t * type_)
  (* | NodeStaticArgLic of (Lv6Id.t * sarg_node_eff * node_exp) *)
  | NodeStaticArgLic of (Lv6Id.t * node_key)

and sarg_node_eff = node_key * var_info list * var_info list

(****************************************************************************)
(* Because of clocks and types, we cannot rely on compare; hence this
   dedicated function *)
let (compare_var_info : var_info  -> var_info -> int) = 
  fun v1 v2 -> 
    if 
      (v1.var_name_eff =  v2.var_name_eff) 
      && (v1.var_nature_eff = v2.var_nature_eff) 
      && (v1.var_number_eff = v2.var_number_eff) 
    then
      0
    else compare v1 v2
      
(****************************************************************************)
(** Type check_flag

   Au cours du check, on conserve le statut des idents :
   
   - Checking => en cours de traitement, permet de lever les récursions
   - Checked  => traité et correct
   - Incorrect => déjà marqué comme incorrect (pas besoin d'un nouveau
   message d'erreur)
*)
type 'a check_flag =
    Checking
  | Checked of 'a
  | Incorrect


let (profile_of_node_exp : node_exp -> profile) =
  fun ne -> 
    List.map (fun vi -> vi.var_type_eff) ne.inlist_eff,
    List.map (fun vi -> vi.var_type_eff) ne.outlist_eff
    
(****************************************************************************)
(* currently not used *)


(* type world_env = { *)
(*   wenv_src : AstV6.pack_or_model list; *)
(*   wenv_mod_srcs : (Lv6Id.t, AstV6.model_info srcflagged) Hashtbl.t ; *)
(*   wenv_pack_srcs :  (Lv6Id.t, AstV6.pack_info srcflagged) Hashtbl.t ; *)
(*   wenv_pack_envs : (Lv6Id.t, pack_env) Hashtbl.t ; *)
(* } *)
(* and pack_env = { *)
(*   penv_world : world_env ; *)
(*   (* penv_src : AstV6.package ; *) *)
(*   penv_type_table  : (Lv6Id.t, type check_flag)   Hashtbl.t ; *)
(*   penv_const_table : (Lv6Id.t, const check_flag)  Hashtbl.t ; *)
(*   penv_oper_table  : (Lv6Id.t, node_half) Hashtbl.t ; *)
(*   penv_node_table : (node_key, node_exp check_flag) Hashtbl.t *)
(* } *)

(* the local tables are indexed by Lv6Id.t, because local idents (var,const, flow)
   cannot have any package name. 

   and for nodes, the only possibility to have an entry in this table is via the
   static parameters.
   
   i.e.  
        min_4 = min_n<< 4, toto<<2>> >> ;

   is not allowed (I think). One has to write something like :

      toto_2 = toto<<2>>;
      min_4 = min_n<< 4, toto_2 >> ;

   It would not be difficult to handle that here though.
*)


(****************************************************************************)
(** [type_are_compatible t1 t2] checks that t1 is compatible with t2, i.e., 
    if t1 = t2 or t1 is abstract and not t2.
*)
let (type_are_compatible : type_ -> type_ -> bool) =
  fun te1 te2 -> match te1, te2 with
    | External_type_eff (id1), External_type_eff (id2) -> id1 = id2
    | External_type_eff _, _ -> true
    | Abstract_type_eff _, _ -> true
    | TypeVar Any, _ -> true
    | _, TypeVar Any -> true
    | (TypeVar AnyNum), Real_type_eff | Real_type_eff, (TypeVar AnyNum)
    | (TypeVar AnyNum), Int_type_eff | Int_type_eff, (TypeVar AnyNum) -> true
    | t1, t2 -> t1 = t2

let (is_extern_type : type_  -> bool) =
  fun te -> match te with
  | External_type_eff _  -> true
  | _  -> false


let (clock_are_equals : clock -> clock -> bool) =
  fun c1 c2 -> match c1, c2 with
    | On(cid1,_), On(cid2,_) -> cid1 = cid2
(* equivalent ? try both before commit !!! *)
(*     | On(_,c1), On(_,c2) -> clock_are_equals c1 c2 *)
    | c1, c2 -> c1 = c2

let (var_are_compatible : var_info -> var_info -> bool) =
  fun v1 v2 -> 
    (type_are_compatible v1.var_type_eff v2.var_type_eff) &&
      (clock_are_equals (snd v1.var_clock_eff) (snd v2.var_clock_eff))
  
let ident_of_type = function
  | Bool_type_eff -> Lv6Id.out_of_pack "bool"
  | Int_type_eff -> Lv6Id.out_of_pack "int"
  | Real_type_eff -> Lv6Id.out_of_pack "real"
  | External_type_eff id
  | Abstract_type_eff (id, _)
  | Enum_type_eff     (id, _)
  | Struct_type_eff   (id, _) -> id
  | TypeVar Any -> Lv6Id.out_of_pack "any"
  | (TypeVar AnyNum) -> Lv6Id.out_of_pack "anynum"
  | Array_type_eff(_,_) -> assert false

(****************************************************************************)
(* Utilitaires liés aux node_key *)
let (node_key_of_idref : Lv6Id.idref -> node_key) =
  fun nkey -> (Lv6Id.long_of_idref nkey, [])

let (node_key_of_ident : string -> node_key) =
  fun id -> (Lv6Id.long_of_string id, [])

(* OBSOLETE ET UN PEU FAUX ! 
   R1: pas forcément obsolete ; cf commentaire plus haut. 
*)
let rec (subst_type : type_ -> type_ -> type_) =
  fun t teff_ext -> match teff_ext with
      (* substitutes [t] in [teff_ext] *)
    | Bool_type_eff -> Bool_type_eff
    | Int_type_eff -> Int_type_eff
    | Real_type_eff -> Real_type_eff
    | External_type_eff(l) -> External_type_eff(l)
    | Abstract_type_eff(l,t) -> Abstract_type_eff(l,t)
    | Enum_type_eff(l,el) ->  Enum_type_eff(l,el)
    | Array_type_eff(teff_ext,i) -> 
        Array_type_eff(subst_type t teff_ext, i)
    | Struct_type_eff(l, fl) -> 
        Struct_type_eff(
          l, List.map (fun (id,(teff,copt)) -> (id,(subst_type t teff,copt))) fl)
    | TypeVar Any 
    | (TypeVar AnyNum) -> t

(* *) 
let rec subst_matches (matches: type_matches) (t: type_) : type_ =
   match t with
   | Bool_type_eff
   | Int_type_eff
   | Real_type_eff
   | External_type_eff _
   | Enum_type_eff _ -> t
   (* normallement, seul cas récursif ? *)
   | Array_type_eff(telts,i) -> 
      Array_type_eff(subst_matches matches telts, i)
   (* NE DEVRAIENT PAS ETRE RECURSIFS
      on utilse paranoid au cas où ...
   *)
   | Abstract_type_eff(l,td) ->
      Lv6Verbose.exe ~flag:Lv6MainArgs.paranoid ( fun () ->
         let t' = Abstract_type_eff(l,subst_matches matches td) in
         if t <> t' then assert false   
      );
      t
   | Struct_type_eff(l,fl) -> 
      Lv6Verbose.exe ~flag:Lv6MainArgs.paranoid ( fun () ->
         let t' = Struct_type_eff(
            l, List.map (fun (id,(teff,copt)) -> (id,(subst_matches matches teff, copt))) fl)
         in
         if t <> t' then assert false   
      );
      t
   | TypeVar tvar ->
      try (List.assoc tvar matches) with Not_found -> t

let apply_type_matches (matches: type_matches) (tl: type_ list) : type_ list =
   match matches with
   | [] -> tl
   | _ -> List.map (subst_matches matches) tl


let rec (type_is_poly : type_ -> bool) =
  fun t -> match t with
    | Bool_type_eff
    | Int_type_eff 
    | Real_type_eff
    | External_type_eff _
    | Enum_type_eff(_)  -> false
    (* peut-être un alias ! *)
    | Abstract_type_eff (_id,te) -> type_is_poly te
    | TypeVar Any 
    | (TypeVar AnyNum) -> true
    | Array_type_eff(teff_ext,_i) -> type_is_poly teff_ext
    | Struct_type_eff(_l, fl) -> 
        List.exists (fun (_,(teff,_)) -> type_is_poly teff) fl

let node_is_poly (ne:node_exp) : bool =
   (* let it, ot = profile_of_node_exp ne in *)
   let varispoly v = type_is_poly v.var_type_eff in 
   List.exists varispoly ne.inlist_eff
   ||
   List.exists varispoly ne.outlist_eff

let node_is_extern (ne:node_exp) : bool =
   match ne.def_eff with
   | ExternLic -> true
   | _ -> false

let (is_extern_const : const  -> bool) =
  fun te -> match te with
  | Extern_const_eff _  
  | Abstract_const_eff (_,_,_, true) -> true
  | _  -> false

let (val_exp_is_constant : val_exp  -> bool) = function
  | { ve_core = CallByPosLic({it=(CONST_REF _ | CONST _); _}, _);_ }  -> true
  | _ -> false

let type_of_val_exp ve = ve.ve_typ

let rec lxm_of_val_exp ve = 
  match ve.ve_core with
    | CallByPosLic  (x,_) -> x.src
    | CallByNameLic (x, _) -> x.src
    | Merge(ve, _) -> lxm_of_val_exp ve

(********************************************************************************)
(* for source level info : we want the smallest and the highest lxm of the expr *)
let min_lxm lxm1 lxm2 =
  if (line lxm1) = 0 then lxm2 else
    if (line lxm1) = (line lxm2) then
      if (cstart lxm1) < (cstart lxm2) then lxm1 else lxm2
    else if (line lxm1) <  (line lxm2) then lxm1 else lxm2
      
let max_lxm lxm1 lxm2 =
  if (line lxm1) = (line lxm2) then
    if (cend lxm1) > (cend lxm2) then lxm1 else lxm2
  else if (line lxm1) > (line lxm2) then lxm1 else lxm2


(********************************************************************************)

    
(* Ne doit être appelée que pour les constantes simples *)
let (type_of_const: const -> type_) =
  function
    | Bool_const_eff _ -> Bool_type_eff
    | Int_const_eff  _ -> Int_type_eff
    | Real_const_eff _ -> Real_type_eff
    | Extern_const_eff (_s,  teff) -> teff
    | Abstract_const_eff (_s,  teff, _v, _is_exported) -> teff
    | Enum_const_eff   (_s,  teff) -> teff
    | Struct_const_eff (_fl, teff) -> teff
    (* | Array_const_eff  (ct, teff) -> teff (* Array_type_eff (teff, List.length ct) *) *)
    | Array_const_eff  (ct, teff) -> Array_type_eff (teff, List.length ct)
    | Tuple_const_eff _cl -> 
      (* Utiliser plutot types_of_const (ci dessous) qui traite les tuples *)
		print_internal_error "Lic.type_of_const" "should not have been called for a tuple";
		assert false

(* accepte un UNIQUE niveau de tuple *)
let (types_of_const: const -> type_ list) =
	function
	| Tuple_const_eff cl -> List.map type_of_const cl
	| c -> [type_of_const c]


(* const list *)

(* Ignore the abstraction layer (necessary when expanding struct) *)
(* XXX not used anymore. This is very suspect... *)
let (true_type_of_const: const -> type_) =
  function
    | Abstract_const_eff (_s,  teff, _v, _is_exported) -> teff
    | teff -> type_of_const teff

let (type_of_left: left -> type_) =
  function
    | LeftVarLic(vi,_lxm)   -> vi.var_type_eff 
    | LeftFieldLic(_, _, t) -> t 
    | LeftArrayLic(_, _, t) -> t
    | LeftSliceLic(_, _, t) -> t 

let rec (lxm_of_left: left -> Lxm.t) =
  function
    | LeftVarLic(_,lxm) -> lxm 
    | LeftFieldLic(l, _, _) 
    | LeftArrayLic(l, _, _) 
    | LeftSliceLic(l, _, _) -> lxm_of_left l 
 
let rec (var_info_of_left: left -> var_info) =
  function
    | LeftVarLic  (v, _) -> v
    | LeftFieldLic(left,_,_) -> var_info_of_left left
    | LeftArrayLic(left,_,_) -> var_info_of_left left 
    | LeftSliceLic(left,_,_) -> var_info_of_left left

let (clock_of_left: left -> clock) =
  fun left -> 
    snd (var_info_of_left left).var_clock_eff


let string_of_ident = Lv6Id.string_of_long_bis false

let rec string_of_type = function
  | Bool_type_eff -> "bool"
  | Int_type_eff  -> "int"
  | Real_type_eff -> "real"
  | External_type_eff (name) -> (string_of_ident name)
  | Abstract_type_eff (name, _t) -> (string_of_ident name)
  | Enum_type_eff (name, _) -> (string_of_ident name)
  | Array_type_eff (ty, sz) ->
    Printf.sprintf "%s^%d" (string_of_type ty) sz
  | Struct_type_eff (name, _) -> (string_of_ident name)
  | TypeVar Any -> "any"
  | (TypeVar AnyNum) -> "anynum"

and string_of_type_list = function
  | []  -> ""
  | [x] -> string_of_type x
  | l   -> String.concat " * " (List.map string_of_type l)

and string_of_type_profile (i, o) =
  (string_of_type_list i)^" -> "^(string_of_type_list o)

and string_of_clock = function
  | BaseLic -> " on base"
  | ClockVar i -> " on 'CV"^(string_of_int i)
  | On ( (cc,cv,_ct), ck) -> 
     " on "^(Lv6Id.string_of_long false cc) ^ "("
     ^ (Lv6Id.to_string cv) ^ ")"  ^(string_of_clock ck)

and enum_to_string s ll =
  match Lv6MainArgs.global_opt.Lv6MainArgs.expand_enums with
    | Lv6MainArgs.AsInt -> (string_of_int (Lv6util.pos_in_list 0 s ll))
    | Lv6MainArgs.AsBool 
    | Lv6MainArgs.AsConst 
    | Lv6MainArgs.AsEnum -> string_of_ident s

and string_of_const = function
  | Bool_const_eff true -> "true"
  | Bool_const_eff false -> "false"
  | Int_const_eff i -> (sprintf "%s" i)
  | Real_const_eff r -> r
  | Extern_const_eff (s,_) -> (string_of_ident s)
  | Abstract_const_eff (s,_t,_v,_) -> (string_of_ident s)
  | Enum_const_eff   (s,Enum_type_eff(_,ll)) -> enum_to_string s ll
  | Enum_const_eff _ -> assert false
  | Struct_const_eff (fl, t) -> 
    let string_of_field (id, veff) =
      (Lv6Id.to_string id)^" = "^ (string_of_const veff)
    in
    Printf.sprintf "%s{%s}"
      (string_of_type t)
      (String.concat "; " (List.map string_of_field fl))
  | Array_const_eff (ctab, _t) ->
    Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_const ctab))
  | Tuple_const_eff   cl ->
    Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_const cl))

and string_of_var_info x =
  (AstCore.string_of_var_nature x.var_nature_eff) ^ " " ^
    (Lv6Id.to_string x.var_name_eff) ^ ":"^(string_of_type x.var_type_eff)^
      (string_of_clock (snd x.var_clock_eff)^"("
       ^ (Lv6Id.to_string (fst x.var_clock_eff)) ^","^
         (string_of_int x.var_number_eff)^")")

and string_of_var_list vl = String.concat " ; " (List.map string_of_var_info vl)

and string_of_node_key = function
  | (ik, []) ->
    (string_of_ident ik)
  | (ik, sargs)  -> Printf.sprintf "%s<<%s>>"
    (string_of_ident ik)
    (String.concat ", " (List.map string_of_static_arg sargs))

and string_of_static_arg = function
  | ConstStaticArgLic(id, ceff) -> Printf.sprintf "const %s = %s" id (string_of_const ceff)
  | TypeStaticArgLic (id, teff) -> Printf.sprintf "type %s = %s" id (string_of_type teff)
  (* | NodeStaticArgLic (id, ((long,sargs), _, _), _) -> *)
  | NodeStaticArgLic (id, nk) ->
    Printf.sprintf "node %s = %s" id (string_of_node_key nk)

and string_of_type_var tv = string_of_type (TypeVar tv)
and string_of_type_matches pm =
  let sotm (tv,t) = Printf.sprintf "%s <- %s"
    (string_of_type_var tv) (string_of_type t)
  in
  String.concat ", " (List.map sotm pm)

let string_of_node_exp ne =
  (Printf.sprintf "   node_key_eff = %s\n" (string_of_node_key ne.node_key_eff))
^ (Printf.sprintf "   inlist_eff   = %s\n" (string_of_var_list ne.inlist_eff))
^ (Printf.sprintf "   outlist_eff  = %s\n" (string_of_var_list ne.outlist_eff))
  (* ne.loclist_eff  : var_info list option; (* None => extern or abstract *) *)
  (* ne.def_eff      : node_def; *)
  (* ne.has_mem_eff  : bool; *)
  (* ne.is_safe_eff  : bool; *)

(* NodeStaticArgLic of (Lv6Id.t * sarg_node_eff * node_exp) *)
(* sarg_node_eff = node_key * var_info list * var_info list *)

(* utile : liste standard de var_info a partir de liste de type *)
let create_var_list nat tl = 
  let pfx = match nat with
    | AstCore.VarInput -> "i"
    | AstCore.VarOutput -> "o"
    | AstCore.VarLocal -> assert false
  in
  let cpt = ref 1 in
  let dovar t = (
    let i = !cpt in
    let id = Printf.sprintf "%s%d" pfx i in
    incr cpt;
    {
      var_name_eff   = id;
      var_nature_eff = nat;
      var_number_eff = i;
      var_type_eff   = t;
         (* ???? *)
      var_clock_eff  = (id, BaseLic);
    }
  ) in List.map dovar tl 

let create_var nat t name = 
  {
    var_name_eff   = name;
    var_nature_eff = nat;
    var_number_eff = 0;
    var_type_eff   = t;
         (* ???? *)
    var_clock_eff  = (name, BaseLic);
  }

(*---------------------------------------------------------------------
Une erreur associée à un noeud + 1 lexeme dans le fichier source
----------------------------------------------------------------------*)
exception Compile_node_error of node_key * Lxm.t * string
exception Global_node_error of node_key * string

(******************************************************************************)
(* topologically sort vars wrt their clock dependecency *)

  
module TopoSortVarInfo = 
  TopoSort.Make(
      struct 
        type elt = var_info
        type store = (var_info, var_info list) Hashtbl.t
        let find_dep tbl x = try Hashtbl.find tbl x with Not_found -> []
        let have_dep tbl x = try Hashtbl.find tbl x <> [] with Not_found -> false
        let remove_dep tbl x = Hashtbl.remove tbl x; tbl
      end
    )

(* Looks like the one in LicTab *)
let (sort_var_info :  var_info list -> var_info list) =
  fun vars  ->  (* we sort vars according to their clock deps *)
  let vartable = Hashtbl.create (List.length vars) in
  let find_direct_dep  v _vars =
    match snd v.var_clock_eff with
    | BaseLic -> None
    | ClockVar _ -> None
    | On((_cc,cv,_ct), _sclk) ->
       (*        Printf.printf "sort_var_info: %s depends on %s\n" v.var_name_eff cv; *)
       flush stdout;
       try Some (Hashtbl.find vartable cv)
       with Not_found -> None
  in 
  let dep_star vars =
    let tbl = Hashtbl.create (List.length vars) in
    let rec find_deps v = 
      if Hashtbl.mem tbl v then Hashtbl.find tbl v else
        match find_direct_dep v vars with
        | None -> []
        | Some v2 ->
           let v2_deps = find_deps v2 in
           let v_deps = v2::v2_deps in
           Hashtbl.replace tbl v v_deps;
           v_deps
    in
    List.iter  
      (fun v ->   
       match find_deps v with 
       | [] -> Hashtbl.remove tbl v (* cleaning *) 
       | _::_ -> () 
      ) 
      vars; 
    tbl
  in
  List.iter (fun v -> Hashtbl.add vartable v.var_name_eff v) vars;
  TopoSortVarInfo.f (dep_star vars) vars
   
 
