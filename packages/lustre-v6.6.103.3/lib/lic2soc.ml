(** Time-stamp: <modified the 29/08/2019 (at 16:43) by Erwan Jahier> *)

(* XXX ce module est mal écrit. A reprendre. (R1) *)
 
open Lxm
open Lic

let dbg = (Lv6Verbose.get_flag "exec")

type action = Action.t

(* Raised when a soc that haven't been translated yet is used in
   another soc during the translation *)
exception Undef_soc of Soc.key * Lxm.t * Lic.by_pos_op * Data.t list * Soc.var_expr option 

(*********************************************************************************)
(** Informations liées au contexte de traduction. *)
type ctx    = {
  prg           : LicPrg.t;
  last_temp_var : int;
  last_mem      : int;
  locals        : Soc.var list;
}

let create_context: (LicPrg.t -> ctx) =
  fun prg ->
    {
      prg = prg;
      last_temp_var = 0;
      last_mem      = 0;
      locals        = [];
    }

exception Polymorphic

let rec lic_to_data_type: (Lic.type_ -> Data.t) = 
  function
  | Lic.Bool_type_eff -> Data.Bool
  | Lic.Int_type_eff  -> Data.Int
  | Lic.Real_type_eff -> Data.Real
  | Lic.External_type_eff s -> Data.Extern (Lv6Id.string_of_long false s)
  | Lic.Enum_type_eff     (id, l) -> (
    Data.Enum(Lv6Id.string_of_long false id, List.map (Lv6Id.string_of_long false) l)
  )
  | Lic.Struct_type_eff   (id, fl) -> (
    let trans_field (id,(t,_)) = (* fde_value is ignored. Good idea? *)
      Lv6Id.to_string id, lic_to_data_type t
    in
    let id = Lv6Id.string_of_long false id in
    Data.Struct(id, List.map trans_field fl)
  )
  | Lic.Array_type_eff(ty,i) -> Data.Array(lic_to_data_type ty,i)
  | Lic.Abstract_type_eff (_id, t) -> lic_to_data_type t
  (*     Data.Alias(Lv6Id.string_of_long false id,lic_to_data_type t) *)

  | Lic.TypeVar Lic.Any -> Data.Alpha 0
  | Lic.TypeVar Lic.AnyNum -> 
    (* For some reasons, L2lRmPoly did not manage to resolve all the overloeding.
       In that case, we stop. 

       nb : i raise an exception here because I've got no Lxm.t to use
       to display a nice error message. If ever Polymorphic is raised
       at the toplevel, its means that my "try/with Polymorphic" is not
       done at the right place (as usual)
    *)
    raise Polymorphic


(*********************************************************************************)
(** Renomme une variable définie par l'utilisateur.

    On veut éviter de créer des variables temporaires portant le même nom que
    celles définies par l'utilisateur. Donc on renomme simplement celles de
    l'utilisateur, c'est le plus simple. 

XXX obselete ?
Mieux vaudrait utiliser le meme mechanisme que celui utilisé
actuellement lors des l2l*.ml 
???
*)

 (* R1: je vire le "_" parce que ca m'emmerde ce truc. 
                        On verra plus tard les clash de nom...

bon, des clashs de nom, y'en a pas trop, mais avec les mots clefs C, si !
ex: si un noeud a une variable qui s'appelle "long"...
*) 
let user_var_prefix = ""

let rename_user_var: (string -> string) = fun s ->
  let prefix = user_var_prefix in
  let suffix = "" in
  prefix ^ s ^ suffix

let is_predefined_const: string -> Lic.type_ option = 
  function
    | "true" | "false" -> Some Lic.Bool_type_eff
    | _ -> None

(*********************************************************************************)
(* Returns the list of indexes represented by the slice *)
(* let (slice_info_to_index_list : Lic.slice_info -> int list) = *)
(*   fun si ->  *)
(*     let (f,l,s) = (si.Lic.se_first, si.Lic.se_last, si.Lic.se_step) in *)
(*     let rec aux f = *)
(*       if f>l && s > 0 || f<l && s <0 then [] else *)
(*         f::(aux (f+s)) *)
(*     in *)
(*       aux f *)

let rec (lic2soc_const : Lic.const -> Soc.var_expr list) =
  function
    | Bool_const_eff true -> [Soc.Const("true", Data.Bool)]
    | Bool_const_eff false -> [Soc.Const("false", Data.Bool)]
    | Int_const_eff  i -> [Soc.Const(i, Data.Int)]
    | Real_const_eff r -> [Soc.Const(r, Data.Real)]
    | Extern_const_eff (s,  teff) -> [Soc.Const(Lv6Id.string_of_long false s,
                                                lic_to_data_type teff)]
    | Abstract_const_eff (s,  teff,_,_) -> [Soc.Const(Lv6Id.string_of_long false s,
                                                      lic_to_data_type teff)]
    | Enum_const_eff   (s,  teff) -> 
      let ll = match teff with Enum_type_eff(_,ll) -> ll | _ ->  assert false in
      [Soc.Const(enum_to_string s ll, lic_to_data_type teff)]
    | Struct_const_eff (_fl, _teff) -> assert false (* todo *)
    | Array_const_eff  (_ct, _teff) -> assert false (* todo *)
    | Tuple_const_eff cl -> List.flatten (List.map lic2soc_const cl)

(* Returns Some(thing) if val_exp is a leaf (a var or a constant) 

   XXX c'est pas tres clair le role de cette fonction. Expliquer !
*)
let rec get_leaf: (LicPrg.t -> Lic.val_exp -> Soc.var_expr list option) = 
  fun licprg val_exp  ->
    let v = val_exp.Lic.ve_core in
    let type_ = val_exp.Lic.ve_typ in
    match v with
    | Lic.CallByNameLic(_by_name_op_flg,_fl) -> None
    | Lic.Merge(_c_flg, _cl) -> None
    | Lic.CallByPosLic (by_pos_op_flg, val_exp_list) -> (
        match by_pos_op_flg.it with
        | Lic.VAR_REF name -> 
          let type_ = (List.hd type_) in
          let translation =
            match is_predefined_const name with
            | Some type_ -> Soc.Const(name,  lic_to_data_type type_)
            | None -> Soc.Var(rename_user_var name, lic_to_data_type type_)
          in
          Some [translation]
        | Lic.CONST_REF l -> (
            (match LicPrg.find_const licprg l with
             | Some c -> Some(lic2soc_const c)
             | None -> assert false
            )             
          )
        | Lic.CONST c -> Some(lic2soc_const c)
        | Lic.STRUCT_ACCESS(field) -> (
            let expr = match val_exp_list with [e] -> e | _ -> assert false in
            let type_ = lic_to_data_type (List.hd type_) in
            let filter_expr = match get_leaf licprg expr with
              | Some [f] -> f
              | None -> assert false
              | _ ->  assert false
            in
            Some [Soc.Field(filter_expr, field, type_)]
          )
        | Lic.ARRAY_ACCES i -> (
            let expr = match val_exp_list with [e] -> e | _ -> assert false in
            let type_ = lic_to_data_type (List.hd type_) in
            let filter_expr = match get_leaf licprg expr with
              | Some [f] -> f
              | None -> assert false
              (* should not happen, since the expression should be a leaf *)
              | _ -> assert false
              (* We should get only ONE filter, otherwise it doesn't make any sense *)
            in
            Some [Soc.Index(filter_expr, i, type_)]
          )
        | Lic.TUPLE -> (
            let var_values = List.map (get_leaf licprg) val_exp_list in
            let del_some = function | None -> assert false | Some x -> x in
            Some (List.flatten (List.map del_some var_values))
          )
        | Lic.ARRAY_SLICE _si -> (
            (* XXX is it a good idea to explode slices? 
               let id, t, i = match val_exp_list with 
               | [{Lic.ve_core=Lic.CallByPosLic({it=Lic.VAR_REF id},[]);
                  Lic.ve_typ=[Array_type_eff(t,i)]
                 }] -> id, t, i
               | _ -> assert false
               in
               let t_soc = lic_to_data_type t in
               let type_elt_ref,type_ref = t_soc, Data.Array(t_soc,i) in
               let index_list = slice_info_to_index_list si in
               let exploded_array =  
               (* val_exp is a var ident (t) of type array; we want to gen the list
                 t[i1], ...,t[in], where the index are specified by the slice *)
               List.map
                (fun i -> Soc.Index(Soc.Const(id, type_ref), i, type_elt_ref))
                index_list
               in
            *)
            None
            (*             Some(exploded_array) *)
          )
        | Lic.PREDEF_CALL _
        | Lic.CALL _
        | Lic.PRE
        | Lic.ARRAY 
        | Lic.HAT _
        | Lic.ARROW
        | Lic.FBY
        | Lic.CURRENT _
        | Lic.WHEN(_)
        | Lic.CONCAT
          -> None
      )      
(* Traduction d'une partie gauche d'équation en filtre d'accès soc. *)
let rec filter_of_left_part: (LicPrg.t -> Lic.left -> Soc.var_expr list) = 
  fun licprg lp -> 
    match lp with
      | Lic.LeftVarLic  (vi, _lxm) -> (
        [Soc.Var (rename_user_var vi.Lic.var_name_eff,
                  lic_to_data_type vi.Lic.var_type_eff)]
      )
      | Lic.LeftFieldLic(lp,field,t) -> (
        let lpl = filter_of_left_part licprg lp in
        List.map (fun lp -> Soc.Field(lp, field, lic_to_data_type t)) lpl
      )
      | Lic.LeftArrayLic(lp,index,t) -> (
        let lpl = filter_of_left_part licprg lp in
        List.map (fun lp -> Soc.Index(lp, index, lic_to_data_type t (* type_ ? *))) lpl
      )
      | Lic.LeftSliceLic(lp,si,t) -> (
        let lpl = filter_of_left_part licprg lp in
        List.map (fun lp -> Soc.Slice(lp, si.se_first, si.se_last, si.se_step, 
                                      si.se_width, lic_to_data_type t)) lpl
      )

(*********************************************************************************)

let build_step: Lxm.t -> string -> Lic.node_exp -> Soc.var list -> Soc.gao list -> 
  Soc.step_method =
  fun lxm name node locals gaol ->
    (* Converti les entrées/sorties d'un noeud en index
       d'entrées/sorties du composant *)
    let convert_node_interface = fun l ->
      fst (List.fold_left (fun (a, i) _ -> a @ [i], i+1) ([], 0) l)
    in
      {
        Soc.name    = name;
        Soc.lxm     = lxm;
        Soc.idx_ins  = convert_node_interface node.Lic.inlist_eff;
        Soc.idx_outs = convert_node_interface node.Lic.outlist_eff; 
        Soc.impl     = Soc.Gaol (locals, gaol)
      }

let build_extern_step: Lxm.t -> string -> Lic.node_exp -> Soc.step_method =
  fun lxm name node ->
    (* Converti les entrées/sorties d'un noeud en index
       d'entrées/sorties du composant *)
    let convert_node_interface = fun l ->
      fst (List.fold_left (fun (a, i) _ -> a @ [i], i+1) ([], 0) l)
    in
      {
        Soc.name    = name;
        Soc.lxm     = lxm;
        Soc.idx_ins  = convert_node_interface node.Lic.inlist_eff;
        Soc.idx_outs = convert_node_interface node.Lic.outlist_eff; 
        Soc.impl     = Soc.Extern
(*          Soc.impl     = Soc.Gaol ([], [])  *)
      }

let (lic_val_exp_to_soc_var : LicPrg.t -> Lic.val_exp Lxm.srcflagged ->
                              Lxm.t * Soc.var) = 
  fun prg ve ->
    match get_leaf prg ve.it with
    | Some [Soc.Var v] -> ve.src, v
    | Some _ -> assert false
    | None ->
       failwith ("Should be a var: "^(LicDump.string_of_val_exp_eff false ve.it))
  
let (lic_to_soc_var : Lic.var_info -> Soc.var) = 
  fun vi -> 
    vi.Lic.var_name_eff, lic_to_data_type vi.Lic.var_type_eff

let soc_profile_of_node: Lic.node_exp -> Soc.var list * Soc.var list =
  fun n ->
    let inputs  = List.map lic_to_soc_var n.Lic.inlist_eff in
    let outputs = List.map lic_to_soc_var n.Lic.outlist_eff in
    inputs, outputs 

let (make_soc_key_of_node_key :
       Lic.node_key -> Lic.slice_info option -> Data.t list -> Soc.key) =
  fun nk si_opt vl ->
    let key_opt =
    (match si_opt with
      | None -> Soc.Nomore  
      | Some si -> Soc.Slic(si.Lic.se_first,si.Lic.se_last,si.Lic.se_step))
    in
    let key_opt = 
      if (snd (fst nk)) = "condact" then (
        assert (key_opt=Soc.Nomore);
        Soc.MemInit(Soc.Const("_true", Data.Bool)) (* the first step flag *)
      ) else (
        key_opt
      )
    in
    LicDump.string_of_node_key_rec false false nk, vl, key_opt


let (soc_key_of_node_exp : Lic.node_exp -> Soc.key) =
  fun n -> 
    let svi,svo = soc_profile_of_node n in
    let (id,tl,key_opt) =
      make_soc_key_of_node_key n.node_key_eff None (List.map snd (svi@svo))
    in
    (id,tl,key_opt)

(* Translate val_exp into wires. XXX duplicated code with get_leaf *)
let rec (val_exp_to_filter: LicPrg.t -> Lic.val_exp -> Soc.var_expr list) =
  fun licprg val_exp ->
    let v = val_exp.Lic.ve_core in
    let type_ = val_exp.Lic.ve_typ in
    match v with
    | CallByNameLic(_by_name_op_flg,_fl) -> assert false (* SNO if correctly L2lSpitted *)
    | Merge(_c_flg, _cl) -> assert false (* Should Not Occur if correctly L2lSpitted *)
    | CallByPosLic (by_pos_op_flg, val_exp_list) -> (
        match by_pos_op_flg.it with
        | WHEN(_) ->
          (* ignore it. A good idea? Such when should only appear for const *)
          List.flatten (List.map (val_exp_to_filter licprg) val_exp_list)
        | TUPLE  -> 
          List.flatten (List.map (val_exp_to_filter licprg) val_exp_list)
        | VAR_REF name -> 
          let type_ = (List.hd type_) in
          let translation =
            match is_predefined_const name with
            | Some type_ -> Soc.Const(name,  lic_to_data_type type_)
            | None -> Soc.Var(rename_user_var name, lic_to_data_type type_)
          in
          [translation]
        | CONST_REF l -> (
            (match LicPrg.find_const licprg l with
             | Some c ->
               let by_pos_op_flg = { by_pos_op_flg with it = CONST c } in
               val_exp_to_filter licprg
                 ({ val_exp with Lic.ve_core = CallByPosLic (by_pos_op_flg, [])} ) 
             | None -> assert false
            )
          )
        | CONST c -> const_to_filter c

        | STRUCT_ACCESS(field) -> (
            let expr = match val_exp_list with [e] -> e | _ -> assert false in
            let type_ = match type_ with [t] -> lic_to_data_type t | _ -> assert false in
            let filter_expr = match get_leaf licprg expr with
              | Some [f] -> f
              | None -> assert false
              | _ ->  assert false
            in
            [Soc.Field(filter_expr, field, type_)]
          )
        | ARRAY_ACCES i -> (
            let expr = match val_exp_list with [e] -> e | _ -> assert false in
            let type_ = lic_to_data_type (List.hd type_) in
            let filter_expr = match get_leaf licprg expr with
              | Some [f] -> f
              | None -> assert false
              | _ -> assert false
            in
            [Soc.Index(filter_expr, i, type_)]
          )
        | PREDEF_CALL _ 
        | CALL _
        | PRE
        | ARROW
        | FBY
        | CURRENT _
        | CONCAT
        | HAT _
        | ARRAY
        | ARRAY_SLICE _ -> 
          let lxm = by_pos_op_flg.src in
          let msg = (Lxm.details lxm) ^ 
                    ": only one operator per equation is allowed ("^
                    (LicDump.string_of_val_exp_eff false val_exp)^").\n"
          in
          raise (Lv6errors.Global_error msg)
      )
and (const_to_filter : Lic.const -> Soc.var_expr list) =
  function
  |  (Bool_const_eff true) -> [Soc.Const("true", Data.Bool)]
  |  (Bool_const_eff false) -> [Soc.Const("false", Data.Bool)]
  |  (Int_const_eff i)  -> [Soc.Const(i, Data.Int)]
  |  (Real_const_eff str) -> [Soc.Const(str, Data.Real)]
  |  (Enum_const_eff (str, type_)) ->
    [Soc.Const(Lv6Id.string_of_long false str, lic_to_data_type type_)]
  |  (Array_const_eff (cl, _type_)) ->
    let vell = 
      List.map (fun c ->
          let vel = const_to_filter c in
          vel
        )
        cl
    in
    List.flatten vell
  |  Extern_const_eff (str,type_) ->
    [Soc.Const(Lv6Id.string_of_long false str, lic_to_data_type type_)]

  |  Abstract_const_eff  _ -> assert false
  |  Struct_const_eff _ -> assert false
  |  Tuple_const_eff _ -> assert false

(*********************************************************************************)

let rec (is_a_sub_clock : Lic.clock -> Lic.clock -> bool) =
  fun ck1 ck2 ->
    ck1 = ck2 ||
    match ck1,ck2 with
      | _, BaseLic -> true
      | BaseLic, _ -> false
      | On(_,ck1), On(_,_) -> ck1 = ck2 || is_a_sub_clock ck1 ck2
      | ClockVar _, _ -> assert false
      | _, ClockVar _ -> assert false
      

(* We can have 2 different definitions for the clock of an
   expression. It can be the clock of its output, which is useful to
   check that the lhs and the rhs of an equation are on the same
   clock.  But for node call, it also makes sense to consider that
   the clock of the expression is the clock of its input, as their
   control when the node should be called.

   Anyway, the field ve_clk contains the clock of the outputs, and
   this node compute the clock of the input (i.e., the quicker clock
   among the inputs).

*)
let (clock_of_expr : Lic.val_exp -> Lic.clock) =
  function
  | { ve_core = CallByPosLic({it=CALL _;_}, []) ;_} ->
     BaseLic (* the clock of a node with no arg is the base clock *)
    | { ve_core = CallByPosLic({it=CALL _;_}, args);_ } ->
        let clks = List.map (fun arg -> arg.ve_clk) args in
        let clks = List.flatten clks in
        assert(clks<>[]);
        List.fold_left 
          (fun ck1 ck2 -> 
              if is_a_sub_clock ck1 ck2 then ck2 else
                (assert (is_a_sub_clock ck2 ck1); ck1)
          ) 
          (List.hd clks) (List.tl clks)
    | ve ->
      (* if the expression is not a node call, its clock is the clock
         of its output *)
      (match ve.ve_clk with 
        | clk::_ -> clk  (* no multiclock tuple for the time being *)
        | [] -> assert false)

(*********************************************************************************)
(* type instance_init = Soc.instance * action list (* instance + son initialisation *) *)

(** Créé une opération à partir d'un nom de méthode d'un composant. *)
let soc_step_to_operation: 
    Soc.ident -> Soc.t -> Soc.instance option -> Soc.atomic_operation =
  fun name comp -> function
    | None -> Soc.Procedure (comp.Soc.key) 
    | Some (i) -> Soc.Method(i,name) 

let (action_of_step : Lxm.t -> Soc.t -> Lic.clock -> Soc.var_expr list -> 
      Soc.var_expr list -> Soc.instance option -> Soc.step_method -> action) =
  fun lxm c clk il ol mem step ->
    let inputs  = SocUtils.filter_step_params step.Soc.idx_ins  il in
    let outputs = SocUtils.filter_step_params step.Soc.idx_outs ol in
    let call_action = soc_step_to_operation step.Soc.name c mem in
      (clk, inputs, outputs, call_action, lxm)

(** Créé un nouveau nom pour une instance. *)
let create_new_instance_name: (Soc.key -> ctx -> ctx * string) = fun (soc_name,_,_) ctx ->
  let prefix  = soc_name in
  let suffix  = ""  in
  let make id = Format.sprintf "%s%d%s" prefix id suffix in
  let new_ctx = {ctx with last_mem = ctx.last_mem + 1 } in
    new_ctx, make new_ctx.last_mem

 (** Créé une nouvelle instance pour être utilisée dans un composant.

     Pendant la traduction d'un opérateur, on s'apercoit que cet
     opérateur dispose d'une (ou plusieur) mémoire.  Il faut donc
     qu'on créé une instance de ce composant. *)
let create_instance_from_soc: (ctx -> Soc.t -> ctx * Soc.instance) = 
  fun ctx c ->
    let ctx, inst_name = create_new_instance_name c.Soc.key ctx in
      ctx, (inst_name, c.Soc.key)

(* if the soc has memories (a pre, or node with memory), do create an instance *)
let (make_instance : 
       Lxm.t -> Lic.clock -> ctx -> Soc.t -> ctx * Soc.instance option) =
  fun _lxm _clk ctx soc  -> 
    match soc.Soc.instances with
      | [] -> (
        match soc.Soc.memory with
          | Soc.No_mem -> ctx, None
          | _ -> (* pre/fby/arrow/condact + extern *)
            let ctx, m = create_instance_from_soc ctx soc in
            ctx, Some(m)
      )
      | _  -> (* the soc has sub-soc with memory *)
        let ctx, m = create_instance_from_soc ctx soc in
        ctx, Some(m)

(*********************************************************************************)
(** actions_of_expression_acc translates an expression and an
    accumulator into an new accumulator. The accumulator is augmented
    with the action resulting from the translation of the expression
    plus the new dependancies.

    It also augments the 3rd element of the 5-tuple that holds a list
    of Soc.var_expr ; this list is meant to be used by the recursive calls only
    (i.e., not by actions_of_expression) 
*)
type e2a_acc = 
    ctx * action list * Soc.var_expr list (* this list is used in rec calls*)
    * Soc.instance list * ActionsDeps.t

let rec (actions_of_expression_acc: Lxm.t -> Soc.tbl ->
         Lic.clock -> Soc.var_expr list -> e2a_acc -> Lic.val_exp -> e2a_acc) =
  fun _lxm soc_tbl clk lpl acc expr ->
    let (ctx, al, iol, ml, deps) = acc in
    match get_leaf ctx.prg expr with
      | Some names -> 
        (* expr est déjà une feuille (un ident ou une constante), RAF. *)
        let action = clk, names, lpl, Soc.Assign, expr.ve_src in
        (ctx, action::al, iol@names, ml, deps)
      | None -> (
        let v = expr.Lic.ve_core in
        match v with
          | CallByNameLic(_by_name_op_flg,fl) -> (
            (* Pas de soc pour les structures non plus.  On se
               contente d'éclater la structure en autant d'égalités
               que nécessaire.  *)
            let filter_to_field filter field ftype =
              let ftype =  match ftype  with [x] -> x | _ -> assert false in
              let filter = match filter with [x] -> x | _ -> assert false in
              Soc.Field(filter, field, lic_to_data_type ftype)
            in
            let actions = 
              List.map
                (fun (fn, fv) -> 
                  let ft = fv.ve_typ in
                  let nfv = val_exp_to_filter ctx.prg fv in
                  (clk, nfv, [filter_to_field lpl fn.it ft], Soc.Assign, fn.src)
                )
                fl
            in
            ctx, List.rev_append actions al, iol, ml, deps
          )
          | Merge(mclk, cl) -> (
            (* Merge (like when) does not generate any soc, but states when
               expressions are executed. 

               Here, we split Lic.Merge into several actions. Hopefully,
               the test opening optimisation stage would be able to
               reconstruct this merge into a proper Soc.Case.  
            *)
            let acc = List.fold_left
              (fun acc (cc_flg,ve) ->
                let clk_type = List.hd mclk.ve_typ in
                let clkclk = List.hd mclk.ve_clk in
                let clk_id = match mclk with
                  | { ve_core= CallByPosLic({it=VAR_REF id;_},[]) ;_} -> id
                  | _ -> assert false
                in 
                let cc_long = match cc_flg.it with
                  | Bool_const_eff true  -> "Lustre", "true"
                  | Bool_const_eff false -> "Lustre", "false"
                  | Enum_const_eff(long,_) -> long
                  | _ -> assert false
                in 
                let (clk:Lic.clock) = On((cc_long, clk_id, clk_type),clkclk) in
                let ctx, actions, _, mems, deps = acc in
                let ctx, actions2, inputs, mems2, deps2 =
                  actions_of_expression cc_flg.src soc_tbl ctx clk lpl ve
                in
                let mems = mems@mems2 in
                let deps = ActionsDeps.concat deps deps2 in
                let actions = actions@actions2 in
                ctx, actions, inputs, mems, deps
              )
              acc
              cl
            in
            acc
          )
          | CallByPosLic (by_pos_op_flg, val_exp_list) -> (
            match by_pos_op_flg.it with
              | Lic.WHEN ck -> (
                (* 'when' does not generate any soc, but it states
                   when expressions are executed . *)
                let ctx, actions, inputs, mems, deps =
                  actions_of_expression_list by_pos_op_flg.src soc_tbl clk
                    lpl acc val_exp_list
                in
                let ctx, outputs, actions_reclocked = 
                  match actions with
                  | [] -> (* val_exp is a leaf x. *)
                    let lxm = by_pos_op_flg.src in
                    ctx, lpl, [ck, inputs, lpl, Soc.Assign, lxm]
                    | _  -> ctx, inputs,
                      (* Remplacement de l'horloge des actions de l'expression par
                         la nouvelle horloge issue du `when`. *)
                      List.map (fun (_, i,o,op,lxm) -> ck,i,o,op,lxm) actions
                in
                ctx, actions_reclocked, outputs, mems, deps
              )
              | Lic.VAR_REF _ | Lic.CONST_REF _ | Lic.CONST _
              | Lic.ARRAY_ACCES _ | Lic.STRUCT_ACCESS _ | Lic.TUPLE
                -> assert false (* should not occur: handled via get_leaf *)
              | CURRENT _
              | Lic.ARRAY_SLICE _ 
              | CALL _ | PREDEF_CALL _
              | HAT _ | ARRAY | PRE | ARROW | FBY  | CONCAT -> (
                (* retreive the soc of "expr" in soc_tbl *)
                let soc : Soc.t =
                  let args_types : Data.t list =
                    List.map lic_to_data_type
                      (List.flatten (List.map (fun ve -> ve.ve_typ) val_exp_list))
                  in
                  let res_type = List.map lic_to_data_type expr.ve_typ in
                  (* let (get_exp_type : Soc.var_expr list -> Data.t list) =
                     fun vl -> 
                     let tl = List.map Soc.data_type_of_var_expr vl in
                     tl
                     let res_type =  get_exp_type lpl in *)
                  let full_profile = args_types @ res_type in
                  let si_opt = match by_pos_op_flg.it with
                      Lic.ARRAY_SLICE si -> Some si | _ -> None
                  in
                  (* XXX Béquille en attendant mieux *)
                  let (node_key_of_pos_op : Lic.by_pos_op -> Lic.node_key) = fun op -> 
                    match op with
                      | PRE  -> ("","Lustre::pre"),[]
                      | ARROW -> ("","Lustre::arrow" ),[]
                      | FBY-> ("","Lustre::fby"),[]
                      | CURRENT _ -> ("","Lustre::current"),[]
                      | CONCAT-> ("","Lustre::concat"),[]
                      | ARRAY  -> ("","Lustre::array"),[]
                      | ARRAY_SLICE _ -> ("","Lustre::array_slice"),[]
                      | HAT _ -> ("","Lustre::hat"),[]
                      | CALL n | PREDEF_CALL n -> n.it
                      | _  -> assert false
                  in
                  let node_key = node_key_of_pos_op by_pos_op_flg.it in 
                  let sk = make_soc_key_of_node_key node_key si_opt full_profile in
                  let (sk_name, sk_prof,_) = sk in
                  let sk,fby_init_opt = 
                    match by_pos_op_flg.it with 
                      | Lic.FBY -> 
                        let init = val_exp_to_filter ctx.prg (List.hd val_exp_list) in
                        let init = List.hd init in
                        (sk_name, sk_prof, Soc.MemInit init), Some init
                      | Lic.ARROW -> 
                        let init = Soc.Const("_true", Data.Bool) in
                        (sk_name, sk_prof, Soc.MemInit init), Some init
                      | Lic.CURRENT (Some cc) -> 
                        (sk_name, sk_prof, Soc.Curr(cc)), None
                      | _ ->  sk, None
                  in
                  try SocUtils.find by_pos_op_flg.src sk soc_tbl
                  with Lv6errors.Compile_error(lxm,msg) ->
                    Lv6Verbose.exe ~flag:dbg (fun () -> print_string msg; flush stdout);
                    raise (Undef_soc (sk, lxm,by_pos_op_flg.it,args_types,fby_init_opt))
                in
                make_e2a_elt by_pos_op_flg.src clk lpl acc val_exp_list soc
              )
          )
      )
and (make_e2a_elt: Lxm.t -> Lic.clock -> Soc.var_expr list -> e2a_acc -> 
     Lic.val_exp list -> Soc.t -> e2a_acc) =
  fun lxm clk lpl acc val_exp_list soc -> 
    (* Update the acc with the actions made of the soc call: 
       « lpl = soc(val_exp_list) » on clk
    *)
    let (ctx, al, iol, ml, deps) = acc in
    let inputs = List.flatten (List.map (val_exp_to_filter ctx.prg) val_exp_list) in
    let ctx, mem_opt = make_instance lxm clk ctx soc in
    let actions =
      let m2act = action_of_step lxm soc clk inputs lpl mem_opt in
      List.map m2act soc.Soc.step
    in
    let actions = al @ actions in
    let dependances : ActionsDeps.t =
      let (prefixed_actions : (Soc.ident * action) list) = List.map2
        (fun s a -> s.Soc.name,a) soc.Soc.step actions
      in
      ActionsDeps.generate_deps_from_step_policy
        soc.Soc.precedences prefixed_actions
    in
    let dependances = ActionsDeps.concat deps dependances in
    let ml = match mem_opt with Some m -> m::ml | None -> ml in
    (ctx, actions, iol, ml, dependances)
      
(** Traduction d'une liste d'expressions. *)
and (actions_of_expression_list: Lxm.t -> Soc.tbl -> Lic.clock -> Soc.var_expr list -> 
     e2a_acc -> Lic.val_exp list -> e2a_acc) =
  fun lxm soc_tbl clk lpl expr_list acc ->
    List.fold_left (actions_of_expression_acc lxm soc_tbl clk lpl) expr_list acc

and (actions_of_expression : Lxm.t -> Soc.tbl -> ctx -> Lic.clock -> Soc.var_expr list ->
     Lic.val_exp -> e2a_acc) =
  fun lxm soc_tbl ctx clk lpl expr ->
    let acc0 = (ctx, [], [], [], ActionsDeps.empty) in
    actions_of_expression_acc lxm soc_tbl clk lpl acc0 expr

      

(*********************************************************************************)
(** Translates an equation into one or several actions.
     
    Generated dependencies are merged by the caller. 
 *)
let (actions_of_equation: Lxm.t -> Soc.tbl -> ctx -> Lic.eq_info -> 
      ctx * action list * Soc.instance list * ActionsDeps.t) = 
  fun lxm soc_tbl ctx (left_part, right_part) ->
    let clk = clock_of_expr right_part in 
    let left_loc = List.map (filter_of_left_part ctx.prg) left_part in
    let left_loc = List.flatten left_loc in
    let ctx, actions, _, instances, deps =
      actions_of_expression lxm soc_tbl ctx clk left_loc right_part
    in
      ctx, actions, instances, deps
 
(*********************************************************************************)
open Soc

let profile_info = Lv6Verbose.profile_info

let f: (LicPrg.t -> Lic.node_key -> Soc.key * Soc.tbl) = 
  fun prog mnk ->
    let rec (process_node : Lic.node_key -> Soc.tbl -> Soc.key * Soc.tbl) =
      fun nk soc_tbl -> 
        profile_info ("Lic2soc.process_node "^(Lic.string_of_node_key nk)^"\n");
        let node = 
          match LicPrg.find_node prog nk with
          | None  -> 
            prerr_string (
              "*** "^ (LicDump.string_of_node_key_rec false false nk) ^
              " not defined (as lic).\n" ^
              "*** Defined nodes are:"^ 
              (String.concat
                 ",\n"  
                 (List.map (fun (nk,_) ->
                      "\""^LicDump.string_of_node_key_rec false false nk ^"\"")
                     (LicPrg.list_nodes prog)))
            );
            assert false
          | Some node_exp -> node_exp
        in
        let sk = soc_key_of_node_exp node in
        let soc_tbl = 
          if SocMap.mem sk soc_tbl then soc_tbl else
            try
              (match LicPrg.find_node prog nk with
               | None  -> assert false 
               | Some node_def ->
                 (match soc_of_node prog node_def soc_tbl with
                  | Some(_,soc,soc_tbl) -> SocUtils.add sk soc soc_tbl
                  | None ->
                    print_string ("Undefined soc : " ^ (string_of_node_key nk) ^ "\n");
                    flush stdout;
                    soc_tbl
                 )
              )
            with
            | Undef_soc (_sk,_lxm,Lic.CALL { it = nk2 ;_}, _types,_) -> 
              (* Il manque une dépendance, on essaie de la
                 traduire puis de retraduire le noeud courant. 
                 ZZZ ca part facilement en vrille ici si une erreur
                 a été faite en amont... 
              *)
              let soc_tbl = snd (process_node nk2 soc_tbl) in
              snd (process_node nk soc_tbl)

            | Undef_soc (sk,lxm,pos_op, types, fby_init_opt) -> (
                let soc =
                  SocPredef.soc_interface_of_pos_op lxm pos_op types fby_init_opt
                in
                if (sk<>soc.key) then (
                  print_string ("Soc key mismatch :\n\t" ^
                                (SocUtils.string_of_soc_key sk) ^ "\n<>\n\t" ^
                                (SocUtils.string_of_soc_key soc.key) ^ "\n");
                  flush stdout;
                  assert false
                );
                let soc_tbl = SocUtils.add soc.key soc soc_tbl in
                snd (process_node nk soc_tbl)
              )
            | Polymorphic -> 
              let msg = (Lxm.details node.lxm) ^ 
                        ": cannot infer the type of this polymorphic node."^
                        " Please be more specific.\n"
              in
              raise (Lv6errors.Global_error msg)

        in
        sk, soc_tbl

    and make_condact_soc node condact_node soc_key soc_tbl ctx lxm vel =
      let nsk, soc_tbl = process_node condact_node soc_tbl in
      let nsoc = SocUtils.find lxm nsk soc_tbl in
      let nsoc_step = match nsoc.step with [s] -> s 
                                         | _ -> assert false (* hmm. Iterating on a pre will not work. XXX fixme ! *)
      in
      let _ctx,inst = 
        match make_instance lxm Lic.BaseLic ctx nsoc with
        | ctx,Some inst -> ctx,[inst]
        | ctx,None -> ctx,[]
      in
      let soc_key = 
        let x,y,_=soc_key in
        x,y, Soc.MemInit(Soc.Const("_true", Data.Bool)) (* the first step flag *)
      in
      let soc = {
        Soc.key       = soc_key ;
        Soc.profile   = soc_profile_of_node node;
        Soc.clock_profile = [];
        Soc.instances = inst ;
        Soc.step      = [
          {
            name     = "step";
            lxm      = lxm;
            idx_ins  = nsoc_step.idx_ins@[List.length nsoc_step.idx_ins];
            idx_outs = nsoc_step.idx_outs;
            impl     = Condact(nsk, List.flatten (List.map lic2soc_const vel));
          }
        ];
        Soc.memory      = Soc.Mem Data.Bool; (* to hold the first step flag *)
        Soc.precedences = [];
        Soc.assertions  = [ (* something to do? *)];
      } 
      in 
      soc_tbl, soc

    (* Produit des soc de noeuds. *)
    and (soc_of_node: LicPrg.t -> Lic.node_exp -> Soc.tbl ->
         (ctx * Soc.t * Soc.tbl) option) =
      fun licprg node soc_tbl ->
        profile_info ("Lic2soc.soc_of_node "^
                      (Lic.string_of_node_key node.node_key_eff)^"\n");
        let io_list = node.Lic.inlist_eff @ node.Lic.outlist_eff in 
        let io_type = List.map (fun vi -> lic_to_data_type vi.var_type_eff) io_list in
        let soc_key = make_soc_key_of_node_key node.Lic.node_key_eff None io_type in
        let lxm = node.Lic.lxm in
        let ctx = create_context licprg in
        let (soc_of_body: Lic.node_body -> Soc.tbl -> (ctx * Soc.t * Soc.tbl) option) =
          fun b soc_tbl ->
            profile_info "   Lic2soc.soc_of_node: computing actions...\n";
            let ctx, actions, instances, deps =
              (* on itere sur la liste des équations *)
              List.fold_left
                (fun (c, a, i, d) eq ->
                   let nc, na, ni, nd = actions_of_equation eq.src soc_tbl c eq.it in
                   nc, List.rev_append na a, List.rev_append ni i,
                   (ActionsDeps.concat nd d)
                )
                (ctx, [], [], ActionsDeps.empty)
                b.eqs_eff
            in
            (* Construction des dépendances entre les expressions *)
            profile_info "   Lic2soc.soc_of_node: computing dependencies...\n";
            let all_deps =
              ActionsDeps.build_data_deps_from_actions lic_to_data_type deps actions
            in
            Lv6Verbose.exe ~flag:dbg
              (fun () -> print_string (ActionsDeps.to_string all_deps); flush stdout);
            profile_info "   SortActions.f: sorting actions...\n";
            let gaol = SortActions.f actions all_deps lxm in
            profile_info "   Lic2soc.soc_of_node: actions sorted. \n";
            let (locals: Soc.var list) =
              match node.Lic.loclist_eff with
              | None -> []
              | Some l ->  List.map (lic_to_soc_var) l 
            in
            let step = build_step lxm "step" node (locals @ ctx.locals) gaol in
            let soc = {
              Soc.key         = soc_key;
              Soc.profile     = soc_profile_of_node node;
              Soc.clock_profile = [];
              Soc.instances   = instances ;
              Soc.step        = [step];
              Soc.memory      = Soc.No_mem;
              Soc.precedences = [];
              Soc.assertions  =
                if Lv6MainArgs.global_opt.Lv6MainArgs.gen_autotest then
                  [] (* In this mode no code is generated and the var creation
                        is inhibited in L2Lsplit *)
                else
                  List.map (lic_val_exp_to_soc_var licprg) b.asserts_eff;
            } 
            in
            Some(ctx, soc, soc_tbl)
        in
        let (soc_of_metaop: Lic.node_key -> Soc.tbl -> (ctx * Soc.t * Soc.tbl) option) =
          fun nk soc_tbl ->
            profile_info "Lic2soc.soc_of_metaop...\n";
            match snd (fst nk), List.sort compare (snd nk) with
            | ("map"|"red"|"fill"|"fillred"|"fold"),[
                ConstStaticArgLic(_,Int_const_eff(c)); NodeStaticArgLic(_,iter_node)] 
            | ("map"|"red"|"fill"|"fillred"|"fold"),[
                ConstStaticArgLic(_,Int_const_eff(c)); TypeStaticArgLic(_);
                NodeStaticArgLic(_,iter_node)] -> ( (*red, fill, fillred, map *)
                let nsk, soc_tbl = process_node iter_node soc_tbl in
                let nsoc = SocUtils.find lxm nsk soc_tbl in
                let nsoc_step = match nsoc.step with [s] -> s
                                                   | _ -> assert false (* Iterating on a pre will not work. XXX fixme ! *)
                in
                let rec make_n_instance ctx acc n =
                  if n=0 then ctx, List.rev acc else
                    match make_instance lxm Lic.BaseLic ctx nsoc with
                    | ctx,Some inst -> make_n_instance ctx (inst::acc) (n-1)
                    | ctx,None -> ctx,[]
                in
                let c = int_of_string c in
                let ctx, instances = make_n_instance ctx [] c in
                let soc = {
                  Soc.key       = soc_key;
                  Soc.profile   = soc_profile_of_node node;
                  Soc.clock_profile = [];
                  Soc.instances = instances ;
                  Soc.step      = [
                    {
                      name     = "step";
                      lxm      = lxm;
                      idx_ins  = nsoc_step.idx_ins;
                      idx_outs = nsoc_step.idx_outs;
                      impl     = Iterator(snd (fst nk), nsk, c);
                    }
                  ];
                  Soc.memory    = Soc.No_mem;
                  Soc.precedences = [];
                  Soc.assertions  = [];
                } 

                in
                Some(ctx, soc, soc_tbl)
              )
            | ("condact"), [
                ConstStaticArgLic("dflt",Tuple_const_eff vel);
                NodeStaticArgLic("oper",condact_node)
              ] -> (
                let soc_tbl,soc =
                  make_condact_soc node condact_node soc_key soc_tbl ctx lxm vel
                in
                Some(ctx, soc, soc_tbl)
              )
            | ("condact"), [
                ConstStaticArgLic("dflt",const); NodeStaticArgLic ("oper",condact_node)
              ] -> (
                let soc_tbl,soc =
                  make_condact_soc node condact_node soc_key soc_tbl ctx lxm [const] in
                Some(ctx, soc, soc_tbl)
              )
            | _e -> 
              match (nk) with 
              | ("Lustre","boolred"), [ConstStaticArgLic(_,Int_const_eff(i)); 
                                       ConstStaticArgLic(_,Int_const_eff(j)); 
                                       ConstStaticArgLic(_,Int_const_eff(k)) ] -> (
                  let i,j,k = int_of_string i, int_of_string j, int_of_string k in
                  let soc = {
                    Soc.key       = soc_key;
                    Soc.profile   = soc_profile_of_node node;
                    Soc.clock_profile = [];
                    Soc.instances = [] ;
                    Soc.step      = [
                      {
                        name     = "step";
                        lxm      = lxm;
                        idx_ins  = [0];
                        idx_outs = [0];
                        impl     = Boolred(i,j,k);
                      }
                    ];
                    Soc.memory      = Soc.No_mem;
                    Soc.precedences = [];
                    Soc.assertions  = [];
                  } 
                  in
                  Some(ctx, soc, soc_tbl)
                )
              | _ -> assert false
        in
        let (soc_of_extern: Lic.node_exp -> Soc.tbl -> (ctx * Soc.t * Soc.tbl) option) =
          fun node soc_tbl ->
            try 
              let soc = SocPredef.of_soc_key lxm soc_key in
              Some(ctx, soc, soc_tbl)
            with _ -> 
              (* This extern node is not a predef *)
              let step = build_extern_step lxm "step" node in
              let soc = {
                Soc.key         = soc_key;
                Soc.profile     = soc_profile_of_node node;
                Soc.clock_profile = [];
                Soc.instances   = [] ;
                Soc.step        = [step];
                Soc.memory      =
                  if node.Lic.has_mem_eff then Soc.Mem_hidden else Soc.No_mem; 
                Soc.precedences = [];
                Soc.assertions  = [];
              } 
              in
              Some(ctx, soc, soc_tbl)
        in
        match node.Lic.def_eff with
        | AbstractLic None -> assert false (* None if extern in the provide part *)
        | AbstractLic (Some node_exp) -> soc_of_node licprg node_exp soc_tbl
        | MetaOpLic                   -> soc_of_metaop node.Lic.node_key_eff soc_tbl
        | BodyLic b                   -> soc_of_body b soc_tbl
        | ExternLic                   -> soc_of_extern node soc_tbl
    in
    process_node mnk SocMap.empty
