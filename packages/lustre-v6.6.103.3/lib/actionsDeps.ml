(** Time-stamp: <modified the 06/03/2020 (at 13:32) by Erwan Jahier> *)
  
let dbg = (Lv6Verbose.get_flag "deps")

type action = Action.t

(*********************************************************************************)
module OrderedAction = struct
  type t = action
  let compare = compare
end

(** Gère un ensemble d'actions uniques. *)
module Actions = Set.Make(OrderedAction)

module MapAction = Map.Make(OrderedAction)

(** maps an action to the set of actions that it depends on *)
(* exported *)
type t = Actions.t MapAction.t

(* exported *)
let empty: t = MapAction.empty

(* exported *)
let (have_deps : t -> action -> bool) = 
  fun m a -> 
    MapAction.mem a m
  
(* exported *)
let (remove_dep :  t -> action -> t) =
  fun deps a -> 
    MapAction.remove a deps

(* exported *)
let (find_deps: t -> action -> action list) = 
  fun m a ->
    try Actions.elements (MapAction.find a m) with Not_found -> []


let rec (depends_on : t -> Action.t -> Action.t -> bool) =
  fun m a1 a2 ->
    try
      let a1_deps = MapAction.find a1 m in
      Actions.mem a2 a1_deps || 
        (* XXX should I compute the closure of the deps once for all ? *)
        Actions.exists (fun a1 -> depends_on m a1 a2) a1_deps
    with 
        Not_found -> false
    
(*********************************************************************************)
(** Ajoute une liste de dépendances à une action. *)
let add_deps: t -> action -> action list -> t =
  fun m a -> function
          | [] -> m
          | al ->
             let actions = try MapAction.find a m with Not_found -> Actions.empty in
             let actions = List.fold_left (fun set a -> Actions.add a set) actions al in
             MapAction.add a actions m
          
(* exported *)
let (concat: t -> t -> t) =
  fun m1 m2 ->
    MapAction.fold (fun key value m -> add_deps m key (Actions.elements value)) m1 m2
   
(*********************************************************************************)
(* exported *)
let (generate_deps_from_step_policy:
       Soc.precedence list -> (string * action) list -> t) =
  fun precedences actions ->
    let generate_deps_for_action: (t -> string * string list -> t) =
      fun ad (action_name, actions_needed) ->
        let main_action = snd (List.find (fun (n, _) -> n = action_name) actions) in
        let deps =
          List.map
            (fun dep_name -> snd (List.find (fun (n, _) -> n = dep_name) actions))
            actions_needed
        in
          add_deps ad main_action deps
    in
      List.fold_left (generate_deps_for_action) empty precedences


(*********************************************************************************)
module OrderedSocVar = struct
  type t = Soc.var_expr
  let compare = compare
end
module VarMap = Map.Make(OrderedSocVar)

(** A Data structure that maps a Soc.var_expr to all the
    actions that needed to compute it. 

    It is used to know which actions impact which Soc.var_expr.

nb : you can have several actions associated to the same var_expr
when defining arrays or structures parts by parts. For instance
  x[0]=42;
  x[1]=1;
 are two actions that define the var_expr "x"

*)
type var2actions_tbl = Actions.t VarMap.t

let var2actions k tbl = try VarMap.find k tbl with Not_found -> Actions.empty

let rec (gen_parents : Soc.var_expr  -> Soc.var_expr list) =
  fun var -> 
(* if var = t.[2].field, then it returns [t.[2].field; t.[2] ; t]  *)
    match var with
      | Soc.Slice(ve,_,_,_,_,_)
      | Soc.Field(ve,_,_)  
      | Soc.Index(ve,_,_) -> ve::(gen_parents ve)
      | Soc.Var(_,_vt)
      | Soc.Const(_,_vt) -> [var]

let rec (_get_top_var : Soc.var_expr  -> Soc.var_expr) =
  fun var -> 
(* if var = t.[2].field, then it returns (also) t.[2] and t  *)
    match var with
      | Soc.Slice(ve,_,_,_,_,_)
      | Soc.Field(ve,_,_)  
      | Soc.Index(ve,_,_) -> _get_top_var ve
      | Soc.Var(_,_vt)
      | Soc.Const(_,_vt) -> var 

(** If x is a int^2, then  
     then actions such as  a="x = y" 
  should produce the following dependancies :
     x -> a
     x[0] -> a
     x[1] -> a

  Hence, gen_children "x" produces "x[0]", and "x[1]"
 *) 
let rec (gen_children: Soc.var_expr -> Soc.var_expr list) =
  fun v ->
  match Soc.data_type_of_var_expr v with
  | Data.Alpha _ | Data.Extern _ | Data.Enum _ | Data.String
  | Data.Bool | Data.Int | Data.Real
    -> [v]
  | Data.Struct(_ident, ident_t_list) ->
     List.fold_left
       (fun acc (id,t) ->
        let new_ve = Soc.Field(v,id,t) in
        new_ve::((gen_children new_ve) @ acc)
       )
       []
       ident_t_list
  | Data.Array(t,size) ->
     let new_ve_list = ref [] in
     for i=0 to size - 1 do
       let new_ve = Soc.Index(v, i, t) in
       new_ve_list := new_ve::((gen_children new_ve) @ !new_ve_list);
     done;
     !new_ve_list
  | Data.Alias(_,_t) ->  assert false (* sno ? *)

let nodupl l =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) [] l 

                               
let (get_var2actions_tbl : action list -> var2actions_tbl) = 
  fun al ->
    let (tabulate_action : var2actions_tbl -> action -> var2actions_tbl) =
      fun tbl action ->
        let _, _, lhs, _, _lxm = action in
        let (tabulate_output:var2actions_tbl -> Soc.var_expr -> var2actions_tbl) =
          fun tbl output ->
          let v = (* get_top_var *) output in (* for x of type t^2^2  *)
          let children = gen_children v in (*    children(x[0]) = [x[0][0];x[0][1]] *)
          let parents = gen_parents v in   (* and parents(x[0]) = [x] *)
          let all = nodupl ((v::children)@parents) in
          let tbl =
            (* add the current action as a dep of v and its children and its parents *)
            List.fold_left
              (fun tbl cv ->
               let cv_actions = var2actions cv tbl in
               VarMap.add cv (Actions.add action cv_actions) tbl)
              tbl all
          in
          tbl
        in
          List.fold_left tabulate_output tbl lhs
    in
      List.fold_left tabulate_action VarMap.empty al


(** Returns the actions that depend on a set of vars, according to the content
   of a table compute before
    
    [actions_of_vars input_vars al] trouve toutes les actions de [al] qui
    ont besoin d'être effectuées avant de pouvoir se servir de [input_vars]
    comme entrée d'une autre action.

    TODO: gérer les dépendances entre des filtres plus complexes,
    comme par ex., l'utilisation d'un champ d'une structure.
*)
let (_actions_of_vars_old: Soc.var_expr list -> var2actions_tbl -> action list) =
  fun vars tbl ->
    let find_deps var = Actions.elements (var2actions var tbl) in
    (*     let vars = List.flatten (List.map gen_parents vars) in   *)
    (*     let vars = List.fold_left (* remove duplicates *) *)
    (*                  (fun acc x -> if List.mem x acc then acc else x::acc) [] vars *)
    (*     in *)
    List.flatten (List.map find_deps vars)

let (actions_of_vars: Soc.var_expr list -> var2actions_tbl -> action list) =
  fun vars tbl ->
  let actions = 
    List.fold_left 
      (fun acc v -> Actions.union acc (var2actions v tbl))
      Actions.empty
      vars
  in
  Actions.elements actions
     
(*********************************************************************************)
(* Some Printers to ease the debugging *)

let string_of_actions: Actions.t -> string = fun s ->
  let to_string a acc =
    acc ^ "\n\t + '"^ (Action.to_string a) ^ "'"
  in
    "" ^ (Actions.fold to_string s "") ^ ""

let string_of_var2actions_tbl: var2actions_tbl -> string = 
  fun s ->
    let to_string key value acc =
      let entry = Format.sprintf "%s depends on the following actions: %s"
                                 (SocUtils.string_of_filter key) 
                                 (string_of_actions value)
      in
        acc ^ entry ^ "\n"
    in
      "var2actions_tbl: {\n" ^ (VarMap.fold to_string s "") ^ "}"

let to_string: t -> string = fun m ->
  let to_string key value acc =
    let entry =
      Format.sprintf "- '%s' depends on:%s"
        (Action.to_string key)
        (string_of_actions value)
    in
      acc ^ entry ^ "\n"
  in
    "dependencies between equations are: \n" ^ (MapAction.fold to_string m "") ^ ""

(*
let (add_parents : var2actions_tbl -> var2actions_tbl) =
  fun tbl ->
  let f var actions acc =
    let pvars = gen_parents var in
    List.folf_left
      (fun acc pvar ->
       let pactions = try var2actions pvar acc with Not_found -> Actions.empty in

      )
      acc pvars
  in
  VarMap.fold f tbl tbl
 *)                                                                                 
(* It's useless to close this ; toposort will do it

let rec close : t -> t =
  fun deps ->
  let f action actions acc =
      Actions.fold
        (fun a acc ->
         let a_actions = MapAction.find a acc in
         let new_actions = Actions.union actions a_actions in
         MapAction.add action new_actions acc
        )
        actions acc    
  in
  let new_deps = MapAction.fold f deps deps in
  if deps = new_deps (* use MapAction.equal ? *)
  then deps else close new_deps
 *)                                        
(*********************************************************************************)
(* exported *)
let build_data_deps_from_actions:  (Lic.type_ -> Data.t) -> t -> action list -> t =
  fun lic_to_data_type deps al ->
  let tbl = get_var2actions_tbl al in
  (*   let tbl = add_parents tbl in *)
  let pp_dbg () = 
    let al_str = List.map Action.to_string al in
    print_string "\n ====> List of actions to be sorted:\n";
    print_string (String.concat "\n   " al_str);
    print_string "\n ====> List of computed dependencies:\n";
    print_string (string_of_var2actions_tbl tbl);
    flush stdout
  in
    let deps =
    Lv6Verbose.exe ~flag:dbg pp_dbg;
    List.fold_left
      (fun acc_deps action ->
       let (clk, rhs, _, _,_) = action in
       let dep_vars = match clk with
         | Lic.BaseLic -> rhs
         | Lic.ClockVar _int -> rhs
         | Lic.On ((_cc,cv,ct),_) ->
            (* The guard should be computed before the guarded expression *)
            (Soc.Var(cv, lic_to_data_type ct))::rhs
       in
       let deps = actions_of_vars dep_vars tbl in
       if deps = [] then (
         let rhs_str = String.concat "," (List.map SocUtils.string_of_filter rhs) in
         Lv6Verbose.exe
           ~flag:dbg (fun () -> print_string ("\n====> No deps for " ^ rhs_str));
         acc_deps
       )
       else
         add_deps acc_deps action deps
      )
      deps
      al
  in
  (*     let deps = close deps in *)
  deps

   
