open Smtlib_error
module SMap = Map.Make(String)

let init len f =
  let rec init_aux i n f =
    if i >= n then []
    else
      let r = f i in
      r :: init_aux (i+1) n f
  in init_aux 0 len f

type assoc =
  | Right
  | Left
  | Chainable
  | Pairwise

type fun_def = {
  params : Smtlib_ty.ty;
  assoc : assoc option;
}

type env = {
  sorts : ((int * int) *
           (string ->
            (Smtlib_ty.ty list * int list) -> Smtlib_ty.desc)) SMap.t;
  funs : fun_def list SMap.t;
  par_funs : (string list -> fun_def) list SMap.t;
  constructors : int SMap.t SMap.t;
}

let empty () = {
  sorts = SMap.empty;
  funs = SMap.empty;
  par_funs = SMap.empty;
  constructors = SMap.empty;
}

open Smtlib_syntax
(******************************************************************************)
(*********************************** Utils ************************************)

let get_arit symb arit =
  try int_of_string arit
  with  _ ->
    error (Typing_error "This expression is not an int") symb.Smtlib_syntax.p

let get_index i =
  match i.c with
  | IndexSymbol(symb) -> symb.c
  | IndexNumeral(s) -> s

let get_identifier id =
  match id.c with
  | IdSymbol(symb) -> symb, []
  | IdUnderscoreSymNum(symb,index_list) ->
    symb, List.map get_index index_list

let check_identifier id arit =
  match id.c with
  | IdSymbol(_symb) -> assert (arit = 0);
  | IdUnderscoreSymNum(_symb,index_list) ->
    assert (List.length index_list = arit)

(******************************************************************************)
(*********************************** Sorts ************************************)
let check_sort_already_exist (env,locals) symb =
  if SMap.mem symb.c locals then
    error (Sort_declaration_error
             ("sort " ^ symb.c ^ " already declared/defined")) symb.p
  else if SMap.mem symb.c env.sorts then
    error (Sort_declaration_error
             ("sort " ^ symb.c ^ " already declared/defined")) symb.p

let check_sort_exist (env,locals) symb =
  if not (SMap.mem symb.c locals) then
    if not (SMap.mem symb.c env.sorts) then
      error (Sort_declaration_error
               ("sort " ^ symb.c ^ " undeclared/undefined")) symb.p

let mk_sort_definition arit_s arit_t is_dt =
  if is_dt then
    ((arit_s,arit_t),(fun s (l,_) ->
         assert (List.length l = arit_s); Smtlib_ty.TDatatype(s,l)))
  else
    ((arit_s,arit_t),(fun s (l,_) ->
         assert (List.length l =  arit_s); Smtlib_ty.TSort(s,l)))

let mk_sort (env,locals) symb sort_def =
  check_sort_already_exist (env,locals) symb;
  {env with sorts = SMap.add symb.c sort_def env.sorts}

let mk_sort_decl (env,locals) symb arit is_dt =
  let arit = get_arit symb arit in
  let sort_def = mk_sort_definition arit 0 is_dt in
  mk_sort (env,locals) symb sort_def

let find_sort_def env symb =
  try SMap.find symb.c env.sorts
  with Not_found ->
    error (Sort_declaration_error ("Undefined sort " ^ symb.c)) symb.p

let add_sorts env sorts =
  List.fold_left (fun env (name,sort) ->
      {env with sorts = SMap.add name sort env.sorts}
    ) env sorts

let rec find_sort_symb (env,locals) symb pars_s pars_t=
  try SMap.find symb.c locals
  with Not_found ->
    let (arit_s,arit_t),fun_sort = find_sort_def env symb in
    assert (List.length pars_s = arit_s);
    assert (List.length pars_t = arit_t);
    Smtlib_ty.new_type (fun_sort symb.c (pars_s,pars_t))

and find_sort (env,locals) sort =
  match sort.c with
  | SortIdentifier(id) -> begin
      match id.c with
      | IdSymbol(symb) ->
        let s_ty = find_sort_symb (env,locals) symb [] [] in
        Smtlib_ty.unify sort.ty s_ty sort.p;
        s_ty
      | IdUnderscoreSymNum(s,l) ->
        let l = List.map (fun i ->
            try int_of_string (get_index i)
            with Not_found ->
              error (Sort_declaration_error
                       (Printf.sprintf "Args of %s should be integers" s.c)
                    ) id.p
          ) l
        in
        let s_ty = find_sort_symb (env,locals) s [] l in
        Smtlib_ty.unify sort.ty s_ty sort.p;
        s_ty
    end
  | SortIdMulti (id, sort_list) ->
    let symb,_ = get_identifier id in
    let arg_sort = List.map (fun s ->
        let s_ty = find_sort (env,locals) s in
        s_ty
      ) sort_list in
    let s_ty = find_sort_symb (env,locals) symb arg_sort [] in
    Smtlib_ty.unify sort.ty s_ty sort.p;
    s_ty

(******************************************************************************)
(************************************ Funs ************************************)
let extract_arit_ty_assoc ty =
  match ty.Smtlib_ty.desc with
  | Smtlib_ty.TFun(params,_) ->
    List.length params,
    (try List.hd params with _ -> Smtlib_ty.new_type (Smtlib_ty.TDummy))
  | _ -> assert false

let rec compare_fun_assoc (_env,locals) symb ty _f assoc =
  let arit,t_fun = extract_arit_ty_assoc ty in
  let _,t_fun = Smtlib_ty.inst locals Smtlib_ty.IMap.empty t_fun in
  let params = init arit (fun _i -> t_fun) in
  let ret =
    match assoc with
    | Right | Left -> t_fun
    | Chainable | Pairwise -> Smtlib_ty.new_type (Smtlib_ty.TBool)
  in
  let def = Smtlib_ty.new_type (Smtlib_ty.TFun (params,ret)) in
  let _,def = Smtlib_ty.inst SMap.empty Smtlib_ty.IMap.empty def in
  Smtlib_ty.unify ty def symb.p;
  Some (Smtlib_ty.fun_ret ty)

and compare_fun_def (env,locals) symb ty funs all_type =
  let rec aux funs =
    match funs with
    | [] -> None
    | def :: funs ->
      try
        match def.assoc with
        | None ->
          let def = def.params in
          let _,def = Smtlib_ty.inst locals Smtlib_ty.IMap.empty def in
          Smtlib_ty.unify ty def symb.p;
          if all_type then
            Some ty
          else
            Some (Smtlib_ty.fun_ret ty)
        | Some a -> compare_fun_assoc (env,locals) symb ty def.params a
      with
      | _ -> aux funs
  in
  aux funs

let find_fun (env,locals) symb params args all_type=
  let defs =
    if args == [] then
      SMap.find symb.c env.funs
    else
      List.map (fun def -> def args) (SMap.find symb.c env.par_funs)
  in
  let ty = Smtlib_ty.new_type
      (Smtlib_ty.TFun (params,Smtlib_ty.new_type Smtlib_ty.TDummy)) in
  (* Printf.eprintf "Find fun : %s : %s\n%!" symb.c (Smtlib_ty.to_string ty); *)
  let res = compare_fun_def (env,locals) symb ty defs all_type in
  match res with
  | Some def -> def
  | None ->
    error (Typing_error (Printf.sprintf "Undefined fun definition %s : %s"
                           symb.c (Smtlib_ty.to_string ty))) symb.p

let check_fun_exists (env,locals) symb params all_type =
  try
    let ty = Smtlib_ty.new_type
        (Smtlib_ty.TFun (params,Smtlib_ty.new_type Smtlib_ty.TDummy)) in
    let defs = SMap.find symb.c env.funs in
    let res = compare_fun_def (env,locals) symb ty defs all_type in
    match res with
    | Some _ ->
      error (Fun_declaration_error
               ("Function already declared/defined : " ^ symb.c)) symb.p
    | None -> ()
  with Not_found -> ()

let mk_fun_ty pars ret assoc =
  let ty = Smtlib_ty.new_type (Smtlib_ty.TFun(pars,ret)) in
  {params= ty; assoc = assoc}

let mk_fun_ty_arg pars ret assoc _args =
  let ty = Smtlib_ty.new_type (Smtlib_ty.TFun(pars,ret)) in
  (fun _args -> {params= ty; assoc = assoc})

let add_fun_def (env,locals) name params return assoc =
  check_fun_exists (env,locals) name params false;
  let funs =
    try SMap.find name.c env.funs
    with Not_found -> []
  in
  {env with funs = SMap.add name.c
                ((mk_fun_ty params return assoc) :: funs) env.funs}

let mk_fun_dec (env,locals) (name,pars,return) =
  let pars = List.map (fun par ->
      let s = find_sort (env,locals) par in
      Smtlib_ty.unify par.ty s par.p;
      s
    ) pars in
  let s_return = find_sort (env,locals) return in
  Smtlib_ty.unify return.ty s_return return.p;
  add_fun_def (env,locals) name pars s_return


let add_funs env funs =
  List.fold_left (fun env (name,fun_def) ->
      let funs =
        try SMap.find name env.funs
        with Not_found -> []
      in
      {env with funs = SMap.add name
                    (fun_def :: funs) env.funs}
    ) env funs

let add_par_funs env funs =
  List.fold_left (fun env (name,fun_def) ->
      let funs =
        try SMap.find name env.par_funs
        with Not_found -> []
      in
      {env with par_funs = SMap.add name
                    (fun_def :: funs) env.par_funs}
    ) env funs

let find_simpl_sort_symb (env,_locals) symb params =
  let (ar_s,_ar_t),fun_sort = find_sort_def env symb in
  assert (ar_s = (List.length params));
  Smtlib_ty.new_type (fun_sort symb.c (params,[]))

(******************************************************************************)
(*********************************** Datatypes ********************************)
let extract_pars locals pars =
  let pars = List.fold_left (fun pars par ->
      let symb = par.c in
      if SMap.mem symb pars then
        error (Typing_error ("Type variable already declared : " ^ symb)) par.p;
      let ty = Smtlib_ty.new_type (Smtlib_ty.TVar(symb)) in
      Smtlib_ty.unify par.ty ty par.p;
      SMap.add symb ty pars
    ) SMap.empty pars; in
  SMap.union (fun _k _v1 v2 -> Some v2) locals pars

let mk_const (env,locals) (name,pars,sort) =
  let locals = extract_pars locals pars in
  mk_fun_dec (env,locals) (name,[],sort) None

let mk_fun_def (env,locals) (name,params,return) =
  mk_fun_dec (env,locals) (name,params,return) None

let mk_fun_dec (env,locals) (name,(pars,params,return)) =
  let locals = extract_pars locals pars in
  mk_fun_dec (env,locals) (name,params,return) None

let find_sort_name sort =
  match sort.c with
  | SortIdentifier id -> get_identifier id
  | SortIdMulti (id, _) -> get_identifier id

let mk_sort_def (env,locals) symb pars sort =
  let locals_old = locals in
  let locals = extract_pars locals pars in
  let pars = List.map (fun par -> SMap.find par.c locals) pars in
  let sort =  find_sort (env,locals) sort in
  let arit = List.length pars in
  let sort_def = (arit,0), (fun _s (l,_) ->
      assert (List.length l = arit);
      let links = List.fold_left2 (fun links t1 t2 ->
          let links, t2 = Smtlib_ty.inst locals_old links t2 in
          Smtlib_ty.unify t1 t2 symb.p;
          links
        ) (Smtlib_ty.IMap.empty) l pars
      in
      let sort = Smtlib_ty.subst links sort in
      sort.Smtlib_ty.desc
    )
  in
  {env with sorts = SMap.add symb.c sort_def env.sorts}

(******************************************************************************)
(*********************************** Datatypes ********************************)
let find_constr env symb =
  try
    let cstrs = SMap.find symb.c env.funs in
    if (List.length cstrs > 1) then
      error (Typing_error
               ("Constructor have mutliple signatures : " ^ symb.c)) symb.p;
    (try (List.hd cstrs).params with _e ->
       error (Typing_error ("Undefined Constructor : " ^ symb.c)) symb.p;)
  with Not_found ->
    error (Typing_error ("Undefined Constructor : " ^ symb.c)) symb.p

let mk_constr_decs (env,locals) dt dt_sort constr_decs =
  let cstrs = ref [] in
  let env = List.fold_left (fun env (symb_cstr,selector_dec_list) ->
      let env,destr_list =
        (* Add all destructors *)
        List.fold_left (fun (env,destr_list) (symb_destr,sort_destr) ->
            let return = find_sort (env,locals) sort_destr in
            let env, l =
              add_fun_def (env,locals) symb_destr [dt_sort] return None,
              return :: destr_list in
            env, l
          ) (env,[]) selector_dec_list in
      let env =
        add_fun_def (env,locals) symb_cstr (List.rev destr_list) dt_sort None in

      cstrs := (symb_cstr.c, List.length destr_list) :: !cstrs;

      (* tester of constructor *)
      let is_cstr = { symb_cstr with c = Printf.sprintf "is %s" symb_cstr.c } in
      let env = add_fun_def (env,locals) is_cstr
          (* [(Smtlib_ty.new_type (Smtlib_ty.TDatatype("",[])))] *)
          [dt_sort]
          (Smtlib_ty.new_type Smtlib_ty.TBool) None in
      env
    ) env constr_decs
  in
  let cstrs = List.fold_left (fun acc (cst,arrit) ->
      SMap.add cst arrit acc) SMap.empty !cstrs in
  if not (SMap.is_empty cstrs) then
    {env with constructors = SMap.add dt.c cstrs env.constructors}
  else
    env

let mk_dt_dec (env,locals) dt (pars,cst_dec_list) =
  let locals = ref locals in
  let dt_pars =
    List.map (fun s ->
        let ty = Smtlib_ty.new_type (Smtlib_ty.TVar s.c) in
        locals := SMap.add s.c ty !locals;
        ty
      ) pars
  in
  let dt_sort = find_simpl_sort_symb (env,locals) dt dt_pars in
  mk_constr_decs (env,!locals) dt dt_sort cst_dec_list



let mk_datatype (env,locals) dt pars dt_dec =
  let arit = List.length pars in
  let sort_def = mk_sort_definition arit 0 true in
  let env = mk_sort (env,locals) dt sort_def in
  mk_dt_dec (env,locals) dt (pars,dt_dec)

let mk_datatypes (env,locals) sort_decs datatype_decs =
  let env = List.fold_left (fun env (symb,arit) ->
      mk_sort_decl (env,locals) symb arit true
    ) env sort_decs in
  let env = List.fold_left2 (fun env (symb, _arit) dt_dec ->
      mk_dt_dec (env,locals) symb dt_dec
    ) env sort_decs datatype_decs in
  env
