(** Time-stamp: <modified the 29/08/2019 (at 16:49) by Erwan Jahier> *)
 
open Lic
open IdSolver
open Printf
open Lxm
open Lv6errors

(***
MESSAGES D'ERREUR :
Pour rester homogène, utiliser les fonctions de LicEvalType:
  raise_type_error (provided: string) (expect: string) (msg: string)
  raise_arity_error (msg:string) (provided:int) (expect:int)
*)
let raise_type_error = LicEvalType.raise_type_error
let raise_arity_error = LicEvalType.raise_arity_error
exception EvalType_error = LicEvalType.EvalType_error

let dbg = (Lv6Verbose.get_flag "poly")

(******************************************************************************)
let finish_me msg = print_string ("\n\tXXX evalType.ml:"^msg^" ->  finish me!\n")

let rec (f : IdSolver.t -> Lic.val_exp -> Lic.val_exp * Lic.type_ list) =
  fun id_solver ve ->
    let ve_core, tl =
      match ve.ve_core with
        | CallByPosLic ({it=posop; src=lxm}, args) -> (
          let posop_opt, args, tl = 
            try eval_by_pos_type id_solver posop lxm args
            with EvalType_error msg -> 
              raise (Compile_error(lxm, "type error: "^msg))
          in
          let ve = 
            match posop_opt with
              | None -> CallByPosLic ({it=posop; src=lxm}, args)
              | Some posop -> CallByPosLic ({it=posop; src=lxm}, args)
          in
          ve, tl
        )
        | CallByNameLic ({it=nmop; src=lxm}, nmargs ) -> (
          let nmargs, tl = 
            try eval_by_name_type id_solver nmop lxm nmargs
            with LicEvalConst.EvalConst_error msg ->
              raise (Compile_error(lxm, "\n*** can't eval constant: "^msg))
          in
          CallByNameLic ({it=nmop; src=lxm}, nmargs ), tl
        )
        | Merge (mclk, nmargs ) -> (
          let lxm = match mclk with {ve_core=CallByPosLic({src=lxm;_},[]);_} -> lxm
            | _  -> assert false
          in
          try eval_merge id_solver mclk lxm nmargs
          with LicEvalConst.EvalConst_error msg ->
            raise (Compile_error(lxm, "\n*** can't eval constant: "^msg))
        )
    in
    { ve with ve_core = ve_core; ve_typ = tl }, tl

and eval_by_pos_type
    (id_solver: IdSolver.t)
  (posop: Lic.by_pos_op)
  (lxm: Lxm.t)
  (args: Lic.val_exp list)
  : (
    Lic.by_pos_op option (* For op that hold a val_exp, we return the modified op *)
    * Lic.val_exp list     (* The args with type info added *)
    * Lic.type_ list       (* The type of the val_exp "posop(args)" *)
  ) =
  match posop with
    | Lic.PREDEF_CALL (nkf) -> (
      let op = AstPredef.string_to_op (snd(fst nkf.it)) in
      let args, targs = List.split (List.map (f id_solver) args) in
      (* ICI pas de matches possible ? *)
      let tve = LicEvalType.f id_solver op nkf.src targs in
      None, args, tve
    )
    | Lic.CALL nkf -> 
      let node_key = nkf.it in
      (* let node_exp_eff = id_solver.id2node node_key lxm in *)
      let node_exp_eff = IdSolver.node_exp_of_node_key id_solver node_key lxm in
      let (lti, lto) = Lic.profile_of_node_exp node_exp_eff in
      let args, t_argsl = List.split (List.map (f id_solver) args) in
      let t_args = List.flatten t_argsl in
      let llti = List.length lti and lt_args = List.length t_args in
      let _ = if llti <> lt_args then
          raise_arity_error (fst (fst node_key)) lt_args llti
      in
      (* lti = expecteds, t_args = given *)
      let tmatches = try UnifyType.is_matched lti t_args 
        with UnifyType.Match_failed msg -> (
          let msg' = Printf.sprintf 
            "\n***    while unifing (%s) with (%s)"
            (Lic.string_of_type_list lti)
            (Lic.string_of_type_list t_args)
          in raise (EvalType_error(msg'^msg))
        ) in
      let tve = match tmatches with
        | [] -> lto
        | _  -> 
          Lv6Verbose.exe ~flag:dbg (fun () ->
            Printf.fprintf stderr "#DBG: EvalType of CALL '%s' (%s)\n"
              (Lic.string_of_node_key node_key) 
              (Lxm.details lxm);
            Printf.fprintf stderr "#     unifying '%s' with '%s'\n"
              (Lic.string_of_type_list lti)
              (Lic.string_of_type_list t_args) ;
            Printf.fprintf stderr "#     required matches %s\n"
              (Lic.string_of_type_matches tmatches) 
          );
          Lic.apply_type_matches tmatches lto
      in
      (None, args, tve)
    | Lic.CONST ceff -> None, [], Lic.types_of_const ceff
    | Lic.CONST_REF idl ->
      let ceff = IdSolver.const_eff_of_item_key id_solver idl lxm in
      let tve = Lic.types_of_const ceff in
      None, [], tve
    | Lic.VAR_REF id ->
      let tve = [
        (IdSolver.var_info_of_ident id_solver id lxm).var_type_eff
      ] in
      None, [], tve
    | Lic.TUPLE ->
      let args, targs = List.split (List.map (f id_solver) args) in
      None, args, List.flatten targs

    | Lic.CONCAT ->
      let args, targs = List.split (List.map (f id_solver) args) in
      let tve = match targs with
        | [ [Array_type_eff (t0, s0)]; [Array_type_eff (t1, s1)]] ->
          if t0 = t1 then [Array_type_eff (t0, s0+s1)]
          else
            raise_type_error (List.flatten targs) []
              "two arrays of the same type was expected"
        | [ _t1; _t2 ] ->
           raise_type_error (List.flatten targs) []
                            "             whereas 2 arrays were expected"
        | _ -> 
          raise_arity_error "concat" (List.length args) 2
      in
      None, args, tve
    | Lic.STRUCT_ACCESS (fid) ->
      assert (List.length args = 1);          
      let arg, targ = f id_solver (List.hd args) in
      let teff_field =
        match targ with
          | [Struct_type_eff (_name, fl)] -> (
            try fst (List.assoc fid fl)
            with Not_found ->
              raise (
                EvalType_error
                  (Printf.sprintf "%s is not a field of struct %s"
                     (Lv6Id.to_string fid)
                     (Lic.string_of_type (List.hd targ))))
          )
          | [x] -> raise_type_error [x] [] "some struct type was expected"
          |  x -> raise_arity_error "struct access" (List.length x) 1 
      in
      None, [arg], [teff_field]
    | Lic.ARRAY_ACCES(i) ->
      assert (List.length args = 1);
      let arg, targ = f id_solver (List.hd args) in
      let sz, teff =
        match targ with
          | [Array_type_eff(teff_elt, size)] -> size, teff_elt
          | _ ->
            let msg = 
              "\n*** Type error: '"^(Lic.string_of_type_list targ) ^
                "' was expected to be an array"
            in
            raise (Compile_error(lxm, msg))
      in
      let _ = if ((i >= 0) && (i < sz)) then () else
          raise(EvalType_error(sprintf "array index %d out of bounds 0..%d" i (sz-1)))
      in
      None, [arg], [teff]

    | Lic.ARRAY_SLICE(sieff) ->
      assert (List.length args = 1);          
      let arg, targ = f id_solver (List.hd args) in
      let sz, teff_elt =
        match targ with
          | [Array_type_eff(teff_elt, size)] -> size, teff_elt
          | _ ->
            raise (Compile_error(
              lxm, "\n*** Type error: '" ^ 
                (Lic.string_of_type_list targ) ^ 
                "' was expected to be an array"))
      in
      let _ = if ((sieff.se_first >= 0) && (sieff.se_first < sz)) then () else
          raise(
            EvalType_error(sprintf "array index %d out of bounds 0..%d" 
                             sieff.se_first (sz-1)))
      in
      let _ = if ((sieff.se_last >= 0) && (sieff.se_last < sz)) then () else
          raise(
            EvalType_error(sprintf "array index %d out of bounds 0..%d" 
                             sieff.se_last (sz-1)))
      in
      None, [arg], [Array_type_eff(teff_elt, sieff.se_width)]

    | Lic.HAT(size) -> 
      let arg, targs = f id_solver (List.hd args) in
      let targ = try List.hd targs with _ -> assert false in
      let teff = Array_type_eff(targ, size) in
      Some(Lic.HAT(size)), [arg], [teff]

    | Lic.ARRAY ->
      (* check that args are of the same type *)
      let args, targs = List.split (List.map (f id_solver) args) in
      let teff_elt_opt =
        List.fold_left
          (fun acc teff ->
            match acc with
            | None -> Some teff
            | Some teff' -> if teff' = teff then acc else
             raise(EvalType_error("all array elements should have the same type"))
          )
          None
          (List.flatten targs)
      in
      let teff_elt:Lic.type_ = match teff_elt_opt with
          None -> TypeVar(Any) | Some x -> x in
      let tve = [Array_type_eff(teff_elt, List.length args)] in
      None, args, tve

    | Lic.WHEN clk_exp -> (
      let args, targs = List.split (List.map (f id_solver) args) in
      let _ = match clk_exp with
        | BaseLic -> ()
        | ClockVar _ -> (assert false)
        | On((_cc,_cv,ct),_clk) -> 
          (match ct with
	         | Lic.Bool_type_eff
            | Lic.Enum_type_eff _ -> ()
	         | _teff -> 
		        let msg = "the type of a clock cannot be " ^
                (Lic.string_of_type ct) 
		        in
		        raise(Compile_error(lxm,msg))
          )
      in
      match targs with
        | [teff] -> None, args, teff
        | _ -> raise_arity_error "when" (List.length targs) 1 
    )
    | Lic.ARROW
    | Lic.FBY -> (
      let args, targs = List.split (List.map (f id_solver) args) in
      match targs with
        | [init; teff] -> if init = teff then None, args, teff else 
            raise(EvalType_error("type mismatch. "))
        | _ -> raise_arity_error "fby" (List.length targs) 2
    )
    | Lic.CURRENT (Some _) -> (
      let args, targs = List.split (List.map (f id_solver) args) in
      match targs with
        | [_;teff] -> None, args, teff
        | _ -> raise_arity_error "current" (List.length targs) 2
    )
    | Lic.CURRENT None
    | Lic.PRE -> (
      let args, targs = List.split (List.map (f id_solver) args) in
      match targs with
        | [teff] -> None, args, teff
        | _ -> raise_arity_error "pre" (List.length targs) 1
    )


(**
   Juste pour les structures ...
*)
and eval_by_name_type (id_solver:  IdSolver.t) (namop: Lic.by_name_op) (lxm: Lxm.t)
    (namargs: (Lv6Id.t Lxm.srcflagged * Lic.val_exp) list )
  (* renvoie la liste de modif de champs compilée + le type du résultat *)
  :  (Lv6Id.t Lxm.srcflagged * Lic.val_exp) list * Lic.type_ list
    = 
  match namop with
    | Lic.STRUCT_anonymous -> 
      (* ??? comment faire ici pour recuperer son type ???
         il faut que je recherche à l'aide des noms de champs
         le type structure qui va bien !

         - creer une table [liste des noms de champs -> ident de type structure] ?
         - rajouter dans la table a sa creation une entree dont le nom
         est composé du nom des champs ?
      *)
      finish_me "anonymous struct not yet supported"; 
      assert false
        
    | Lic.STRUCT (opid) 
    | Lic.STRUCT_with (opid,_) -> 
      let struct_type = id_solver.id2type (Lv6Id.idref_of_long opid) lxm in
      match struct_type with
        | Struct_type_eff(_sn, fl) -> 
          let do_field_assign (fn, fv) =
            (* traitement d'un ''field_name  = field_value'' *)
            let (ft,_fopt) = 
              try List.assoc fn.it fl
              with Not_found -> 
                let msg = "type error: bad field"^(Lv6Id.to_string fn.it) in
                raise (Compile_error(lxm, msg))
            in
            (* let's check the type of fv *)
            let fv, fv_type = f id_solver fv in
            if fv_type = [ft] then (fn,fv)
            else raise_type_error fv_type [ft]
              ("while checking struct field "^(Lxm.details fn.src))
          in
          let namargs = List.map do_field_assign namargs in
          let namargs = 
            List.map 
              (fun (id,_) ->  
                let l = List.filter (fun (idf,_) -> id=idf.it)  namargs in
                match namop, l with
                  | _,[x] -> x
                  | _,_::_ -> assert false
                  | (STRUCT_anonymous, []) -> assert false 
                  | Lic.STRUCT(_),[] -> (
                    try
                      let const = 
                        match snd(List.assoc id fl) with
                          | None -> raise Not_found
                          | Some const -> const
                      in
                      let ve = snd (UnifyClock.const_to_val_eff 
                                      lxm true UnifyClock.empty_subst const)
                      in
                      (flagit id lxm), ve                      
                    with Not_found -> 
                      let msg = Printf.sprintf
                        "Error: the  field '%s' of structure '%s' is undefined"
                        (id) (Lv6Id.string_of_long false opid)
                      in 
                      raise (Compile_error(lxm, msg))
                  )
                  | Lic.STRUCT_with(_,id_with),[] -> 
                    let (type_of_struct_field : Lv6Id.t -> Lic.type_ -> Lic.type_) =
                      fun id t -> 
                        match t with
                          | Struct_type_eff(_l,fl) -> 
                            (try fst(List.assoc id fl) 
                             with Not_found -> 
                               print_string ("field " ^id^" not foudn in ");
                               print_string (Lic.string_of_type t);
                               assert false)
                          | _ -> assert false
                    in
                    let (get_field_of_id : Lv6Id.t -> Lv6Id.t -> Lxm.t -> 
                         Lv6Id.t Lxm.srcflagged * Lic.val_exp) =
                      fun id_with id lxm ->
                        let vi = id_solver.id2var id_with lxm in
                        let dft_ve = 
                          {ve_core = CallByPosLic(flagit (VAR_REF id_with) lxm,[]);
                           ve_typ  = [vi.var_type_eff];
                           ve_clk  = [snd vi.var_clock_eff];
                           ve_src = lxm
                          }
                        in
                        let ve =
                          {ve_core = CallByPosLic ((flagit (STRUCT_ACCESS id) lxm), 
                                                   [dft_ve]);
                           ve_typ  = [type_of_struct_field id vi.var_type_eff];
                           ve_clk  = [snd vi.var_clock_eff];
                           ve_src = lxm
                          }
                        in
                        (flagit id lxm), ve                      
                    in
                    get_field_of_id id_with id lxm
              )
              fl
          in  
          (namargs, [struct_type])
        | _ -> raise (Compile_error(lxm, "type error: a structure is expected"))

and (eval_merge : IdSolver.t -> Lic.val_exp -> Lxm.t -> 
     (Lic.const Lxm.srcflagged * Lic.val_exp) list -> Lic.val_exp_core * Lic.type_ list) =
  fun id_solver mclk lxm nargs -> 
    let id_clk = match mclk with
      | {ve_core=CallByPosLic({it=VAR_REF id;_},[]);_} -> id
      | _ -> assert false
    in
    let tclk = (IdSolver.var_info_of_ident id_solver id_clk lxm).var_type_eff in
    let nargs,tl_opt = 
      List.fold_left
        (fun (acc,tl_opt) (c,ve) ->
          (* check that id is of type tclk *)
          let id_type = type_of_const c.it in
          if id_type <> tclk then (
            let msg = "type error in a merge branch: " ^
              (Lic.string_of_type tclk) ^ " was expected, but " ^
              (Lic.string_of_type id_type) ^ " was provided. " 
            in
            raise (Compile_error(lxm, "type error: "^msg))
          );
          let ve,tl = f id_solver ve in
          let tl_opt = 
            match tl_opt with
              | None -> Some tl
              | Some tl' -> 
                if tl = tl' then tl_opt else
                  let tl2str tl = String.concat "*" (List.map Lic.string_of_type tl) in
                  let msg = "types differ in merge branches: " ^
                    (tl2str tl) ^ " <> " ^ (tl2str tl')
                  in 
                  raise (Compile_error(lxm, "type error: "^msg))
          in
          (c,ve)::acc, tl_opt
        )
        ([],None)
        nargs
    in
    let tl = match tl_opt with Some tl -> tl | None -> assert false in
    Merge(mclk, nargs), tl

