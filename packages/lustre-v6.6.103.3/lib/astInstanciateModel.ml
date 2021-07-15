(* Time-stamp: <modified the 26/02/2015 (at 11:19) by Erwan Jahier> *)

open Lxm
open AstV6
open AstCore
open Lv6errors



let instance_error lxm = 
  let msg = Printf.sprintf
    "bad argument in package instance: %s" (Lxm.details lxm)
  in
    raise (Compile_error (lxm, msg)) 


(* Model instanciation is done via a call by name binding.  This
   function checks whether each parameter matches one of the arguments,
   and returns (by appending it to an accumulator):

   - the item (const, type, node) corresponding to the parameter:
   - its definition.
*)
type check_arg_acc = item_ident list * item_info srcflagged list
type tables = 
    (Lv6Id.t, const_info Lxm.srcflagged) Hashtbl.t *
    (Lv6Id.t, type_info Lxm.srcflagged) Hashtbl.t *
    (Lv6Id.t, node_info Lxm.srcflagged) Hashtbl.t 


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


let (check_arg :
       tables -> (Lv6Id.t * static_arg srcflagged) list -> check_arg_acc -> 
     static_param srcflagged -> check_arg_acc) =
  fun (ctab, ttab, ntab) args (defs, prov) param -> 
    let find_arg id =
      try List.assoc id args with Not_found -> instance_error param.src
    in
    match param.it with
      | StaticParamType s -> (
        let arg = find_arg s in
        let te = match arg.it with
          | StaticArgLv6Id idr -> Lxm.flagit (Named_type_exp idr) arg.src
          | StaticArgType x -> x
          | _ -> instance_error param.src
        in
        let ti = AliasedType (s, te) in
        let x = Lxm.flagit (TypeInfo ti) param.src in
        let y = Lxm.flagit ti param.src in
        put_in_tab "type" ttab s y;
        ((TypeItem s)::defs, x::prov)
      )
      | StaticParamConst (s,te) -> (
        let arg = find_arg s in
        let ce = match (arg.it) with
          | StaticArgLv6Id idr -> Lv6parserUtils.leafexp arg.src (IDENT_n idr) 
          | StaticArgConst x -> x
          | _ -> instance_error param.src
        in
        let ci = DefinedConst (s, Some te, ce) in
        let x = Lxm.flagit (ConstInfo ci) param.src in
        let y = Lxm.flagit ci param.src in
        put_in_tab "const" ctab s y ;
        ((ConstItem s)::defs, x::prov)
      ) 
      | StaticParamNode (s, inl, outl, has_memory, is_safe) -> (
        let arg = find_arg s in
        let by_pos_op = match (arg.it) with
          | StaticArgLv6Id idr -> CALL_n(Lxm.flagit ((idr,[])) arg.src)
          | StaticArgNode by_pos_op -> by_pos_op 
          | _ -> instance_error param.src 
        in
        let sparams = [] in
        let ni = {
          name = s;
          static_params = sparams;
          vars = Some (Lv6parserUtils.build_node_var inl outl None);
          loc_consts = [];
          def = Alias (flagit by_pos_op arg.src);
          has_mem = has_memory;
          is_safe = is_safe;
        } 
        in
        let x = Lxm.flagit (NodeInfo ni) param.src in
        let y = Lxm.flagit ni param.src in
        put_in_tab "node" ntab s y ;
        ((NodeItem (s,sparams))::defs, x::prov)
      )

let (f: (Lv6Id.t, AstV6.model_info Lxm.srcflagged) Hashtbl.t ->
     (AstV6.pack_info  Lxm.srcflagged) -> AstV6.pack_given) =
  fun mtab pdata ->
    match (pdata.it.pa_def) with
      | PackGiven pg -> pg 
      | PackInstance pi ->
          let mi = try Hashtbl.find mtab pi.pi_model  with Not_found ->
            let msg = Printf.sprintf "bad pack instance: model %s undeclared"
              (Lv6Id.to_string pi.pi_model)
            in
              raise ( Compile_error (pdata.src, msg))
          in
            (* On part du packbody du modèle, dont on duplique les tables *)
          let ctab = Hashtbl.copy mi.it.mo_body.pk_const_table in
          let ttab = Hashtbl.copy mi.it.mo_body.pk_type_table in
          let ntab = Hashtbl.copy mi.it.mo_body.pk_node_table in
          let args = pi.pi_args in
          let pars = mi.it.mo_needs in
          let (used_packages : Lv6Id.pack_name srcflagged list) =
            (* We add to the list of used packages the packages that are explicitely
               used in the model arguments *)
            List.fold_left
              (fun acc (_,arg) -> 
                 (match arg.it with
                    | StaticArgLv6Id(idref) ->
                        (match Lv6Id.pack_of_idref idref with
                           | None -> acc
                           | Some p -> 
                               let p_flagged = Lxm.flagit p arg.src in
                                 if List.mem p_flagged acc then acc else p_flagged::acc
                        )
                    | _ -> acc
                 )
              )
              mi.it.mo_uses
              args
          in
          let (newdefs, newprov) = 
            List.fold_left (check_arg (ctab, ttab, ntab) args) ([],[]) pars 
          in
          let pars_nb = string_of_int (List.length pars)
          and args_nb = string_of_int (List.length args) in
            try (
              (*------------TRAITEMENT---------------------------------*)
              if (pars_nb <> args_nb) then
                let msg = "\n*** " ^pars_nb ^ " arguments are expected, but "^args_nb^
                  " were provided when defining package "^
                  (Lv6Id.pack_name_to_string pdata.it.pa_name)
                in
                  raise(Compile_error (pdata.src, msg))
              else
                (* on fabrique un pack_given valide avec les infos récoltées *)   
                let body = {
                  pk_const_table = ctab ;
                  pk_type_table = ttab ;
                  pk_node_table = ntab ;
                  pk_def_list = (mi.it.mo_body.pk_def_list) @ (List.rev newdefs)
                } 
                in
                  (* les provides du modèle + les nouveaux de newprov *)
                  (* SAUF SI ON EXPORTE DEJA TOUT !                   *)
                let prov = match (mi.it.mo_provides) with
                  | Some l -> Some (l @ (List.rev newprov))
                  | None -> None
                in
                  {
                    pg_uses = used_packages;
                    pg_provides = prov ;
                    pg_body = body ;
                  }
            ) 
            with Invalid_argument _ ->
              let msg = Printf.sprintf
                "bad pack instance: %d args provided while model %s has %d params"
                (List.length args) (Lv6Id.to_string pi.pi_model) (List.length pars)
              in
                raise (Compile_error (pdata.src, msg))
      
