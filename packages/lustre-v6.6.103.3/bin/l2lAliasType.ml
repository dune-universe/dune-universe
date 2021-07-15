(* Time-stamp: <modified the 02/12/2019 (at 14:27) by Erwan Jahier> *)

(**  
Source 2 source transformation :
- toutes les expressions de type sans NOM
  (donc uniquement des tableaux immédiats ?)
  sont traquées et remplacées par un alias

 XXX Ce module est buggué. Des expressions de type apparaissent
     aussi dans les Lic.val_exp (via le champ ve_typ). Du coup, les
     clefs sur les types (comme Soc.key) ne sont plus canoniques.

*)

open Lic


let doit (inp : LicPrg.t) : LicPrg.t =
  (* n.b. on fait un minumum d'effet de bord pour
     pas avoir trop d'acummulateur ... *)
  let atab = Hashtbl.create 10 in
  let res = ref inp in

  (* UTILE : nommage des alias d'array *)
  let array_ident ty sz =
    let tid = Lic.ident_of_type ty in
    let sfx = Printf.sprintf "%s_%d" (snd tid) sz in
    let id = LicPrg.fresh_type_id !res (fst tid) sfx in
    id 
  in

  (* UTILE : cherche/crée un alias de type *)
  let rec alias_type te =
    match te with
      | Array_type_eff (ty, sz) -> (
        let ty = alias_type ty in
        let te = Array_type_eff (ty, sz) in
        try
          let ref_te = Hashtbl.find atab te in
          (*
            Lv6Verbose.printf "--> alias_type %s = %s ^ %d FOUND : %s\n"
            (LicDump.string_of_type_eff te)
            (LicDump.string_of_type_eff ty)
            sz
            (LicDump.string_of_type_eff ref_te);
          *)
          ref_te
        with Not_found -> (
          let id = array_ident ty sz in
          let ref_te = Abstract_type_eff (id, te) in
          res := LicPrg.add_type id ref_te !res;
          Hashtbl.add atab te ref_te; 
          (*
            Lv6Verbose.printf "--> alias_type %s = %s ^ %d NOT FOUND, gives: %s\n"
            (LicDump.string_of_type_eff te)
            (LicDump.string_of_type_eff ty)
            sz
            (LicDump.string_of_type_eff ref_te);
          *)
          ref_te 
        )
      ) 
      | Struct_type_eff (id, fields) -> 
        let do_field (id, (tf, co)) =
          (id, (alias_type tf, co)) 
        in
        Struct_type_eff (id, List.map do_field fields)
        
      | _ -> te
  in

  (* TRAITE LES TYPES *)
  let do_type k te =
    let te' = match te with
      | Array_type_eff (t, sz) -> Array_type_eff (alias_type t, sz)          
      | Struct_type_eff (id, fields) -> 
        let do_field (id, (tf, co)) =
          (id, (alias_type tf, co)) 
        in
        Struct_type_eff (id, List.map do_field fields)
      | _ -> te
    in
    if (te = te') then () 
    else
      res := LicPrg.add_type k te' !res 
  in
  LicPrg.iter_types do_type inp;

  (* TRAITE LES CONSTANTES *)
  let do_const k ec =
    let ec' = match ec with
      | Extern_const_eff (i, te) ->
        let te' = alias_type te in
        Extern_const_eff (i, te')
      | Abstract_const_eff (i, te, c, b) ->
        let te' = alias_type te in
        Abstract_const_eff (i, te', c, b)
      | Array_const_eff (cl, te) ->
        let te' = alias_type te in
        Array_const_eff (cl, te')
      | Bool_const_eff _
      | Int_const_eff _ 
      | Real_const_eff _
      | Enum_const_eff _ 
      | Struct_const_eff _
      | Tuple_const_eff _ -> ec
    in
    if (ec = ec') then ()
    else 
      (* n.b. add=replace *)
      res := LicPrg.add_const k ec' !res 
  in 
  LicPrg.iter_consts do_const inp ;

  (* TRAITE LES NOEUDS *)
  let do_node k en =
    (* n.b. les Lic.type_ apparraissent uniquement dans les var infos *)
    let do_var vi =
      let ty = alias_type vi.var_type_eff in
      {vi with var_type_eff = ty}
    in
    let en' = { en with
      inlist_eff = (List.map do_var en.inlist_eff);
      outlist_eff = (List.map do_var en.outlist_eff);
      loclist_eff = (
        match en.loclist_eff with
          | Some vl -> Some (List.map do_var vl)
          | None -> None
      )
    } in
    (* on fait pas dans la dentelle, on remplace ... *)
    res := LicPrg.add_node k en' !res
  in
  LicPrg.iter_nodes do_node inp;
  !res
