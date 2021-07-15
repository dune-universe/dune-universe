(* Time-stamp: <modified the 29/08/2019 (at 16:25) by Erwan Jahier> *)
open Lv6MainArgs

module ItemKeyMap = struct
	include Map.Make (
		struct
			type t = Lic.item_key
			let compare = compare
		end
	)
	let _dummy () = "dummy item: add things below to complete the module"
end

module NodeKeyMap = struct
	include Map.Make (
		struct
			type t = Lic.node_key
			let compare = compare
		end
	)
	let _dummy () = "dummy item: add things below to complete the module"
end

type t = {
  types  : Lic.type_ ItemKeyMap.t;
  consts : Lic.const ItemKeyMap.t;
  nodes  : Lic.node_exp NodeKeyMap.t;
  (* consts : (Lic.item_key, Lic.const    Lic.check_flag) Hashtbl.t; *)
  (* nodes  : (Lic.node_key, Lic.node_exp Lic.check_flag) Hashtbl.t; *)
}

let empty = {
   types = ItemKeyMap.empty;
   consts = ItemKeyMap.empty;
   nodes = NodeKeyMap.empty
}

(* KoKêterie : pour changer ds suffixes 
   numériques, produit :
   a, b, c, d, e, aa, ab, ac, ad, ae, etc ...
Pour tester :
for i = 0 to 42 do
Printf.printf "%3d -> %s\n" i (pretty_sfx i)
done
*)
let rec pretty_sfx i =
   if i = 0 then ""
   else
      (pretty_sfx ((i-1)/5))^(Char.escaped (char_of_int (97 + (i-1) mod 5)))


(** RECHERCHE *)
let find_type  this k = try Some(ItemKeyMap.find k this.types ) with Not_found -> None
let find_const this k = try Some(ItemKeyMap.find k this.consts) with Not_found -> None
let find_node  this k = try Some(NodeKeyMap.find k this.nodes ) with Not_found -> None
   
let node_exists this k = NodeKeyMap.mem k this.nodes


let (find_var : Lv6Id.t -> Lic.node_exp -> Lic.var_info option) = 
  fun id ne -> 
    let name_matches vi = vi.Lic.var_name_eff = id in
    try Some (List.find name_matches ne.Lic.inlist_eff) with Not_found -> 
    try Some (List.find name_matches ne.Lic.outlist_eff) with Not_found -> 
    match ne.Lic.loclist_eff with
      | None -> None
      | Some vil -> 
        try Some (List.find name_matches vil)
        with Not_found -> None


(** PARCOURS *)
let fold_consts (f: Lic.item_key -> Lic.const -> 'a -> 'a) (this:t) (accin:'a) : 'a =
   ItemKeyMap.fold f this.consts accin
let fold_types (f: Lic.item_key -> Lic.type_ -> 'a -> 'a) (this:t) (accin:'a) : 'a =
   ItemKeyMap.fold f this.types accin
let fold_nodes (f: Lic.node_key -> Lic.node_exp -> 'a -> 'a) (this:t) (accin:'a) : 'a =
   NodeKeyMap.fold f this.nodes accin

let list_nodes t = fold_nodes (fun k e acc -> (k,e)::acc) t []

let choose_node t = 
  (* since ocaml 3.12.0 only... NodeKeyMap.choose *)
  let rec aux = function
    | [] -> None
    | (nk,ne)::l -> 
      match ne.Lic.def_eff with
        | Lic.BodyLic _ -> Some(nk,ne) 
        | _ ->  aux l
  in 
  aux (list_nodes t)

let iter_consts (f: Lic.item_key -> Lic.const -> unit) (this:t) : unit =
   ItemKeyMap.iter f this.consts
let iter_types (f: Lic.item_key -> Lic.type_ -> unit) (this:t) : unit = 
   ItemKeyMap.iter f this.types
let iter_nodes (f: Lic.node_key -> Lic.node_exp -> unit) (this:t) : unit =
   NodeKeyMap.iter f this.nodes



let add_type (k:Lic.item_key) (v:Lic.type_) (prg:t) : t =
   { prg with types = ItemKeyMap.add k v prg.types }

let add_const (k:Lic.item_key) (v:Lic.const) (prg:t) : t =
   { prg with consts = ItemKeyMap.add k v prg.consts }

let add_node (k:Lic.node_key) (v:Lic.node_exp) (prg:t) : t =
  Lv6Verbose.exe ~level:3 (fun () ->
    Printf.printf  "## LicPrg.add_node %s\n" 
                   (LicDump.string_of_node_key_rec false false k));
  { prg with nodes = NodeKeyMap.add k v prg.nodes }

let del_node (k:Lic.node_key) (prg:t) : t =
  Lv6Verbose.exe ~level:3 (fun () ->
    Printf.printf  "## LicPrg.del_node %s\n" 
   (LicDump.string_of_node_key_rec false false k));
   { prg with nodes = NodeKeyMap.remove k prg.nodes }


(* to encode int into bools (for --expand-enums-as-bool)
let rec (int_to_bool_array_hot: int -> int -> bool list) =
  fun i size -> 
  assert(size >= 0);
  if size = 0 then [] else
  let d,r = i / 2, (i mod 2) = 1 in
  r::(int_to_bool_array d (size-1))
let _ = 
  assert (int_to_bool_array 1 3  = [true; false; false]);
  assert (int_to_bool_array 2 3  = [false; true; false]);
  assert (int_to_bool_array 8 3  = [false; false; false]);;
 *)
(* hot 1 encoding *)
let rec (int_to_bool_array: int -> int -> bool list) =
  fun i size -> 
  assert(size >= 0);
  if size = 0 then [] else
  let x = (i=0) in
  x::(int_to_bool_array (i-1) (size-1))

let _ = 
  assert (int_to_bool_array 0 3  = [true; false; false]);
  assert (int_to_bool_array 1 3  = [false; true; false]);
  assert (int_to_bool_array 2 4  = [false; false; true; false]);;



exception Print_me of Lic.node_exp
let to_file (opt: Lv6MainArgs.t) (this:t) (main_node: Lv6Id.idref option) = 
  LicDump.dump_entete opt.Lv6MainArgs.oc;
  if (global_opt.Lv6MainArgs.lv4) then () else (
    ItemKeyMap.iter
      (fun tn te ->
       (*        if (Lic.is_extern_type te) && ( *)
       (*          global_opt.Lv6MainArgs.kcg ||  *)
       (*            Lv6MainArgs.global_opt.Lv6MainArgs.expand_enums = AsEnum) *)
       (*        then  *)
       output_string opt.Lv6MainArgs.oc (LicDump.type_decl true tn te)
      (*        else  *)
      (*          () *)
      )
      this.types
  );

  (* const definition

     for generating lv4 or ec compatible code, enum types are
     translated into an extern type + an extern const per enums.

     For instance, 

     type color1 = enum { blue, white, black };
     type color2 = enum { green, orange, yellow };

     will be translated into 

     type color1;
     type color2;
     const orange:color2;
     const green:color2;
     const black:color1;
     const yellow:color2;
     const blue:color1;
     const white:color1;

   *)
  let to_const_list _types =
    ItemKeyMap.fold
      (fun _tn te acc -> 
       match te with
       | Lic.Enum_type_eff(long, longl) ->  
          (List.map (fun x -> long,x) longl) :: acc
       | _ -> acc
      )
      this.types
      []
  in                                                                   
  (match Lv6MainArgs.global_opt.Lv6MainArgs.expand_enums with
   | Lv6MainArgs.AsConst  ->
      if global_opt.kcg then () else (
        let const_list = 
          ItemKeyMap.fold
            (fun _tn te acc -> 
             match te with
             | Lic.Enum_type_eff(long, longl) ->  
                output_string opt.Lv6MainArgs.oc 
                              (LicDump.type_decl true long (Lic.External_type_eff long));
                List.rev_append (List.map (fun x -> long,x) longl) acc
             | _ -> acc
            )
            this.types
            []
        in
        List.iter
          (fun (t,elt) -> 
           let const = Lic.Extern_const_eff (elt, Lic.External_type_eff t) in
           output_string opt.Lv6MainArgs.oc (LicDump.const_decl true elt const))
          const_list;
      )
   | Lv6MainArgs.AsInt -> 
      if global_opt.kcg || global_opt.ec then () else (
        let const_list = to_const_list this.types in
        List.iter
          (List.iteri
             (fun i (_t,elt) ->
              let const = Lic.Int_const_eff (string_of_int i) in
              output_string opt.Lv6MainArgs.oc (LicDump.const_decl true elt const))
          )
          const_list;     
      )
   | Lv6MainArgs.AsBool -> (
     if global_opt.kcg || global_opt.ec then () else (
       let const_list = to_const_list this.types in
       List.iter
         (fun l ->
          let size = List.length l in
          (* let size = get_n size in *)
          (List.iteri
             (fun i (_t,elt) ->
              let bool_list = int_to_bool_array i size  in
              (*
              let get_n x = (* returns the n s.t., 2^(n-1) < x <= 2^n *)
                assert(x>0);
                let rec f n acc = if x > acc then f (n+1) (2*acc) else n in
                f 0 1
              in
              let size = get_n (List.length l) in
               *)

              let const = Lic.Array_const_eff 
                            (List.map (fun b -> Lic.Bool_const_eff(b)) bool_list,
                             Lic.Bool_type_eff)
              in
              output_string opt.Lv6MainArgs.oc (LicDump.const_decl true elt const))
          )
            l)
         const_list;     
     )
   )
   | Lv6MainArgs.AsEnum -> ()
  );
  ItemKeyMap.iter
    (fun cn ce -> 
     if (not Lv6MainArgs.global_opt.Lv6MainArgs.ec || Lic.is_extern_const ce) then
       output_string opt.Lv6MainArgs.oc (LicDump.const_decl true cn ce)
    )
    this.consts  ;
  if Lv6MainArgs.global_opt.Lv6MainArgs.ec then (
    (* in ec, we first need to declare the profile of extern nodes *)
    NodeKeyMap.iter 
      (fun (key,_) nexp -> (
         if nexp.Lic.def_eff = Lic.ExternLic && Lv6Id.pack_of_long key <> "Lustre" then (
           let str = (if nexp.Lic.has_mem_eff then "extern node " else "function ") ^
                       (Lv6Id.of_long key)^
                         (LicDump.profile_of_node_exp_eff true nexp)^".\n" 
           in
           output_string opt.Lv6MainArgs.oc (str);
           flush opt.Lv6MainArgs.oc;
         )))
      this.nodes ;
    
    (* If no node is set a top-level, the compiler will compile every node. But the
       ec format only accepts one node (and no type nor const) 
       Hence we print the first one (if no main node is set).
     *)
    try 
      NodeKeyMap.iter 
        (fun (key,_) nexp -> (
           match main_node with
           | Some { Lv6Id.id_pack = None ; Lv6Id.id_id= name } -> 
              if Lv6Id.of_long key = name && Lv6Id.pack_of_long key <> "Lustre" 
              then raise (Print_me nexp)
           | Some idref -> 
              if Lv6Id.long_of_idref idref = key then raise (Print_me nexp)
           | None -> (
             match nexp.Lic.node_key_eff, nexp.Lic.def_eff with
             (* only user or extern nodes with a body makes valid ec node *)
             | _, Lic.BodyLic _ -> raise (Print_me nexp)
             | _, Lic.ExternLic -> ()
             | _  -> ()
           )
         )
        )
        this.nodes        
    with Print_me nexp -> 
      output_string opt.Lv6MainArgs.oc (LicDump.node_of_node_exp_eff true nexp);
      flush opt.Lv6MainArgs.oc;
  ) else (
    (* Pour les noeuds, pas sur que ça marche tant qu'on n'a
       pas séparés les transformations  source_to_source du LicTab:
       en cas d'expansion, il y avait cette remarque :
       nb: we print res_struct, but do not return it from
       node_check, because the structure and array expansion
       modify (instanciate) the node profiles.

       On n'affiche PAS les extern Lustre::...
     *)

    NodeKeyMap.iter (
        fun _ nexp ->
        match nexp.Lic.node_key_eff with
        (* inutile d'écrire les noeuds predefs *)
        | (("Lustre",_),[]) -> ()
        | _ -> output_string opt.Lv6MainArgs.oc (LicDump.node_of_node_exp_eff true nexp)
      )
                    this.nodes
  )

(********************************************************************************)
(* exported *)

(** Creer Des Idents De Type Tout Frais *)
let fresh_type_id this pname pfx =
   let rec fresh x =
      let id = Printf.sprintf "%s%s" pfx (pretty_sfx x) in
      let res = Lv6Id.make_long pname id in
      if ItemKeyMap.mem res this.types then fresh (x+1)
      else res
   in
   fresh 0

