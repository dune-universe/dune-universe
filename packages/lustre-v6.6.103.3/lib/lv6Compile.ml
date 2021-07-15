(* Time-stamp: <modified the 29/08/2019 (at 16:45) by Erwan Jahier> *)

open Lxm
open AstV6

(* get the first package in the package/model list *)
let dbg = (Lv6Verbose.get_flag "ast")

let profile_info = Lv6Verbose.profile_info

let split opt zelic = 
  if
    Lv6MainArgs.global_opt.Lv6MainArgs.one_op_per_equation
    || opt.Lv6MainArgs.expand_nodes (* expand performs no fixpoint, so it will work
                                           only if we have one op per equation...*)
  then (
    (* Split des equations (1 eq = 1 op) *)
    profile_info "One op per equations...\n";
    L2lSplit.doit opt zelic)
  else 
    zelic

let expand_nodes opt main_node zelic =
  if opt.Lv6MainArgs.expand_node_call <> [] || opt.Lv6MainArgs.expand_nodes then (
    let mn:Lv6Id.idref = 
      match main_node with
      | None -> 
         (match LicPrg.choose_node zelic with
          | None -> assert false
          | Some(nk,_) -> Lv6Id.idref_of_long (fst nk)
         )
      | Some mn -> mn
    in
    let ids_to_expand =
      List.map Lv6Id.idref_of_string opt.Lv6MainArgs.expand_node_call
    in
    let long_match_idref (p,n) idref =
      (* if no pack is specified, we match them all *)
      (Lv6Id.name_of_idref idref = n)
      && (match Lv6Id.pack_of_idref idref with
            None -> true
          | Some p2 -> p = p2)
    in
    let nodes_to_keep: Lic.node_key list = 
      LicPrg.fold_nodes
        (fun (long,sargs) _ acc -> 
         if opt.Lv6MainArgs.expand_nodes then 
           (if long_match_idref long mn then
              (long,sargs)::acc
            else
              acc)
         else if
           List.exists (long_match_idref long) ids_to_expand 
         then 
           acc 
         else 
           (long,sargs)::acc
        )
        zelic
        []
    in
    assert (nodes_to_keep <> []);
    profile_info ("Expanding the following node calls: "
       ^(String.concat "," (List.map (Lv6Id.string_of_idref false) ids_to_expand))^"\n");
    profile_info ("Keeping the following node calls: "
      ^(String.concat "," (List.map Lic.string_of_node_key nodes_to_keep))^"\n");
    L2lExpandNodes.doit nodes_to_keep zelic
  )
  else 
    zelic

(* may introduce arrays, that may need to be expanded, so
   this has to be done before expand_arrays *)
let expand_enums _opt zelic =
  match Lv6MainArgs.global_opt.Lv6MainArgs.expand_enums with
  | Lv6MainArgs.AsBool -> L2lExpandEnum.doit L2lExpandEnum.BA zelic
  | Lv6MainArgs.AsInt  -> L2lExpandEnum.doit L2lExpandEnum.I  zelic
  | Lv6MainArgs.AsEnum
  | Lv6MainArgs.AsConst -> zelic

let remove_polymorphism _opt zelic =
(* élimination polymorphisme  surcharge *)
  profile_info "Removing polymorphism...\n";
  L2lRmPoly.doit zelic 
    
let expand_iterators opt zelic =
  if not opt.Lv6MainArgs.inline_iterator then zelic else (
    profile_info "Inlining iterators...\n";
    (* to be done before array expansion otherwise they won't be expanded *)
    let zelic = L2lExpandMetaOp.doit zelic in
    if Lv6MainArgs.global_opt.Lv6MainArgs.kcg && not opt.Lv6MainArgs.inline_iterator
    then 
	   L2lExpandMetaOp.doit_boolred zelic
    else
	   zelic
  )

let optimize_ite opt zelic =
  if not opt.Lv6MainArgs.optim_ite then zelic else ( 
    profile_info "Optimizing if/then/else...\n";
    L2lOptimIte.doit zelic)

(* Array and struct expansion: to do after polymorphism elimination
and after node expansion *)
let expand_arrays opt zelic =
  if not opt.Lv6MainArgs.expand_arrays then zelic else (
    profile_info "Expanding arrays...\n";
    let zelic = L2lExpandArrays.doit zelic in
    let zelic = split opt zelic in
    zelic  
  )

(* alias des types array XXX fait partir lic2soc en boucle à
   cause des soc key qui ne sont plus cohérentes entre elles 
  (cf commentaire au début du module). Bon, j'enleve, car j'en ai
   pas vraiment besoin en plus.
 *)
let _alias_arrays _opt zelic = zelic
  (* profile_info "Aliasing arrays...\n";  *)
  (* let zelic = L2lAliasType.doit zelic in  *)
                             
let remove_aliases opt zelic =
  if opt.Lv6MainArgs.keep_aliases then zelic else L2lRemoveAlias.doit zelic

let when_on_idents _opt zelic =
  (* should be done after L2lOptimIte, as it introduces some 'when' *)
  if not Lv6MainArgs.global_opt.Lv6MainArgs.when_on_ident then zelic else ( 
    profile_info "Creating ident on when statements if necessary...\n";
    L2lWhenOnId.doit zelic)

let no_when_not _opt zelic = 
  if not Lv6MainArgs.global_opt.Lv6MainArgs.no_when_not then zelic else (
    profile_info "Replace 'when not' statements by new variables...\n";
    L2lNoWhenNot.doit zelic)

let check_loops _opt zelic main_node =  
    profile_info "Check loops...\n";
   (* let zelic = if opt.Lv6MainArgs.expand_arrays then zelic else
   (* L2lCheckLoops only works if struct and array are expanded *)
           L2lExpandArrays.doit zelic
    in
    L2lCheckLoops.doit zelic *)
      (* The dep loop check is done by Lic2soc.f 
       nb : we need to do this check before removing aliases.
       because if there is a loop, the remove alias pass is wrong
       *)
      match main_node with
      | None -> ()
      (* LicPrg.iter_nodes (fun n _ -> ignore(Lic2soc.f zelic n)) zelic  *)
      | Some mn -> ignore(Lic2soc.f zelic (Lic.node_key_of_idref mn))

let check_decl opt zelic =
  profile_info "Check safety and memory declarations...\n";
  if opt.Lv6MainArgs.gen_c then 
    L2lCheckCKeyWord.doit zelic;
  if  Lv6MainArgs.global_opt.Lv6MainArgs.kcg then 
    L2lCheckKcgKeyWord.doit zelic
  else
    L2lCheckMemSafe.doit zelic

let check_outputs _opt zelic =
  profile_info "Check unique outputs...\n";
  L2lCheckOutputs.doit zelic
                         
let (doit : Lv6MainArgs.t -> AstV6.pack_or_model list -> Lv6Id.idref option ->
            LicPrg.t) =
  fun opt srclist main_node ->
  (*     let t0 = Sys.time() in *)
  profile_info "Lv6Compile: Start!\n";
  let syntax_tab = AstTab.create srclist in
  (* Pour chaque package, on a un solveur de références
       globales, pour les types, const et node :
       - les références pointées (p::n) sont recherchées
       directement dans la syntax_tab puisqu'il n'y a pas 
       d'ambiguité
       - les références simples sont recherchées :
       . dans le pack lui-même
       . dans un des packs déclarés "uses", avec
       priorité dans l'ordre
   *)
  let lic_tab = LicTab.create syntax_tab in
  Lv6Verbose.exe ~flag:dbg (fun () -> AstTab.dump syntax_tab);

  profile_info "Lv6Compile: Compiling into lic\n";
  let lic_tab = match main_node with
    | None -> LicTab.compile_all lic_tab
    | Some main_node -> 
       if opt.Lv6MainArgs.compile_all_items then
         LicTab.compile_all lic_tab
       else 
         LicTab.compile_node lic_tab main_node
  in
  profile_info "Converting to lic_prg...\n";
  let zelic = LicTab.to_lic_prg lic_tab in
  if opt.Lv6MainArgs.print_interface then zelic else (
    check_decl opt zelic;
    
    let zelic = optimize_ite        opt zelic in
    let zelic = remove_polymorphism opt zelic in
    let zelic = expand_iterators    opt zelic in (* before expand_arrays *)
    let zelic = split               opt zelic in (* after expand_iterators *)
    let zelic = expand_enums        opt zelic in (* before expand_arrays *)
    let zelic = when_on_idents      opt zelic in (* after optimize_ite *)
    let zelic = expand_nodes        opt main_node zelic in (* after split *)
    let zelic = no_when_not         opt zelic in
    let zelic = expand_arrays       opt zelic in (* after expand_nodes 
                                                    and remove_polymorphism *)
    check_loops opt zelic main_node; 
    let zelic = remove_aliases      opt zelic in (* after check_loops *)
    (* let zelic = alias_arrays opt zelic in  *)
    check_outputs opt zelic;
    profile_info "Lic Compilation done!\n";
    zelic
  )    
      
let test_lex ( lexbuf ) = (
  let tk = ref (Lv6lexer.lexer lexbuf) in 
  while !tk <> Lv6parser.TK_EOF do
    match (Lv6lexer.token_code !tk) with 
	     ( co , lxm ) ->
	       Printf.printf "line %3d col %2d to %2d : %15s = \"%s\"\n"
	         (line lxm) (cstart lxm) (cend lxm) co (str lxm) ;
	       tk := (Lv6lexer.lexer lexbuf)
  done
)

(* Retourne un AstV6.t *)
let lus_load lexbuf = 
  let tree = Lv6parser.program Lv6lexer.lexer lexbuf in
    FreshName.update_fresh_var_prefix ();
    AstRecognizePredef.f tree
  
type maybe_packed = 
  | Packed of AstV6.pack_or_model
  | Unpacked of AstV6.packbody 

let (get_source_list : Lv6MainArgs.t -> string list -> AstV6.pack_or_model list) =
  fun opt infile_list -> 
    let (get_one_source : string -> string list * maybe_packed list) = 
      fun infile -> 
        let incl_files, l =
          let lexbuf = Lv6MainArgs.lexbuf_of_file_name infile in
          if opt.Lv6MainArgs.tlex then test_lex lexbuf;
          match (lus_load lexbuf) with
          | PRPackBody(incl_files, pbdy) -> incl_files, [Unpacked pbdy]
          | PRPack_or_models(incl_files, nsl) ->
            incl_files, (List.map (fun ns -> Packed ns) nsl)
        in
        (* If included files have a relative path, strange things may happen.
           Hence we make the path absolute, using the directory of the includer.
        *)
        let includer_dir = Filename.dirname infile in
        let fix_dir f = if Filename.is_relative f then
            Filename.concat includer_dir f else f in
        let incl_files = List.map fix_dir incl_files in
        incl_files, l
    in
    let rec (get_remaining_source_list : maybe_packed list * string list * string list -> 
             maybe_packed list * string list * string list) =
      fun (pack_acc, compiled, to_be_compiled) -> 
        match to_be_compiled with
        | [] -> (pack_acc, compiled, [])
        | infile::tail ->
          let infile = FilenameExtras.simplify infile in
          if List.mem infile compiled then
            get_remaining_source_list (pack_acc, compiled, tail)
          else
            let included_files, pack = get_one_source infile in
            let new_pack_acc = pack_acc@pack in
            get_remaining_source_list(
              new_pack_acc, 
              infile::compiled, 
              tail@included_files)
    in
    let infile_list = 
      (* We need absolute paths to make sure that files are not
         included several times.  Indeed, otherwise,
         FilenameExtras.simplify may miss some simplifications.  For
         example, consider the files "../../x/toto.lus" and
         "../toto.lus".  They actually refer to the same file if the
         current directory is a sub-directory of "x". Working with
         absolute paths solves the problem.

      *)
      let make_it_absolute f = 
        if Filename.is_relative f then Filename.concat (Sys.getcwd ()) f else f 
      in
      List.map make_it_absolute infile_list
    in
    let first_file = assert (infile_list <> []); List.hd infile_list in
    let included_files, first_pack = get_one_source first_file in
    let (pack_list, _compiled_files, included_files) = 
      get_remaining_source_list (first_pack, [first_file],
                                 (List.tl infile_list) @ included_files)
    in
    let _ = assert (included_files=[]) in
    let packed_list, unpacked_list = 
      List.fold_left 
        (fun (pl, upl) p -> 
           match p with
           | Packed p ->  p::pl, upl
           | Unpacked up -> pl, up::upl
        )
        ([], [])
        pack_list
    in
    let unpacked_merged_opt = (* All unpacked files are merged into one single package *)
      List.fold_left
        (fun acc pbody -> 
           match acc with
           | None -> Some pbody
           | Some pbody_acc -> 
             let add tbl x y =
               (* Let's perform some clashes checks *)
               if Hashtbl.mem tbl x then
                 let ybis = Hashtbl.find tbl x in
		           print_string ("*** Error: "^(Lv6Id.to_string x)^
                               " is defined twice: \n\t" ^ 
                               (Lxm.details y.src) ^ "\n\t" ^
                               (Lxm.details ybis.src) ^ ".\n"); 
                 exit 2
               else
                 Hashtbl.add tbl x y
             in
             Hashtbl.iter (fun x y -> add pbody_acc.pk_const_table x y)
               pbody.pk_const_table;
             Hashtbl.iter (fun x y -> add pbody_acc.pk_type_table x y)
               pbody.pk_type_table;
             Hashtbl.iter (fun x y -> add pbody_acc.pk_node_table x y)
               pbody.pk_node_table;
             Some { pbody_acc with
                    pk_def_list=pbody_acc.pk_def_list@pbody.pk_def_list;
                  }
        )
        None
        unpacked_list
    in
    match unpacked_merged_opt with
    | None -> packed_list
    | Some unpacked_merged ->
      let name = 
        try Filename.chop_extension (Filename.basename first_file) 
        with _ -> 
		    print_string ("*** Error: '"^first_file^"' is a bad file name.\n"); exit 1
      in
      let pi = AstV6.give_pack_this_name (Lv6Id.pack_name_of_string name) unpacked_merged in
      let p = NSPack (Lxm.flagit pi (Lxm.dummy name)) in
      p::packed_list
