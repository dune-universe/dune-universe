(* Time-stamp: <modified the 13/03/2020 (at 11:44) by Erwan Jahier> *)

open Soc2cIdent

let (is_extern_type: Lic.type_ -> bool) =
  function 
    | Lic.External_type_eff _ -> true
    | _ -> false

let (_is_extern_const: Lic.const -> bool) =
  function 
    | Lic.Extern_const_eff _ -> true
    | _  -> false

let (type_decl : LicPrg.t -> string) =
  fun prg -> 
    let type_to_string k t acc =
      if is_extern_type t then
        Printf.sprintf "%s\ntypedef FAKE_TYPE_DEF_2FIX %s;" acc  (long2s k)
      else acc
    in
    let preambule = "\n/* XXX FIXME: The following types must be defined/fixed */" ^
      "\n#define FAKE_TYPE_DEF_2FIX char*\n" 
    in
    let str = LicPrg.fold_types type_to_string prg "" in
    if str = "" then "" else (preambule^""^str^"\n")


(* Now done in soc2cGenAssign.ml
let (cpy_def : LicPrg.t -> string) =
  fun prg -> 
    let type_to_string k t acc =
      match t with
        | Lic.External_type_eff t ->
          Printf.sprintf "%s\nvoid _assign_%s(%s* x,%s y){}" acc (long2s k) (long2s k) (long2s k)
        | _  -> acc
    in
    let preambule = "\n/* XXX FIXME: The following copy functions must be defined/fixed */" in
    let str = LicPrg.fold_types type_to_string prg "" in
    if str = "" then "" else (preambule^str^"\n")

(* exported *)
let (cpy_declaration : LicPrg.t -> string) =
  fun prg -> 
    let type_to_string k t acc =
      match t with
        | Lic.External_type_eff t -> 
          Printf.sprintf "%s\nextern void _assign_%s(%s*,%s);" acc (long2s k) (long2s k) (long2s k)
        | _  -> acc
    in
    let preambule = "/*  */\n" in
    let str = LicPrg.fold_types type_to_string prg "" in
    if str = "" then "" else (preambule^str^"\n")

let (cpy_decl : LicPrg.t -> string) =
  fun prg -> 
    let type_to_string k t acc =
      match t with
        | Lic.External_type_eff t -> 
          Printf.sprintf "%s\nvoid _assign_%s(%s*,%s);" acc (long2s k) (long2s k) (long2s k)
        | _  -> acc
    in
    let preambule = "/*  */\n" in
    let str = LicPrg.fold_types type_to_string prg "" in
    if str = "" then "" else (preambule^str^"\n")
*)


let (const_def : LicPrg.t -> string) =
  fun prg -> 
    let const_to_string k c acc =
      match c with 
        | Lic.Extern_const_eff(_,t) -> 
          let t = Soc2cUtil.lic_type_to_c t (long2s k) in 
          Printf.sprintf "%s\n%s=FAKE_CONST_DEF_2FIX;" acc t
        | _ -> acc
    in
    let preambule = "\n/* XXX FIXME: The following const def must be defined/fixed */" ^
      "\n#define FAKE_CONST_DEF_2FIX 1" in
    let str = LicPrg.fold_consts const_to_string prg "" in
    if str = "" then "" else (preambule^str^"\n")

(* exported *)
let (const_declaration : LicPrg.t -> string) =
  fun prg -> 
    let const_to_string k c acc =
      match c with 
        | Lic.Extern_const_eff(_,t) -> 
          let t = Soc2cUtil.lic_type_to_c t (long2s k) in 
          Printf.sprintf "%s\nextern %s;" acc t
        | _ -> acc
    in
    let preambule = "/* */\n" in
    let str = LicPrg.fold_consts const_to_string prg "" in
    if str = "" then "" else (preambule^str^"\n")

let (gen_getters : string -> LicPrg.t -> Soc.t -> string) =
  fun fn prg soc ->
  let ctx = get_ctx_name soc.key in
  let type_to_string k t acc =
    if is_extern_type t then (
        Printf.eprintf "W: please check the def of _get_%s in %s.\n%!" (long2s k) fn;
        Printf.sprintf "%s 
%s _get_%s(%schar* n) {
  %s r; 
  // XXX the code below is just a guess: you should check it is ok
  r = _get_string(%sn);
  return r;
}" acc (long2s k) (long2s k)
          (if SocUtils.is_memory_less soc then "" else
             (Printf.sprintf "%s_type * ctx, " ctx))
          (long2s k)
          (if SocUtils.is_memory_less soc then "" else "ctx, ")
      )
      else acc
    in
    let preambule = "\n/* XXX FIXME: The getters may need to be fixed too */" in
    let str = LicPrg.fold_types type_to_string prg "" in
    if str = "" then "" else (preambule^""^str^"\n")

                               
open Soc

let (gen_files : Soc.t -> Soc.tbl -> LicPrg.t -> string -> string -> string ->
                 bool * bool) = 
  fun _msoc stbl licprg ext_cfile ext_hfile hfile -> 
    let extern_steps = SocMap.fold
      (fun _sk soc acc -> 
        List.fold_left (fun acc sm -> if sm.impl=Extern then (sm,soc)::acc else acc) 
          acc soc.step
      )
      stbl []
    in
    let extern_types = LicPrg.fold_types 
      (fun _ t acc -> match t with Lic.External_type_eff et -> et::acc | _ -> acc)
      licprg []
    in
    let extern_consts = LicPrg.fold_consts 
      (fun _ c acc -> match c with Lic.Extern_const_eff(ec,_) -> ec::acc | _ -> acc)
      licprg []
    in
    let needs_cfile = extern_steps <> []  || extern_consts <> [] in
    let needs_hfile = needs_cfile || extern_types<>[] || extern_consts<>[] in

      if not (Sys.file_exists ext_hfile) && needs_hfile then (
        let ext_och = open_out ext_hfile in

        Printf.eprintf "W: please check the def of FAKE_TYPE_DEF_2FIX in %s.\n%!"
          ext_hfile;
        output_string ext_och (type_decl licprg);
(*         output_string ext_och (cpy_decl licprg); *)

        List.iter (fun (sm,soc) -> 
                   (* let sname = Soc2cDep.step_name soc.key sm.name in *)
          let proto_decl,_,_ = Soc2cDep.get_step_prototype sm soc in 
          output_string ext_och proto_decl; 
        ) extern_steps;
        close_out ext_och;
        Printf.eprintf "W: %s has been generated.\n%!" ext_hfile;
      );
      if not (Sys.file_exists ext_cfile)  && needs_cfile then (
        let ext_occ = open_out ext_cfile in
        if needs_hfile then 
          output_string ext_occ (Printf.sprintf "#include \"%s\"\n" hfile);
        (*         output_string ext_occ (cpy_def licprg); *)
        Printf.eprintf "W: please check the def of FAKE_CONST_DEF_2FIX in %s.\n%!"
          ext_cfile;
        output_string ext_occ (const_def licprg);
        List.iter (fun (sm,soc) -> 
                   (* let sname = Soc2cDep.step_name soc.key sm.name in *)
          let _,proto_begin,_ = Soc2cDep.get_step_prototype sm soc in 
          output_string ext_occ proto_begin;
          output_string ext_occ (Printf.sprintf "   /* finish me! */\n}\n")
        ) extern_steps;
        close_out ext_occ;
        Printf.eprintf "W: %s has been generated.\n" ext_cfile;
      );
      needs_cfile, needs_hfile


