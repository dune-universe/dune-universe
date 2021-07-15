(* Time-stamp: <modified the 29/08/2019 (at 14:56) by Erwan Jahier> *)
(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
*)

open Lv6MainArgs
open Soc
open SocExecValue
open RdbgPlugin

       
let make_do argv opt =
  Lv6Verbose.exe ~level:3 (fun () ->
    Gc.set { (Gc.get ()) with Gc.verbose = 0x01 }
  );
  if (opt.infiles = []) then (Lv6MainArgs.usage stderr opt; exit 1);
  let new_dft_pack = Filename.basename (Filename.chop_extension (List.hd opt.infiles)) in
  Lv6Id.set_dft_pack_name new_dft_pack;

  let main_node = 
    if opt.main_node = "" then None else 
      Some (Lv6Id.idref_of_string opt.main_node)
  in
  if opt.outfile <> "" then opt.oc <- open_out opt.outfile;
  let nsl = Lv6Compile.get_source_list opt opt.infiles in
  let lic_prg = Lv6Compile.doit opt nsl main_node in

  let nk = (Lic.node_key_of_idref (Lv6Id.to_idref opt.main_node)) in
  let sk, soc_tbl = 
    if LicPrg.node_exists lic_prg nk then (
      Lic2soc.f lic_prg nk 
    ) else (
      print_string ("Error: cannot find node "^opt.main_node^" in "^
                       (String.concat "," opt.infiles)^".\n");
      flush stdout;
      exit 1 
    )
  in
  let soc = SocUtils.find_no_exc sk soc_tbl in
  let soc_inputs,soc_outputs = soc.profile in
  let soc_inputs,soc_outputs =
    if opt.Lv6MainArgs.expand_io_type then
      (SocVar.expand_profile true false (fst soc.profile)),
      (SocVar.expand_profile true false (snd soc.profile))
    else
      soc_inputs,soc_outputs
  in
  let (vntl_i:Data.vntl) =  soc_inputs in
  let (vntl_o:Data.vntl) =  soc_outputs in
(*   LicDump.dump_entete oc; *)
(*   RifIO.write_interface oc vntl_i vntl_o None None; *)
(*   RifIO.flush oc; *)

  let (to_soc_subst : SocExecValue.ctx -> Soc.var list -> Data.subst list) =
    fun ctx _vl ->
      (*       let sl = List.map (fun var -> fst var, SocExecValue.get_value ctx (Var var)) vl in *)
      let sl = SocExecValue.filter_top_subst ctx.s in
      let sl = List.flatten (List.map SocVar.expand_subst sl) in
      (* If the order ever matters, I could try the following.  :
         try List.map (fun v -> fst v,
         List.assoc (fst v) sl) vl with Not_found -> assert false
      *)
      sl
  in
  let (add_subst : Data.subst list -> SocExecValue.substs -> SocExecValue.substs) = 
    fun s ctx_s ->
      let s = SocVar.unexpand_profile s (fst soc.profile) in
      List.fold_left (fun acc (id,v) -> SocExecValue.sadd acc [id] v) ctx_s s
  in
  let ctx_ref = ref (SocExecValue.create_ctx soc_tbl soc) in
  let ss_table = Hashtbl.create 10 in
  let step sl_in =
    let ctx = { !ctx_ref with s = add_subst sl_in !ctx_ref.s } in
    let ctx = SocExecDbg.do_step soc_tbl soc ctx in
    let sl_out = to_soc_subst ctx soc_outputs in
    ctx_ref := ctx;
    (*     RifIO.write_outputs oc Util.my_string_of_float  vntl_o sl_out; *)
    (*     RifIO.flush oc; *)
    sl_out
  in
  let step_dbg sl_in ectx cont =
    let cont2 ectx ctx =
      let sl_out = to_soc_subst ctx soc_outputs in
      ctx_ref := ctx;
      cont sl_out ectx
    in
    ctx_ref := { !ctx_ref with s = add_subst sl_in !ctx_ref.s };
    SocExecDbg.do_step_dbg soc_tbl soc ectx !ctx_ref cont2
  in 
  let (mems_in  : Data.subst list) = [] in (* XXX todo *)
  let (mems_out : Data.subst list) = [] in (* XXX todo *)
  {
    id = Printf.sprintf "%s (with lv6 Version %s)"
                        (String.concat " " (Array.to_list argv)) Lv6version.str;
    inputs = vntl_i;
    outputs= vntl_o;
    reset=(fun () -> ctx_ref := SocExecValue.create_ctx soc_tbl soc);
    kill=(fun _ -> if opt.outfile <> "" then (flush opt.oc; close_out opt.oc));
    save_state = (fun i -> Hashtbl.replace ss_table i (!ctx_ref));
    restore_state = (fun i ->
        match Hashtbl.find_opt ss_table i with
        | Some (x) -> ctx_ref := x
        | None  -> Printf.eprintf "Cannot restore state %i from lv6\n" i; flush stderr 
        );
    init_inputs=mems_in;
    init_outputs=mems_out;
    step=step;     
    step_dbg=step_dbg;
  }

open Lv6errors

let my_exit opt i =
  if opt.outfile <> "" then (
      flush opt.oc;
      close_out opt.oc
  );
  flush stdout;
  flush stderr;
  (*   if i>0 && Sys.file_exists opt.outfile then Sys.remove opt.outfile; *)
  exit i

let make argv =
  let opt = Lv6MainArgs.parse argv in
  try make_do argv opt with
  | Sys_error(s) -> prerr_string (s^"\n");  my_exit opt 1 
  | Global_error s -> print_global_error s; my_exit opt 1 
  | Parsing.Parse_error ->
     print_compile_error (Lxm.last_made ()) "Syntax error";
     exit 1

  | Unknown_var(lxm,id) -> 
     print_compile_error lxm ("unknown variable (" ^ (Lv6Id.to_string id) ^")");
     my_exit opt 1
  | Unknown_constant(lxm,str) -> 
     print_compile_error lxm ("unknown constant (" ^ str ^")");
     my_exit opt 1
  | Compile_error(lxm,msg) -> print_compile_error lxm msg;  my_exit opt 1
  | L2lCheckLoops.Error(lxm,msg,lic_prg) -> 
     (* Sometime it helps to see the current state of the faulty program *)
     let main_node = 
       if opt.main_node = "" then None else 
         Some (Lv6Id.idref_of_string opt.main_node)
     in
     LicPrg.to_file opt lic_prg main_node;
     flush  opt.oc;
     print_compile_error lxm msg;
     my_exit opt 1
  | SocExec.AssertViolation lxm ->
     print_compile_error lxm "An assertion is violated in the Lustre program";
     my_exit opt 1
  | Assert_failure (file, line, col)  -> 
     prerr_string (
         "\nError: oops, lv6 internal error\n\tFile \""^ file ^
           "\", line " ^ (string_of_int line) ^ ", column " ^
             (string_of_int col) ^ "\nError: when compiling lustre program" ^
               (if List.length opt.infiles > 1 then "s " else " ") ^
                 (String.concat ", " opt.infiles) ^ "\n"^
                   "\nError: You migth want to sent a bug report to "^Lv6version.maintainer ^"\n") ; 
     flush stderr;
     my_exit opt 2

       
