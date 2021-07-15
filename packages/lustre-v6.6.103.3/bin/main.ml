(* Time-stamp: <modified the 15/03/2021 (at 23:50) by Erwan Jahier> *)

open AstV6
open Lxm
open Lv6errors
open Parsing
open Format
open Lv6MainArgs

let my_exit opt i =
  if opt.rif then 
    output_string opt.oc "\nq\n" 
  else if opt.print_interface then 
    output_string opt.oc "\n" 
  else
    output_string opt.oc "\nbye\n";
  flush opt.oc;
  close_out opt.oc;
  flush stdout;
  flush stderr;
  if i>0 && Sys.file_exists opt.outfile then Sys.remove opt.outfile;
  exit i

let rec first_pack_in =
  function
    | (AstV6.NSPack pi)::_ -> pi.it.pa_name
    | (AstV6.NSModel _)::tail -> first_pack_in tail
    | [] -> raise (Global_error "No package has been provided")

(* try to use the filenale to guess the dft node *)
let find_a_node opt =
  let first_file = List.hd opt.infiles in
  let name = 
    try Filename.chop_extension (Filename.basename first_file) 
    with _ -> 
		print_string ("Error: '"^first_file^"' is a bad file name.\n"); 
      my_exit opt 1
  in
  name


let (gen_rif_interface : LicPrg.t -> Lv6Id.idref option -> Lv6MainArgs.t -> unit) =
  fun lic_prg main_node opt ->
  let nk = 
    match main_node with
    | None -> (
        let name = find_a_node opt in
        let main_node = Lv6Id.to_idref name in
        let nk = (Lic.node_key_of_idref main_node) in
        if LicPrg.node_exists lic_prg nk then nk else (
          output_string opt.oc ("Error: no node is specified.\nbye\n");
          flush opt.oc;
          my_exit opt 2
        )
      )
    | Some main_node -> Lic.node_key_of_idref main_node
  in
  let invars,outvars = 
    match LicPrg.find_node lic_prg nk with
    | None  -> assert false 
    | Some node_def -> Lic2soc.soc_profile_of_node node_def
  in
  let my_type_to_string t =
    match t with
    | Data.Extern _ -> "string" (* what else can i do with extern type for RIF? *)
    | _ ->
      let str = Data.type_to_string t in
      let idref = Lv6Id.idref_of_string str in
      (idref.Lv6Id.id_id)
  in
  let invars  = SocVar.expand_profile true false invars in
  let outvars = SocVar.expand_profile true false outvars in
  let invars_str  = List.map (fun (n,t) -> n^":"^(my_type_to_string t)) invars in
  let outvars_str = List.map (fun (n,t) -> n^":"^(my_type_to_string t)) outvars in
  output_string opt.oc ("#inputs "^ (String.concat " " invars_str) ^"\n");
  output_string opt.oc ("#outputs "^ (String.concat " " outvars_str) ^"\n");
  flush opt.oc


(* Generates a lutin env and a lustre oracle for the node *)
let (gen_autotest_files : LicPrg.t -> Lv6Id.idref option -> Lv6MainArgs.t -> unit) =
  fun lic_prg main_node opt -> 
    let msk, zesoc, main_node = 
      match main_node with
        | None -> (
          let name = find_a_node opt in
          let main_node = Lv6Id.to_idref name in
          let nk = (Lic.node_key_of_idref main_node) in
          if LicPrg.node_exists lic_prg nk then (
            output_string stdout ("WARNING: No main node is specified: will use "
                                  ^ name ^"\n");
            flush stdout;
            let msk, zesoc = Lic2soc.f lic_prg nk in
            msk, zesoc, main_node
          ) else (
            print_string ("Error: no node is specified, cannot exec.\nbye\n");
            flush stdout;
            my_exit opt 1
          )
        )
        | Some main_node -> 
          let msk, zesoc = Lic2soc.f lic_prg (Lic.node_key_of_idref main_node) in
          msk, zesoc, main_node
    in
    let assertions = 
      let main_node_lic = LicPrg.find_node lic_prg (Lic.node_key_of_idref main_node) in
      match main_node_lic with
        | Some({Lic.def_eff = Lic.BodyLic nlic;_}) -> nlic.Lic.asserts_eff
        | _ -> []
    in
    let assertion_to_lutin_cstr a = Assertion2lutin.f a.it in
    let assertions_cstr = List.map assertion_to_lutin_cstr assertions in 
    let assertions_cstr = List.filter (fun str -> str<>"") assertions_cstr in
    let assertion_cstr = String.concat " &> " assertions_cstr in
    let assertion_cstr = if assertion_cstr = "" then "loop true"
                         else "{ "^assertion_cstr^" }" in
    let my_type_to_string range_flag t =
      (* Remove the module name to have correct Lutin and lv4 type decl *)
      let str = Data.type_to_string t in
      let idref = Lv6Id.idref_of_string str in
      (if range_flag then (
        match t with
          | Data.Real -> " real [-10000.0;10000.0]"
          | Data.Int ->  " int [-10000; 10000]"
          | Data.Enum(_, idl) -> " int [0;"^(string_of_int (List.length idl - 1)) ^"]" 
          | _ -> idref.Lv6Id.id_id
       ) else idref.Lv6Id.id_id)
    in
    let soc = try SocUtils.find (Lxm.dummy "") msk zesoc with Not_found -> assert false in
    let invars,outvars=soc.Soc.profile in
    let invars = SocVar.expand_profile false false invars in
    let outvars = SocVar.expand_profile true false outvars in
    let invars_str  = List.map (fun (n,t) -> n^":"^(my_type_to_string true t)) invars in
    let outvars_str = List.map (fun (n,t) -> n^":"^(my_type_to_string false t)) outvars in
    let name = main_node.Lv6Id.id_id in
    let lutin_file_name =  ("_"^name^"_env.lut") in
    let oc = open_out lutin_file_name in
    Lv6util.entete oc "--" "";
    output_string oc ("node " ^ (name) ^ "_env("^ (String.concat ";" outvars_str) ^ 
                         ") returns(" ^ (String.concat ";" invars_str) ^ ") =\n   "^
                         assertion_cstr^" \n");
    flush oc; 
    close_out oc;
    output_string stdout (lutin_file_name ^" generated.\n"); 
    let oracle_file_name =  ("_"^name^"_oracle.lus") in
    let oc = open_out oracle_file_name in
    let invars,outvars=soc.Soc.profile in
    let locals = List.map (fun (n,t) -> n^"_bis",t) outvars in
    let invars_str  = List.map (fun (n,t) -> n^":"^(my_type_to_string false t)) invars in
    let outvars_str = List.map (fun (n,t) -> n^":"^(my_type_to_string false t)) outvars in
    let prg = "node "^name^"_oracle("^(String.concat ";" (invars_str@outvars_str)) in
    let locals_str = List.map (fun (n,t) -> n^":"^(my_type_to_string false t)) locals in
    let ok = (* avoid name clash*)
      let all_vars = List.map fst (List.rev_append
                                     (List.rev_append locals invars) outvars) in
      let rec gen_ok str = if List.mem str all_vars then gen_ok ("_"^str) else str in
      gen_ok "ok" 
    in 
    let prg = "-- oracle to compare Lustre compilers\n" ^ prg in
    let prg = prg^") \nreturns("^ok^":bool;"^(String.concat ";" locals_str)^");\nlet\n" in
    let locals_name  = List.map fst locals in
    let invars_name  = List.map fst invars in
    let prg = prg^"  ("^(String.concat "," locals_name)^") = "^name in
    let prg = prg^"("^(String.concat "," invars_name)^");\n  "^ok^" = (" in
    let (var_to_equals : Soc.var -> Soc.var -> string list) =
      fun (x,t) (y,_) -> 
        let rec aux x y t =
          match t with 
            | Data.Real -> ["r_equal("^x^","^y^")"]
            | Data.Array(t,n) -> 
              let res = ref [] in
              for i = 0 to n-1 do
                let istr = string_of_int i in
                res := !res @ aux (x^"["^istr^"]") (y^"["^istr^"]") t
              done;
              !res
            | Data.Struct(_n,fl) ->
              let do_field (fn,ft) = aux (x^"."^fn) (y^"."^fn) ft in
              List.flatten (List.map do_field fl)
            | _ ->  [x^"="^y]
        in
        aux x y t
    in
    let prg = try prg^(String.concat
                         " and\n \t\t  " 
                         (List.flatten
                            (List.map2 var_to_equals locals outvars))) ^");\ntel;\n"
              with _ -> assert false
    in
    let prg = prg^"\n-- oracle to compare two programs \n" in
    let prg = prg^"node "^name^"_oracle_prog("^(String.concat ";" (invars_str)) in
    let prg = prg ^") \nreturns("^ok^":bool;"^(String.concat ";" locals_str) in
    let prg = prg ^");\nvar\n"^(String.concat ";" outvars_str)^";\nlet\n" in
    let locals_name  = List.map fst locals in
    let invars_name  = List.map fst invars in
    let outvars_name  = List.map fst outvars in
    let prg = prg^"  ("^(String.concat "," outvars_name)^") = P1" in
    let prg = prg^"("^(String.concat "," invars_name)^");\n" in
    let prg = prg^"  ("^(String.concat "," locals_name)^") = P2" in
    let prg = prg^"("^(String.concat "," invars_name)^");\n  "^ok^" = (" in
    let (var_to_equals : Soc.var -> Soc.var -> string list) =
      fun (x,t) (y,_) -> 
        let rec aux x y t =
          match t with 
            | Data.Real -> ["r_equal("^x^","^y^")"]
            | Data.Array(t,n) -> 
              let res = ref [] in
              for i = 0 to n-1 do
                let istr = string_of_int i in
                res := !res @ aux (x^"["^istr^"]") (y^"["^istr^"]") t
              done;
              !res
            | Data.Struct(_n,fl) ->
              let do_field (fn,ft) = aux (x^"."^fn) (y^"."^fn) ft in
              List.flatten (List.map do_field fl)
            | _ ->  [x^"="^y]
        in
        aux x y t
    in
    let prg = try prg^(String.concat " and\n \t\t  " 
                       (List.flatten (List.map2 var_to_equals locals outvars))) ^");\ntel;\n"
      with _ -> assert false
    in
    let prg = prg ^ "
node r_abs(x:real) returns (res:real);
let
  res = if x < 0.0 then -x else x;
tel

const seuil = 0.01;
node r_equal(x,y:real) returns (res:bool);
let
   res = if r_abs(x)>1.0 then r_abs(1.0-(y/x)) < seuil else r_abs(x-y) < seuil;
tel
" in 
    Lv6util.entete oc "--" "";
    output_string oc prg;
    flush oc; 
    close_out oc;
    output_string stdout (oracle_file_name ^" generated.\n"); 
    flush stdout

let profile_info = Lv6Verbose.profile_info

let main () = (
  (* Lv6Compile.init_appli () ; *)
  (* parse_args (); *)
  let opt = Lv6MainArgs.parse Sys.argv in
  Lv6Verbose.exe ~level:3 (fun () ->
    Gc.set { (Gc.get ()) with Gc.verbose = 0x01 }
  );
  if opt.run_unit_test  then (UnifyType.unit_test (); my_exit opt 0);
  if (opt.infiles = []) then (Lv6MainArgs.usage stderr opt; my_exit opt 1);
  let new_dft_pack = Filename.basename (Filename.chop_extension (List.hd opt.infiles)) in
  Lv6Id.set_dft_pack_name new_dft_pack;

  let main_node = 
    if opt.main_node = "" then None else Some (Lv6Id.idref_of_string opt.main_node)
  in
  if opt.outfile <> "" then opt.oc <- open_out opt.outfile; 
  (try (
    let nsl = Lv6Compile.get_source_list opt opt.infiles in
    let lic_prg = Lv6Compile.doit opt nsl main_node in
    if opt.print_interface then (
      gen_rif_interface lic_prg main_node opt;
      profile_info "bye!";
      my_exit opt 0
    );
    if global_opt.Lv6MainArgs.gen_autotest then 
      gen_autotest_files lic_prg main_node opt
    else if opt.gen_ocaml then
      GenOcamlGlue.f Sys.argv opt
    else if (opt.exec || opt.gen_c) then
      (match main_node with
        | None -> (
          let name = find_a_node opt in
          let nk = (Lic.node_key_of_idref (Lv6Id.to_idref name)) in
          if LicPrg.node_exists lic_prg nk then (
            print_string ("WARNING: No main node is specified: will use "^name^"\n");
            flush stdout;
            profile_info "Start compiling to soc...\n";
            let msk,zesoc  = Lic2soc.f lic_prg nk in
            profile_info "Soc Compilation done.\n";
            if opt.gen_c then (
              profile_info "Start generating C code...\n";
              Soc2c.f opt msk zesoc lic_prg);
            if opt.exec then (
              profile_info "Start interpreting soc...\n";
              SocExec.f opt zesoc msk)          
          ) else (
            print_string ("Error: no node is specified, cannot exec.\nbye\n");
            flush stdout;
            my_exit opt 1
          )
        )
        | Some main_node -> 
          profile_info "Start compiling to soc...\n";
          let msk, zesoc = Lic2soc.f lic_prg (Lic.node_key_of_idref main_node) in

          profile_info "Soc Compilation done. \n";
          if opt.gen_c then (
            profile_info "Start generating C code...\n";
            Soc2c.f opt msk zesoc lic_prg);

          if opt.exec then (
            profile_info "Start interpreting soc...\n";
            SocExec.f opt zesoc msk)
      ) else if (
      opt.gen_lic || global_opt.ec || global_opt.lv4 || global_opt.kcg
    ) then (
      LicPrg.to_file opt lic_prg main_node
    ) else (
      Printf.printf "\
This program is syntactically correct. If you want to do something with it, you
could try one the options: -2c, -2c-exec, -exec, -ec, -lic, or -h for more options.
"
    )
    ;

    Lv6Verbose.exe ~level:3 (fun () -> Gc.print_stat stdout);
   ) with 
      Sys_error(s) -> prerr_string (s^"\n");  my_exit opt 1
    | Global_error s -> print_global_error s; my_exit opt 1
    | Parse_error ->
       print_compile_error (Lxm.last_made ()) "Syntax error"; my_exit opt 1
    | Unknown_var(lxm,id) -> 
      print_runtime_error lxm ("unknown variable (" ^ (Lv6Id.to_string id) ^")");
      my_exit opt 1
    | Unknown_constant(lxm,str) -> 
      print_runtime_error lxm ("unknown constant (" ^ str ^")");
      my_exit opt 1
    | Compile_error(lxm,msg) -> print_runtime_error lxm msg;  my_exit opt 1
    | L2lCheckLoops.Error(lxm,msg,lic_prg) -> 
      (* Sometime it helps to see the current state of the faulty program *)
      LicPrg.to_file opt lic_prg main_node;
      flush  opt.oc;
      print_compile_error lxm msg;
      my_exit opt 1
    | SocExec.AssertViolation lxm ->
       print_runtime_error lxm "An assertion is violated in the Lustre program";
       my_exit opt 1
    | Failure msg ->
       Printf.eprintf "\nError: %s\n" msg;
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
  );
  (* | Compile_node_error(nkey,lxm,msg) -> ( *)
  (* print_compile_node_error nkey lxm msg ; *)
  (* exit 1 *)
  (* ) *)
  (* | Global_node_error(nkey,msg) -> ( *)
  (* print_global_node_error nkey msg ; *)
  (* exit 1 *)
  (* ) *)

  if opt.outfile <> "" then close_out opt.oc
    
);;
main();
