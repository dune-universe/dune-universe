(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: luc2c.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
**
** Generates C files to call Lucky from Alice.
**
*)


(* put all args in one structure  *)
type alice_args = { 
  env_name : string;
  alice_module_name : string ;
  seed : int option;
  env_in_vars : Exp.var list;
  env_out_vars : Exp.var list;
  use_sockets: bool;
  output_dir:string;
}


(********************************************************************)

(* let (cast : Type.t -> string) = function *)
(*   | Type.BoolT -> "(_bool*)" *)
(*   | Type.IntT  -> "(_int*)" *)
(*   | Type.FloatT -> "(_real*)" *)
(*   | Type.UT _ -> assert false *)
(*  *)
(*  *)
(* let (declarer_importeurs : string -> Exp.var list -> string) = *)
(* fun an vars  ->  *)
(*   let make_one_decl var = *)
(*     let vn = Var.name var in *)
(*       ("    DeclarerImporteur(DaxFlags32( Nv3BaseDeModule::cEchangeSync ),\" "^ *)
(*          an^":"^vn^":\", "^(cast (Var.typ var))^"&mCtx->_"^vn^");\n") *)
(*   in *)
(*   let items = List.map make_one_decl vars in *)
(*      "" ^ (String.concat "" items) *)
(*  *)
(* let (declarer_exporteurs : string -> Exp.var list -> string) = *)
(*   fun an vars  ->  *)
(*     let make_one_decl var =  *)
(*       let vn = Var.name var in *)
(*         ("    DeclarerExporteur(DaxFlags32( Nv3BaseDeModule::cEchangeSync ),\" "^ *)
(*            an^":"^vn^":\", "^(cast (Var.typ var))^" &mCtx->_"^vn^");\n") *)
(*     in *)
(*     let items = List.map make_one_decl vars in *)
(*       "" ^ (String.concat "" items) *)
(********************************************************************)


let (lucky_seed : int option -> string) =
  function
    | Some i -> ("lucky_set_seed("^(string_of_int i)^");\n")
    | None -> ""

(********************************************************************)


let (define_output_proc : string -> Exp.var list -> string) =
  fun n vl ->
    let f var =
      let vn = Var.name var in
      let t = Type.to_cstring  (Var.typ var) in
      ("  void  "^n^"_O_"^vn^"("^n^"_ctx* ctx, "^t^" value){ ctx->_"^vn^" = value; };\n")
    in
      (String.concat "" (List.map f vl))


(********************************************************************)
(*
** Variable names and types lists
*)

let soi = string_of_int

let (to_char: Type.t -> string) =
  function
  | Type.BoolT -> "_bool_type_char"
  | Type.IntT  -> "_int_type_char"
  | Type.FloatT -> "_real_type_char"
  | Type.UT _ -> assert false

let (gen_alice_var_tab : string -> string -> Exp.var list -> string) =
  fun _alice_name name vl ->
    let n = List.length vl in

    let f (i,acc) var = i+1,acc ^
      "  "^name^".tab[" ^ (soi i) ^ "].var_name = \"" ^ (Var.name var) ^ "\";\n" ^
      "  "^name^".tab[" ^ (soi i) ^ "].var_type = " ^ (to_char(Var.typ var)) ^ ";\n" ^
      "  "^name^".tab[" ^ (soi i) ^ "].var_adrs = &(mCtx->_" ^ (Var.name var)^ ");\n"
    in
      ("
  "^name^".size="^(soi n)^";
  "^name^".tab = new var_info["^(soi n)^"];\n" ^
         (snd (List.fold_left f (0,"") vl))) 


(********************************************************************)


let (gen_alice_stub : alice_args -> string) =
  fun args -> 
    let alice_full_name = args.alice_module_name 
    and alice_name = Filename.basename args.alice_module_name 
    and fn = args.env_name 
    and in_vars  = args.env_in_vars
    and out_vars = args.env_out_vars
    in

("
#include \""^alice_full_name^".h\"
#include \"float.h\"
#include \"Ims/ImsMessageStack.h\"

#ifdef _DEBUG
#define dbg(...) fprintf(fp, __VA_ARGS__); fflush(fp)
#else
#define dbg(...)  while(0)
#endif

int "^alice_name^"::instance_cpt=0;
structTab "^alice_name^"::inputs;
structTab "^alice_name^"::outputs;
structTab "^alice_name^"::memories;


/*  Build an instance of the Class "^alice_name^" */
"^alice_name^"::"^alice_name^"()
{  
  try 
    {
#ifdef _DEBUG
       fp = fopen(\""^fn^"-"^alice_name^".log\", \"w\");
#endif
       dbg(\"%s\\n\", \"--> "^alice_name^"\");

       instance_nb = instance_cpt++;
       mCtx = "^fn^"_new_ctx((void *) instance_nb);

      dbg(\"%s\\n\", \"<--  "^alice_name^"\");

    }
  catch (ImsMessageStack& xMessageStack)
    {
      xMessageStack.InsertErrorMessage(\"2\", \"Err\", cImsFatal, IMS_DEBUG_INFO("^
   alice_name^":Initialisation))
	<< \" Erreur lors de l'initialisation du contexte de "^fn^"\";
      throw xMessageStack;
    }

 // Inputs"
 ^ (gen_alice_var_tab alice_name "inputs" in_vars)  ^
"
   // Outputs"
 ^ (gen_alice_var_tab alice_name "outputs" out_vars) ^
"
   // Memories"
 ^ (gen_alice_var_tab alice_name "memories" []) ^
"
}

/* Remove an instance of the Class "^alice_name^" */
"^alice_name^"::~"^alice_name^"()
{
  "^fn^"_terminate(mCtx);
  delete[] inputs.tab;
  delete[] outputs.tab;
  delete[] memories.tab;
#ifdef _DEBUG
  fclose(fp);
#endif

}

/* Get a new object (for dynamic libs) */
"^alice_name^"* "^alice_name^"::Factory () 
{ 
  return new "^alice_name^"();
}

/* Initialisation */
void "^alice_name^"::Initialisation()
{
  //"^fn^"_reset_ctx(mCtx);" ^ (lucky_seed args.seed) ^ "
}

/* Step */
void "^alice_name^"::Process()
{
  dbg(\"%s\\n\", \"--> Process\");
  "^fn^"_step(mCtx" ^ (if args.use_sockets then "" else ", step_inside")^");
  dbg(\"%s\\n\", \"<-- Process\");
}

/* Terminate */
void "^alice_name^"::Terminate()
{
  "^fn^"_terminate(mCtx);
  // pourquoi ne pas faire 'delete toto' plutot que 'toto.Terminate()' ? 
  // Quand est appelé cette méthode finalement ?
}

/* Returns the Lucky input vars */
structTab* "^alice_name^"::Inputs() {
   { return  &inputs;};
}

/* Returns the Lucky output vars */
structTab* "^alice_name^"::Outputs() {
  { return  &outputs;};
}

/* Returns the Lucky output memories */
structTab* "^alice_name^"::Memories() {
   { return  &memories;};
}


")

let my_open_out fn =
  prerr_string ("Generating file " ^ fn ^ "\n"); 
  open_out fn 

let (gen_alice_stub_c : alice_args -> unit) =
  fun args -> 
    let amn = Filename.basename args.alice_module_name in
    let oc = my_open_out (Filename.concat args.output_dir (amn ^ ".cpp")) in
    let putln s = output_string oc (s^"\n") in
    putln (Util.entete "// " "");
    putln (gen_alice_stub args)


let (gen_alice_stub_h : alice_args -> unit) =
  fun args -> 
    let amn = Filename.basename args.alice_module_name in
    let oc = my_open_out (Filename.concat args.output_dir (amn ^ ".h")) in
    let amn = Filename.basename args.alice_module_name in
    let putln s = output_string oc (s^"\n") in
    let fn = args.env_name in
      putln (Util.entete "// " "");
      putln ("

#include \"AlicesCommon.h\"

#ifdef BUILD_"^amn^"
    #define "^amn^"_interface LINKER_EXPORTED
#else // BUILD_"^amn^"
    #define "^amn^"_interface LINKER_IMPORTED
#endif // BUILD_"^amn^"


#include \""^fn^".h\"

class "^amn^"_interface "^amn^";

#ifndef _real_type_char
#define _real_type_char 'd'
#endif
#ifndef _int_type_char
#define _int_type_char 'i'
#endif
#ifndef _bool_type_char
#define _bool_type_char 'b'
#endif

struct var_info {
  const char *var_name;
  char  var_type;
  void *var_adrs;
};

struct structTab {
  int size;
  var_info* tab;
};


class "^amn^"_interface "^amn^" {

  _"^fn^"_ctx * mCtx;
  static structTab inputs;
  static structTab outputs;
  static structTab memories;
  static int instance_cpt; 
  int instance_nb;

 public :      

  "^amn^"();
  ~"^amn^"();
  static "^amn^"*  Factory();
  void Initialisation();
  void Process();
  void	Terminate()   ;
  structTab* Inputs();
  structTab* Outputs();
  structTab* Memories();
};
")
    
