(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: luc2c.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
**
** Generates C files to call Lucky, Lutin, or Lurette from C.
**

*)

(* open LucProg *)


type gen_mode = Lustre | Scade | Alices | Luciole | Nop
type step_mode = Inside | Edges | Vertices


let step_mode_to_str = function 
    Inside  -> "step_inside"
  | Edges  -> "step_edges"
  | Vertices -> "step_vertices"

type optionT = {
  mutable env : string list;
  mutable main_node : string;   (* Main node *)
  mutable boot : bool;
  mutable load_mem : bool;
  mutable pp : string option;     (* Pre-processor *)
  mutable output : string option;
  mutable rif : string option;
  mutable calling_module_name : string;
  mutable gen_mode : gen_mode;
  mutable step_mode : step_mode;
  mutable seed : int option;
  mutable precision : int option;
  mutable use_sockets : bool;
  mutable sock_addr : string;
  mutable sock_port : int;
  mutable output_dir : string;
  mutable oracle_ec : string option;
}


let (option : optionT) = {
  env = [];
  main_node = "";
  pp = None;
  boot = false;
  load_mem = false;
  output = None;
  rif = None;
  gen_mode = Nop;
  use_sockets = false;
  calling_module_name = "XXX_SCADE_MODULE_NAME";
  step_mode = Inside;
  seed = None;
  precision = None;
  sock_addr = "127.0.0.1";
  sock_port = 2042;
  output_dir = Filename.current_dir_name;
  oracle_ec = None
}


(****************************************************************************)
let my_open_out fn =
  prerr_string ("Generating file " ^ fn ^ "\n"); 
  open_out fn 


let (gen_lustre_ext_h : string -> unit) =
  fun _fn  -> 
    let fn = Filename.concat option.output_dir (option.calling_module_name ^ "_ext.h") in
    let oc = my_open_out fn in
    let putln s = output_string oc (s^"\n") in
      putln (Util.entete "// " "");
      putln ("#include \"" ^ option.calling_module_name ^ "_ext_func.h\"");
      flush oc;
      close_out oc
             
let (gen_lustre_ext_func_h : string -> Exp.var list -> Exp.var list -> unit) =
  fun fn in_vars out_vars -> 
    let oc = my_open_out (Filename.concat option.output_dir (option.calling_module_name ^ "_ext_func.h")) in
    let put s = output_string oc s in
    let putln s = output_string oc (s^"\n") in
    let rec putlist = function
        [] -> ()
      | [x] -> put x
      | x::l' -> put x; put ", "; putlist l'
    in 
      putln (Util.entete "// " "");
      putln  ("#include \"" ^ option.calling_module_name ^ ".h\"");
      putln  ("#include \"" ^ fn ^ ".h\"");
      putln "#define bool int";
      putln "";

      putln ("// A C function that implements "^fn^" which profile is the one ");
      putln "// expected when calling an external C function from Lustre.";
      put ("extern void " ^ fn ^ "(");
      putln "";
      let l1 = List.fold_left
        (fun acc var -> acc @ [(Type.to_cstring (Var.typ var)) ^ "*"])
        []
        out_vars
      in 
      let l2 = List.fold_left
        (fun acc var -> acc @ [Type.to_cstring (Var.typ var)])
        l1
        in_vars
      in
        putlist l2;
        putln ");";

        flush oc;
        close_out oc
             
let (gen_lustre_ext_func_c : string -> Exp.var list -> Exp.var list -> unit) =
  fun fn in_vars out_vars -> 
    let oc = my_open_out (Filename.concat option.output_dir (option.calling_module_name ^ "_ext_func.c")) in
    let put s = output_string oc s in
    let putln s = output_string oc (s^"\n") in
    let rec putlist = function
        [] -> ()
      |        [x] -> put x
      | x::l' -> put x; put ", "; putlist l'
    in 
      putln (Util.entete "// " "");
      putln  ("#include \"" ^ option.calling_module_name ^ "_ext_func.h\"");
      putln "";

      putln "// Output procedures";
      List.iter
        (fun var -> 
           put ("void " ^ fn ^ "_O_" ^ (Var.name var) ^ "(" ^
                  fn  ^"_ctx* lucky_ctx, ");
           put ((Type.to_cstring (Var.typ var)) ^ " v) { lucky_ctx->_");
           putln ((Var.name var) ^ " = v; };");
        )
        out_vars;
      
      putln "";
      putln "#define false 0";
      putln "#define true 1";

      putln (fn ^ "_ctx* lucky_ctx;");
      putln "int not_init = true;";
      putln "";
      put ("void " ^ fn ^ "(");
      let l1 = List.fold_left
        (fun acc var -> 
           acc @ [(Type.to_cstring (Var.typ var)) ^ "* " ^ (Var.name var)])
        []
        out_vars
      in 
      let l2 = List.fold_left
        (fun acc var -> acc @ [Type.to_cstring (Var.typ var)^" "^(Var.name var)])
        l1
        in_vars
      in
        putlist l2;
        putln "){";
        
        putln "";
        putln ("  if (not_init) {
    lucky_ctx = "^fn^"_new_ctx(NULL);
    not_init = false;");
        (match option.seed with
            None -> ()
          | Some i -> putln ("    lucky_set_seed(" ^ (string_of_int i) ^ ");");
        );
        putln "  };";
        List.iter
          (fun var -> 
             put ("  "^fn ^ "_I_" ^ (Var.name var) ^ "(lucky_ctx, ");
             putln ((Var.name var) ^ ");")
          )
          in_vars;
      
        putln ("  "^fn^ "_step(lucky_ctx, "^ (step_mode_to_str option.step_mode)^ ");");
        List.iter
          (fun var -> 
             putln ("  *" ^ (Var.name var) ^ "=lucky_ctx->_" ^(Var.name var) ^ ";");
          )
          out_vars;
        putln "}";
        putln "";
        
        
        flush oc;
        close_out oc




(****************************************************************************)
let (gen_h_file : string -> Exp.var list -> Exp.var list -> Exp.var list -> unit) =
  fun fn in_vars out_vars _loc_vars ->
    let oc = my_open_out (Filename.concat option.output_dir (fn ^ ".h")) in
    let put s = output_string oc s in
    let putln s = output_string oc (s^"\n") in
    let max_buff_size =
      let in_nb = List.length in_vars in
      let out_nb = List.length out_vars in
        (* 10 digits per I/O is enough? *)
      let io_size = 100 in
        max 2048 (2 lsl (int_of_float (log (float_of_int (20+io_size*(in_nb+out_nb))) /. (log 2.))))
    in
    let put_var_decl_in_struct var =
      put "  ";
      put (Type.to_cstring (Var.typ var));
      put " _";
      put (Var.name var);
      putln ";";
    in
      
      putln (Util.entete "// " "");
      
      putln  ("#ifndef _" ^ fn ^ "_H_INCLUDED \n");
      putln  ("#define _" ^ fn ^ "_H_INCLUDED \n");
      if not option.use_sockets then
        putln  "#include <lut4c_stubs.h> \n";
      putln "#include <stdio.h>";

      putln "//-------- Predefined types ---------";
      putln "#ifndef _EC2C_PREDEF_TYPES
#define _EC2C_PREDEF_TYPES
typedef int _bool;
typedef int _boolean;
typedef int _int;
typedef int _integer;
typedef char* _string;
typedef double _real;
typedef double _double;
typedef float _float;
#define _false 0
#define _true 1
#endif
";
      if option.use_sockets then (
        putln ("#define MAX_BUFF_SIZE "^(string_of_int max_buff_size)^"");
      );
      putln "//--------- Pragmas ----------------";
      putln ("//MODULE: " ^ fn ^ " " ^ (string_of_int (List.length in_vars)) 
             ^ " " ^  (string_of_int (List.length out_vars)));
      List.iter
        (fun var -> 
           putln ("//IN: " ^ (Type.to_cstring (Var.typ var)) ^ " " ^(Var.name var))
        )
        in_vars;
      List.iter
        (fun var -> 
           putln ("//OUT: " ^ (Type.to_cstring (Var.typ var)) ^ " " ^(Var.name var))
        )
        out_vars;


      putln ("\n//--------Context type ------------");
      putln "/*--------";
      putln "Internal structure for the call";
      putln "--------*/";


      putln "";
      if option.gen_mode = Scade then 
        (

          putln ("/* \n// Put the following in the scade generated file " ^ fn ^
                   "_fctext.h\n// (#included hereafter), " ^
                   "before the definition of the structure \n// _C_"  
                 ^ fn ^ ".");
          putln ("// Also, add the following field to the structure _C_"  ^ 
                   fn ^ ":");
          putln ("// ==>   " ^ fn ^ "_ctx * scade_ctx;\n");
        );
      putln ("struct C_" ^ fn ^ ";");
      putln ("struct _" ^ fn ^ "_ctx");
      putln " {";
      put "
  void* client_data;
  int step_number;  
  //INPUTS
";
      List.iter put_var_decl_in_struct in_vars;
      putln "  //OUTPUTS";
      List.iter put_var_decl_in_struct out_vars;

      putln "  ";
      put "  // lucky process number";
      put "  
#ifndef _WIN32
   int lp;
#endif
";
      if option.use_sockets then (
        putln "  char buff [MAX_BUFF_SIZE];";
        putln "  int sock;";
      );
      if option.gen_mode = Scade then (
        putln "  // Scade node context";
        putln ("   struct C_" ^ option.calling_module_name ^ " * client_data;");
        putln "};\n";
        putln ("typedef struct _" ^ fn ^ "_ctx " ^ fn ^ "_ctx;\n");
        putln "*/\n";
        putln ("#include \""^ fn ^"_fctext.h\"\n\n");
      )
      else
        (
          putln "};\n";
          putln ("typedef struct _" ^ fn ^ "_ctx " ^ fn ^ "_ctx;\n");
        );

      putln "FILE* fp;";
      putln "// To be defined by users";
      List.iter
        (fun var -> 
           put ("extern void " ^ fn ^ "_O_" ^ (Var.name var) ^ "(" ^
                  fn  ^"_ctx* ctx, ");
           put ((Type.to_cstring (Var.typ var)) ^ " f);\n");
        )
        out_vars;

      putln "\n//--------Context allocation --------";
      putln ("extern "^fn^"_ctx* "^fn^"_new_ctx(void* client_data);");


      putln ("\n//-------Context copy --------------");
      putln ("extern void "^fn^"_copy_ctx("^fn^"_ctx* dest, "^ fn ^
               "_ctx* src);");

      if option.use_sockets then (
        putln "\n//--------Terminate procedure -----------";
        putln ("extern void "^fn^"_terminate("^fn^"_ctx* ctx);");
      );
      putln "\n//--------Reset procedure -----------";
      putln ("extern void "^fn^"_reset("^fn^"_ctx* ctx);");


      putln ("\n//-------Step procedure -----------");

      if option.use_sockets then
        putln ("extern void "^fn^"_step("^fn^"_ctx* ctx);")
      else
        putln ("extern void "^fn^"_step("^fn^"_ctx* ctx, step_mode sm);");

      putln ("\n//-------Input procedures ---------");
      List.iter
        (fun var -> 
           put ("extern void " ^ fn ^ "_I_" ^ (Var.name var) ^ 
                  "("^fn^"_ctx*, ");
           put ((Type.to_cstring (Var.typ var)) ^ ");\n");
        )
        in_vars;

      putln  "#endif \n";
      
      flush oc;
      close_out oc

let var_to_rif_type var =
  match Var.typ var with
    | Type.BoolT -> "bool"
    | Type.IntT -> "int"
    | Type.FloatT -> "real"
    | Type.UT _ -> assert false

let var_to_format_print var =
  match Var.typ var with
    | Type.BoolT -> "%d"
    | Type.IntT -> "%d"
    | Type.FloatT -> "%lf" (* XXX ??? *)
    | Type.UT _ -> assert false

let var_to_format_scan var =
  match Var.typ var with
    | Type.BoolT -> "%c"
    | Type.IntT -> "%d"
    | Type.FloatT -> "%lf"  (* XXX ??? *)
    | Type.UT _ -> assert false


let put_socket_func put fn in_vars out_vars _loc_vars =
  let putln str = put (str^"\n") in
    put ("
#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <locale.h>
#ifdef _WINSOCK
  #include <windows.h>
  #include <process.h>
  #pragma comment(lib, \"Ws2_32.lib\")
#else
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netdb.h>
#endif
" ^ (if option.gen_mode = Alices then
       "#include \"Ims/ImsMessageStack.h\""
     else "")
         ^
"
#ifdef _DEBUG
#define dbg_printf(...) fprintf(fp, __VA_ARGS__); fflush(fp)
#else
#define dbg_printf(...)  while(0)
#endif
" ^ (if option.gen_mode <> Alices then
       "
void raise_error(const char* msg)
{
   printf(msg);
   exit(2);
}
"
     else "
void raise_info(const char* msg)
{
  ImsMessageStack xMessageStack ;  
  xMessageStack.InsertErrorMessage(\"0\",  \"Err\" , cImsInfoAlone, IMS_DEBUG_INFO(Null))
    <<  \"Lurette/Alices interface: %0\"
    <<  msg ;
  throw xMessageStack ; 
} 

void raise_warning(const char* msg)
{
  ImsMessageStack xMessageStack ;  
  xMessageStack.InsertErrorMessage(\"0\",  \"Err\" , cImsWarning, IMS_DEBUG_INFO(Null))
    <<  \"Lurette/Alices interface: %0\"
    <<  msg ;
  throw xMessageStack ; 
} 


void raise_error(const char* msg)
{
  ImsMessageStack xMessageStack ;  
  xMessageStack.InsertErrorMessage(\"0\",  \"Err\" , cImsError, IMS_DEBUG_INFO(Null))
    <<  \"Lurette/Alices interface: %0\"
    <<  msg ;
  throw xMessageStack ; 
} 

void raise_fatal_error(const char* msg)
{
  ImsMessageStack xMessageStack ;  
  xMessageStack.InsertErrorMessage(\"0\",  \"Err\"  , cImsFatal, IMS_DEBUG_INFO(Null))
    <<  \"Lurette/Alices interface: %0\"
    <<  msg ;
  throw xMessageStack ; 
}")
         ^
" 

/*--------
Output procedures must be defined,
Input procedures must be used:
--------*/
");       
    List.iter
      (fun var -> 
         put ("void " ^ fn ^ "_I_" ^ (Var.name var) ^ 
                "(" ^ fn ^ "_ctx* ctx, ");
         put ((Type.to_cstring (Var.typ var)) ^ " _" ^ (Var.name var)^ "){\n");
         putln ("  ctx->_" ^ (Var.name var) ^ " = _" ^ (Var.name var)^ ";");
         putln "}\n";
      )
      in_vars;
    
    let _lut_file = (List.hd option.env) (* only work with lutin XXX fixme? *) in
(*     let lut_dir = Filename.dirname lut_file in *)
    putln ("
/*--------
launch the lutin interpreter and init socket stuff
--------*/
" ^ fn ^ "_ctx* " ^ fn ^ "_new_ctx(void* cdata){
  " ^ fn ^ "_ctx* ctx;     
  int sockfd;
  int newsockfd;
  int portno;
  int clilen;
#ifndef _WIN32
  int lutin_pid;
#endif
  struct sockaddr_in serv_addr;
  struct sockaddr_in cli_addr;
  char portno_str[10];
  char *sock_addr = \""^option.sock_addr^"\";

  // Socket administration stuff   
#ifdef _WINSOCK
  WSADATA WSAData;
  WSAStartup(MAKEWORD(2,0), &WSAData);
#endif

  dbg_printf(\"malloc\");
  ctx = ("^ fn ^ "_ctx*) malloc(sizeof("^ fn ^ "_ctx));
  ctx->client_data = cdata;
  ctx->step_number = 0;

  dbg_printf(\"setlocale\\n\");
  setlocale(LC_ALL, \"English\");

  dbg_printf(\"opening socket\\n\");
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) printf(\"Error: opening socket\");
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = inet_addr(sock_addr);

  portno = "^(string_of_int option.sock_port)^";
  dbg_printf(\"Try Binding  %s:%d...\\n\",sock_addr,portno);
  serv_addr.sin_port = htons(portno);
  while (bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) ) {
    portno++;
    serv_addr.sin_port = htons(portno);
    if (portno > 2000+"^(string_of_int option.sock_port)^") { 
        printf(\"" ^ fn ^ ": cannot find a free socket port\");
    }
  };
  
  dbg_printf(\"Binding  %s:%d...\\n\",sock_addr,portno);
  sprintf(portno_str, \"%d\", portno);
  printf(\" Waiting for someone to connect on %s:%s\\n\", sock_addr, portno_str);
  //raise_info(msg);
  dbg_printf(\"Waiting for lurette to connect\\n\");

  dbg_printf(\"Listening...\\n\");
  listen(sockfd,5);
  clilen = sizeof(cli_addr);
  dbg_printf(\"Accepting...\\n\");
  newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr, &clilen);
  if (newsockfd < 0) printf(\"Error:  on accept\");
  ctx->sock = newsockfd;

  memset(ctx->buff, 0, MAX_BUFF_SIZE);
  return ctx;
}

/*--------
Step procedure
--------*/

void " ^ fn ^ "_step(" ^ fn ^ "_ctx* ctx){
  int rc;
" ^
let cpt = ref 0 in
let decl_char acc var =
  if (Var.typ var= Type.BoolT) then (
    incr cpt; 
    ("  char c"^(string_of_int !cpt)^";\n")::acc)
  else
    acc
in
let char_decl =
  (String.concat "" (List.rev (List.fold_left decl_char [] out_vars)))
in
let var_to_adress var ="ctx->_" ^ (Var.name var) in
  (if !cpt = 0 then "" else char_decl) ^ 
  (if option.gen_mode <> Alices then "" else   
"  if (ctx->step_number == 0) {
    sprintf(ctx->buff, \"@#inputs "^(
    List.fold_left
      (fun acc var -> acc^"\\\""^ (Var.name  var)^"\\\":"^(var_to_rif_type var)^" \"\n       \" \\n")
      ""
      out_vars) ^"#@\\n\");
    dbg_printf(\"\\n\\n ---- Sending the SUT input decl: '%s'\\\\n\",ctx->buff);
    send(ctx->sock, ctx->buff, (int) strlen(ctx->buff),0); 

    sprintf(ctx->buff, \"@#outputs "^(
    List.fold_left
      (fun acc var -> acc ^  "\\\""^ (Var.name  var)^"\\\":"^(var_to_rif_type var)^" \"\n       \"  \\n")
      ""
      in_vars) ^"#@\\n\");
    dbg_printf(\"\\n\\n ---- Sending the SUT output decl: '%s'\\\\n\",ctx->buff);
    send(ctx->sock, ctx->buff, (int) strlen(ctx->buff),0);

    sprintf(ctx->buff, \""^(
    List.fold_left
      (fun acc var -> acc ^ (var_to_format_print var) ^ " ")
      ""
      in_vars) ^"\\n\", "
  ^
    (List.fold_left
       (fun acc var -> acc ^ ", "^ (var_to_adress var))
       (var_to_adress (List.hd in_vars))
       ((List.tl in_vars)))
     ^
    ");
    dbg_printf(\"\\n\\n ---- Sending the 'cliché' (SUT output part): '%s'\\n\",ctx->buff);
    send(ctx->sock, ctx->buff, (int) strlen(ctx->buff),0);

    sprintf(ctx->buff, \""^(
    List.fold_left
      (fun acc var -> acc ^ (var_to_format_print var) ^ " ")
      ""
      out_vars) ^"\\n\", "
  ^
    (List.fold_left
       (fun acc var -> acc ^ ", "^ (var_to_adress var))
       (var_to_adress (List.hd out_vars))
       ((List.tl out_vars)))
     ^
    ");
    dbg_printf(\"\\n\\n ---- Sending the 'cliché' (SUT input part): '%s'\\n\",ctx->buff);
    send(ctx->sock, ctx->buff, (int) strlen(ctx->buff),0);
   }; 
  dbg_printf(\"\\n\\n ---- A new Step begins. Sending to sock: '%s'\\n\",ctx->buff);

") ^
    

"
  dbg_printf(\"reading inputs\\n\");

  rc = 0;
  rc = recv(ctx->sock, ctx->buff, MAX_BUFF_SIZE, 0);
  if (rc<0)  { 
    printf(\"Error: cannot read on socket\\n\"); 
    raise_error(\"" ^ fn ^ ": cannot read on socket\");
   };
  if (strncmp(ctx->buff, \"quit\", 4) == 0) {
    "^fn^"_terminate(ctx); 
    raise_error(ctx->buff);
  };
  dbg_printf(\"reading '%s'\\n\",ctx->buff);
  sscanf(ctx->buff, \"" ^ 
        let cpt = ref 0 in
        let var_to_adress_scan var =
          if (Var.typ var= Type.BoolT) then (
            incr cpt; "&c"^(string_of_int !cpt))
          else
            "&(ctx->_"^ (Var.name var)^")"
        in
        let scan_format = 
          (List.fold_left
             (fun acc var -> acc ^ " " ^ (var_to_format_scan var))
             ""
             out_vars)
        in
          scan_format
          ^ "\"," 
          ^
            (List.fold_left
               (fun acc var -> acc^", "^(var_to_adress_scan var))
               (var_to_adress_scan (List.hd out_vars))
               (List.tl out_vars)) ^ ");
" ^
            let cpt = ref 0 in
            let copy_char_to_ctx acc var =
              if (Var.typ var= Type.BoolT) then (
                incr cpt; 
                let c = ("c"^(string_of_int !cpt)) in
                let conv_char = 
                  "   if (("^c^" == '0') || ("^c^" == 'f') || ("^c^" == 'F')) ctx->_"
                  ^(Var.name var)^" = _false;
   if (("^c^" == '1') || ("^c^" == 't') || ("^c^" == 'T')) ctx->_"
                  ^(Var.name var)^" = _true;
"
                in
                  conv_char::acc)
              else
                acc
            in
              (String.concat ""
                 (List.rev (List.fold_left copy_char_to_ctx [] out_vars)))
              ^ 
(if in_vars = [] then "" else 
    "
   sprintf(ctx->buff, \""^(
    List.fold_left
      (fun acc var -> acc ^ (var_to_format_print var) ^ " ")
      ""
      in_vars) ^"\\n\", "
  ^
    (List.fold_left
         (fun acc var -> acc ^ ", "^ (var_to_adress var))
         (var_to_adress (List.hd in_vars))
         ((List.tl in_vars)))
      ^
        ");
  send(ctx->sock, ctx->buff, (int) strlen(ctx->buff),0);
  memset(ctx->buff, 0, rc);
  ctx->step_number = 1+ctx->step_number;
  dbg_printf(\"----- step done\\n\");
}

/*--------
Terminate procedure
--------*/

void " ^ fn ^ "_terminate(" ^ fn ^ "_ctx* ctx){
  send(ctx->sock, \"q\\n\", 2, 0);
#ifdef _WINSOCK
  closesocket(ctx->sock);
  WSACleanup();
#else
  close(ctx->sock);
  kill(ctx->lp, SIGKILL); // parano
#endif
}

/*--------
Reset procedure
--------*/

void " ^ fn ^ "_reset(" ^ fn ^ "_ctx* ctx){
 if (ctx->step_number > 0) { // no reset before the first step (luciole does that)
  " ^ fn ^ "_terminate(ctx);
  ctx = " ^ fn ^ "_new_ctx(ctx->client_data);
}}"))


let put_call_luc4c_make oc =
  let put s = output_string oc s in
  let putln s = output_string oc (s^"\n") in
    if Util.get_extension (List.hd option.env) = ".lut" then (
      put  ("   ctx->lp = make_lutin(\"" );
      put (List.hd option.env);
      put ("\", \"");
      put option.main_node;
      putln "\");"
        
    ) else (
      
      put  ("  ctx->lp = make(" );
      put (match option.pp with | None -> "\"\"" | Some pp -> ("\""^pp^"\""));
      put (", \"");
      assert (option.env <> []);
      put (List.hd option.env);
      List.iter (fun x -> put (" " ^ x)) (List.tl option.env);
      put ("::"^option.main_node);
      putln "\");"
    )




let (gen_c_file : string -> Exp.var list -> Exp.var list -> Exp.var list -> unit) =
  fun fn in_vars out_vars loc_vars -> 
    let oc = 
      match option.gen_mode with
          Scade -> 
            my_open_out (Filename.concat option.output_dir (fn ^ "_fctext.c"))
        | Alices -> 
            my_open_out (Filename.concat option.output_dir (fn ^ ".cpp"))
        | _  ->  
            my_open_out (Filename.concat option.output_dir (fn ^ ".c")) 
    in
    let put s = output_string oc s in
    let putln s = output_string oc (s^"\n") in
      (*     let in_out_vars = in_vars @ out_vars in *)

      putln (Util.entete "// " "");
      put (( if option.gen_mode = Scade then
                 "#include \"" ^ option.calling_module_name ^ ".h\" \n" 
               else
                 "#include \"" ^ fn ^ ".h\" \n" 
             ) ^
             "");

      if option.use_sockets then (
        put "
#include <stdlib.h>
#include <string.h>
";
        put_socket_func put fn in_vars out_vars loc_vars;
      )
      else (
        if option.gen_mode = Scade then 
          begin
            (*           putln ("static " ^ fn ^ "_ctx* ctx;\n");       *)
            putln "
/*------
Output procedures
--------*/
";        
            List.iter
              (fun var -> 
                 put ("void " ^ fn ^ "_O_" ^ (Var.name var));
                 put ("("^ fn ^ "_ctx* ctx, " ^ (Type.to_cstring (Var.typ var)));
                 putln (" "^(Var.name var) ^ ") {");
                 put ("  ctx->client_data->_<SCADE_VAR_NAME_OF_" ^ (Var.name var));
                 putln ("> = " ^ (Var.name var) ^ ";");
                 putln "  return;";
                 putln "}\n";
              )
              out_vars;
            putln "/*------
Input procedures
--------*/
";        
          end
        else 
          begin
            putln "
/*--------
Output procedures must be defined,
Input procedures must be used:
--------*/

";
          end;
        
        List.iter
          (fun var -> 
             put ("void " ^ fn ^ "_I_" ^ (Var.name var) ^ 
                    "(" ^ fn ^ "_ctx* ctx, ");
             put ((Type.to_cstring (Var.typ var)) ^ " _" ^ (Var.name var)^ "){\n");
             putln ("  ctx->_" ^ (Var.name var) ^ " = _" ^ (Var.name var)^ ";");
             putln "}\n";
          )
          in_vars;
        
        putln ("/*--------
Copy the value of an internal structure
--------*/
void "^ fn ^"_copy_ctx("^ fn ^"_ctx* dest, "^ fn ^"_ctx* src){
  memcpy((void*)dest, (void*)src, sizeof("^ fn ^"_ctx));
}");

      putln "
/*--------
Dynamic allocation of an internal structure
--------*/";
      putln (fn ^ "_ctx* " ^ fn ^ "_new_ctx(void* cdata){");
      putln ("  " ^ fn ^ "_ctx* ctx;");
      putln "";
      putln "  lucky_caml_init();";
      (match option.seed with
         | None -> ()
         | Some seed -> 
             putln ("  set_seed("^ (string_of_int seed) ^");")
      );
      putln ("  ctx = malloc(sizeof("^ fn ^ "_ctx));");
      putln "  ctx->client_data = cdata;";
      putln "  ctx->step_number = 0;";
      put_call_luc4c_make oc;      
      putln "  return ctx;";
      putln "}";

      putln "
/*--------
Step procedure
--------*/";

      put ("void " ^ fn ^ "_step(" ^ fn ^ "_ctx* ctx, step_mode sm){\n");
      
      List.iter
        (fun var ->
           putln ("  lucky_set_input_" ^
                    (Type.to_string (Var.typ var)) ^ "(ctx->lp, \"" ^
                    (Var.name var) ^ "\", ctx->_" ^
                    (Var.name var) ^");");
        )
        in_vars;
      
      putln "  lucky_step(ctx->lp, sm);";

      List.iter
        (fun var -> 
           putln ("  " ^ fn ^ "_O_" ^ (Var.name var) ^ 
                    "(ctx, lucky_get_output_" ^
                    (Type.to_string (Var.typ var)) ^ "(ctx->lp, \"" ^
                    (Var.name var) ^ "\"));");
        )
        out_vars;
      putln "
  ctx->step_number = 1+ctx->step_number;
}
";

      putln "
/*--------
Reset procedure
--------*/";
      put ("void " ^ fn ^ "_reset(" ^ fn ^ "_ctx* ctx){\n");
      put "if (ctx->step_number > 0) { // no reset before the first step (luciole does that)";
      put_call_luc4c_make oc;
      putln "  }";
      
      putln "}";

      if option.gen_mode = Scade then (
        putln "

/*------
This is the main function to be called by Scade
--------*/";

        putln ("void " ^ option.calling_module_name ^ "_init(_C_" ^ 
                 option.calling_module_name ^ " * _C_)");
        putln "{";
        putln ("  _C_->lucky_ctx = " ^ fn ^ "_new_ctx(_C_);");
        putln "}";

        put ("void " ^ option.calling_module_name ^ "(_C_" ^ 
               option.calling_module_name ^ " *_C_){");

        putln "\n  // sets the inputs";
        List.iter 
          (fun var -> 
             put ("  " ^ fn ^ "_I_"^ (Var.name var) ^ "(_C_->lucky_ctx, ");
             putln ((Var.name var) ^ ");")
          )
          in_vars;
        
        putln "\n  // perform the step";
        putln ("  " ^ fn ^ "_step(_C_->lucky_ctx, step_inside);");
        
        putln "}"; 
      );
      );
      
      flush oc;
      close_out oc

(****************************************************************************)
(*
  Invent a file name from the list of file names, unless one was provided
  at the command line.
*)

let (build_file_name : string list -> string) =
  fun el ->
    let change_file_name f =
      (* make sure that there is no strange char in the name of the
         file as it will be used as a C ident.
         Therefore, we replace " " and "-" by "_".
         Is there any other problematic chars?
      *)
      let f0 = 
        try (Filename.basename (Filename.chop_extension f)) 
        with _ ->  Filename.basename f
      in
      let f1 = Str.global_replace (Str.regexp " ") "_" f0 in
      let f2 = Str.global_replace (Str.regexp "-") "_" f1 in
        f2
    in
      match option.output with
          None -> 
            let fn = 
              assert (el <> []);
              List.fold_left 
              (fun f acc -> (acc^"_"^(change_file_name f))) 
                (change_file_name (List.hd el))
                (List.tl el) 
            in
              option.output <- Some fn;
              fn
        | Some fn -> fn


(****************************************************************************)

let (main : Exp.var list -> Exp.var list -> Exp.var list -> unit) =
  fun ins outs locs ->
    let env_list = option.env in
    let _ =  assert (env_list <> []) in
    let fn = build_file_name env_list in
    let _ = 
      gen_c_file fn ins outs locs;
      gen_h_file fn ins outs locs
    in
    let gen_files_for_lustre () =
      gen_lustre_ext_func_c fn ins outs;
      gen_lustre_ext_func_h fn ins outs;
      gen_lustre_ext_h fn
    in
      (match option.gen_mode with
         | Nop -> ()
         | Lustre -> gen_files_for_lustre ()
         | Luciole ->
             if 
               (List.length option.env) > 1 &&
               (Util.get_extension (List.hd option.env) = ".lut") 
             then (
               flush stdout;
               flush stderr;
               print_string("*** For the time being, it is not possible to use luciole " ^
                              "with several Lutin files.\n*** Use the include statement instead.\n"^
                              "*** bye.\n");
               flush stdout;
               exit 2
             );  
             let var_to_vn_ct v = (Var.name v, Type.to_cstring (Var.typ v)) in
               Luciole.gen_stubs fn
                 (List.map var_to_vn_ct ins)
                 (List.map var_to_vn_ct outs)
         | Scade -> ()
         | Alices ->
             let alice_args = {
               Luc2alice.env_name = fn;
               Luc2alice.alice_module_name = option.calling_module_name;
               Luc2alice.seed = option.seed;
               Luc2alice.env_in_vars = ins ;
               Luc2alice.env_out_vars = outs;
               Luc2alice.use_sockets = option.use_sockets;
               Luc2alice.output_dir = option.output_dir;

             }
             in
               Luc2alice.gen_alice_stub_c alice_args;
               Luc2alice.gen_alice_stub_h alice_args;
      )


