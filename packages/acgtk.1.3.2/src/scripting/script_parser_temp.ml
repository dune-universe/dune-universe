
# 21 "script_parser.dyp"
      
  open Environment
  open Script_lexer
  let id = fun x -> x
    
  let pr s = Printf.printf "%s\n%!" s

  type global_data_type = bool*bool*(string list)*(string option)

  module Parser =
  struct  
    module F=Functions.Functions
      
  
let _ = () (* dummy line to improve OCaml error location *)
# 19                 "script_parser_temp.ml"
let _ =
  if "20120619" <> Dyp.version
  then (Printf.fprintf stderr
    "version mismatch, dypgen version 20120619 and dyplib version %s\n" Dyp.version;
  exit 2)

module Dyp_symbols =
struct
  let get_token_name t = match t with
    | ADD _ -> 0
    | ADD_HELP -> 1
    | ANALYSE _ -> 2
    | ANALYSE_HELP -> 3
    | AS -> 4
    | CHECK _ -> 5
    | CHECK_HELP -> 6
    | COMPOSE -> 7
    | CREATE_HELP -> 8
    | CREATE_LEX -> 9
    | CREATE_SIG -> 10
    | DONT -> 11
    | EOII -> 12
    | EXIT -> 13
    | HELP -> 14
    | IDB _ -> 15
    | IDENTT _ -> 16
    | LIST -> 17
    | LOAD_DATA _ -> 18
    | LOAD_HELP -> 19
    | LOAD_OBJECT _ -> 20
    | LOAD_SCRIPT _ -> 21
    | PARSE _ -> 22
    | PARSE_HELP -> 23
    | PRINT _ -> 24
    | QUERY _ -> 25
    | QUERY_HELP -> 26
    | REALIZE _ -> 27
    | REALIZE_HELP -> 28
    | REALIZE_SHOW _ -> 29
    | REALIZE_SHOW_HELP -> 30
    | SAVE _ -> 31
    | SAVE_HELP -> 32
    | SELECT -> 33
    | SEMICOLONN _ -> 34
    | TRACE -> 35
    | UNSELECT -> 36
    | WAIT -> 37
  let str_token t = match t with
    | ADD _ -> "ADD"
    | ADD_HELP -> "ADD_HELP"
    | ANALYSE _ -> "ANALYSE"
    | ANALYSE_HELP -> "ANALYSE_HELP"
    | AS -> "AS"
    | CHECK _ -> "CHECK"
    | CHECK_HELP -> "CHECK_HELP"
    | COMPOSE -> "COMPOSE"
    | CREATE_HELP -> "CREATE_HELP"
    | CREATE_LEX -> "CREATE_LEX"
    | CREATE_SIG -> "CREATE_SIG"
    | DONT -> "DONT"
    | EOII -> "EOII"
    | EXIT -> "EXIT"
    | HELP -> "HELP"
    | IDB _ -> "IDB"
    | IDENTT _ -> "IDENTT"
    | LIST -> "LIST"
    | LOAD_DATA _ -> "LOAD_DATA"
    | LOAD_HELP -> "LOAD_HELP"
    | LOAD_OBJECT _ -> "LOAD_OBJECT"
    | LOAD_SCRIPT _ -> "LOAD_SCRIPT"
    | PARSE _ -> "PARSE"
    | PARSE_HELP -> "PARSE_HELP"
    | PRINT _ -> "PRINT"
    | QUERY _ -> "QUERY"
    | QUERY_HELP -> "QUERY_HELP"
    | REALIZE _ -> "REALIZE"
    | REALIZE_HELP -> "REALIZE_HELP"
    | REALIZE_SHOW _ -> "REALIZE_SHOW"
    | REALIZE_SHOW_HELP -> "REALIZE_SHOW_HELP"
    | SAVE _ -> "SAVE"
    | SAVE_HELP -> "SAVE_HELP"
    | SELECT -> "SELECT"
    | SEMICOLONN _ -> "SEMICOLONN"
    | TRACE -> "TRACE"
    | UNSELECT -> "UNSELECT"
    | WAIT -> "WAIT"
  let ter_string_list = [
      ("ADD",0);
      ("ADD_HELP",1);
      ("ANALYSE",2);
      ("ANALYSE_HELP",3);
      ("AS",4);
      ("CHECK",5);
      ("CHECK_HELP",6);
      ("COMPOSE",7);
      ("CREATE_HELP",8);
      ("CREATE_LEX",9);
      ("CREATE_SIG",10);
      ("DONT",11);
      ("EOII",12);
      ("EXIT",13);
      ("HELP",14);
      ("IDB",15);
      ("IDENTT",16);
      ("LIST",17);
      ("LOAD_DATA",18);
      ("LOAD_HELP",19);
      ("LOAD_OBJECT",20);
      ("LOAD_SCRIPT",21);
      ("PARSE",22);
      ("PARSE_HELP",23);
      ("PRINT",24);
      ("QUERY",25);
      ("QUERY_HELP",26);
      ("REALIZE",27);
      ("REALIZE_HELP",28);
      ("REALIZE_SHOW",29);
      ("REALIZE_SHOW_HELP",30);
      ("SAVE",31);
      ("SAVE_HELP",32);
      ("SELECT",33);
      ("SEMICOLONN",34);
      ("TRACE",35);
      ("UNSELECT",36);
      ("WAIT",37);]
end

type ('dypgen__Inh_dypgen__early_action_0, 'dypgen__Obj_all_commands, 'dypgen__Obj_command, 'dypgen__Obj_dypgen__early_action_0, 'dypgen__Obj_dypgen__epsilon, 'dypgen__Obj_optional_ident, 'dypgen__Obj_optional_idents) obj =
  | Inh_dypgen__early_action_0 of 'dypgen__Inh_dypgen__early_action_0
  | Lexeme_matched of string
  | Obj_ADD of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_ADD_HELP
  | Obj_ANALYSE of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_ANALYSE_HELP
  | Obj_AS
  | Obj_CHECK of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_CHECK_HELP
  | Obj_COMPOSE
  | Obj_CREATE_HELP
  | Obj_CREATE_LEX
  | Obj_CREATE_SIG
  | Obj_DONT
  | Obj_EOII
  | Obj_EXIT
  | Obj_HELP
  | Obj_IDB of (Abstract_syntax.Abstract_syntax.location)
  | Obj_IDENTT of ((string*Abstract_syntax.Abstract_syntax.location))
  | Obj_LIST
  | Obj_LOAD_DATA of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_LOAD_HELP
  | Obj_LOAD_OBJECT of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_LOAD_SCRIPT of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_PARSE of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_PARSE_HELP
  | Obj_PRINT of (Abstract_syntax.Abstract_syntax.location)
  | Obj_QUERY of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_QUERY_HELP
  | Obj_REALIZE of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_REALIZE_HELP
  | Obj_REALIZE_SHOW of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_REALIZE_SHOW_HELP
  | Obj_SAVE of ((string*Abstract_syntax.Abstract_syntax.location*string))
  | Obj_SAVE_HELP
  | Obj_SELECT
  | Obj_SEMICOLONN of (string)
  | Obj_TRACE
  | Obj_UNSELECT
  | Obj_WAIT
  | Obj_all_commands of 'dypgen__Obj_all_commands
  | Obj_command of 'dypgen__Obj_command
  | Obj_dypgen__early_action_0 of 'dypgen__Obj_dypgen__early_action_0
  | Obj_dypgen__epsilon of 'dypgen__Obj_dypgen__epsilon
  | Obj_optional_ident of 'dypgen__Obj_optional_ident
  | Obj_optional_idents of 'dypgen__Obj_optional_idents
  | Obj_zzcommands of ((Environment.t))

module Dyp_symbols_array =
struct
  let token_name_array =
  [|"ADD";
    "ADD_HELP";
    "ANALYSE";
    "ANALYSE_HELP";
    "AS";
    "CHECK";
    "CHECK_HELP";
    "COMPOSE";
    "CREATE_HELP";
    "CREATE_LEX";
    "CREATE_SIG";
    "DONT";
    "EOII";
    "EXIT";
    "HELP";
    "IDB";
    "IDENTT";
    "LIST";
    "LOAD_DATA";
    "LOAD_HELP";
    "LOAD_OBJECT";
    "LOAD_SCRIPT";
    "PARSE";
    "PARSE_HELP";
    "PRINT";
    "QUERY";
    "QUERY_HELP";
    "REALIZE";
    "REALIZE_HELP";
    "REALIZE_SHOW";
    "REALIZE_SHOW_HELP";
    "SAVE";
    "SAVE_HELP";
    "SELECT";
    "SEMICOLONN";
    "TRACE";
    "UNSELECT";
    "WAIT"|]
  let nt_cons_list =
  [
    ("0_1",20);
    ("all_commands",17);
    ("command",18);
    ("dypgen__early_action_0",19);
    ("optional_ident",21);
    ("optional_idents",22);
    ("zzcommands",23)]
  let str_cons o = match o with
    | Inh_dypgen__early_action_0 _ -> "Inh_dypgen__early_action_0"
    | Lexeme_matched _ -> "Lexeme_matched"
    | Obj_ADD _ -> "Obj_ADD"
    | Obj_ANALYSE _ -> "Obj_ANALYSE"
    | Obj_CHECK _ -> "Obj_CHECK"
    | Obj_IDB _ -> "Obj_IDB"
    | Obj_IDENTT _ -> "Obj_IDENTT"
    | Obj_LOAD_DATA _ -> "Obj_LOAD_DATA"
    | Obj_LOAD_OBJECT _ -> "Obj_LOAD_OBJECT"
    | Obj_LOAD_SCRIPT _ -> "Obj_LOAD_SCRIPT"
    | Obj_PARSE _ -> "Obj_PARSE"
    | Obj_PRINT _ -> "Obj_PRINT"
    | Obj_QUERY _ -> "Obj_QUERY"
    | Obj_REALIZE _ -> "Obj_REALIZE"
    | Obj_REALIZE_SHOW _ -> "Obj_REALIZE_SHOW"
    | Obj_SAVE _ -> "Obj_SAVE"
    | Obj_SEMICOLONN _ -> "Obj_SEMICOLONN"
    | Obj_all_commands _ -> "Obj_all_commands"
    | Obj_command _ -> "Obj_command"
    | Obj_dypgen__early_action_0 _ -> "Obj_dypgen__early_action_0"
    | Obj_dypgen__epsilon _ -> "Obj_dypgen__epsilon"
    | Obj_optional_ident _ -> "Obj_optional_ident"
    | Obj_optional_idents _ -> "Obj_optional_idents"
    | Obj_zzcommands _ -> "Obj_zzcommands"
    | _ -> failwith "str_cons, unexpected constructor"
  let cons_array = [|
    "Inh_dypgen__early_action_0";
    "Lexeme_matched";
    "Obj_ADD";
    "Obj_ANALYSE";
    "Obj_CHECK";
    "Obj_IDB";
    "Obj_IDENTT";
    "Obj_LOAD_DATA";
    "Obj_LOAD_OBJECT";
    "Obj_LOAD_SCRIPT";
    "Obj_PARSE";
    "Obj_PRINT";
    "Obj_QUERY";
    "Obj_REALIZE";
    "Obj_REALIZE_SHOW";
    "Obj_SAVE";
    "Obj_SEMICOLONN";
    "Obj_all_commands";
    "Obj_command";
    "Obj_dypgen__early_action_0";
    "Obj_dypgen__epsilon";
    "Obj_optional_ident";
    "Obj_optional_idents";
    "Obj_zzcommands";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
  |]
  let entry_points = [
    "zzcommands";]
end

let dypgen_lexbuf_position lexbuf =
  (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)

module Dyp_aux_functions =
struct
  let get_token_value t = match t with
    | ADD x -> Obj_ADD x
    | ADD_HELP -> Obj_ADD_HELP
    | ANALYSE x -> Obj_ANALYSE x
    | ANALYSE_HELP -> Obj_ANALYSE_HELP
    | AS -> Obj_AS
    | CHECK x -> Obj_CHECK x
    | CHECK_HELP -> Obj_CHECK_HELP
    | COMPOSE -> Obj_COMPOSE
    | CREATE_HELP -> Obj_CREATE_HELP
    | CREATE_LEX -> Obj_CREATE_LEX
    | CREATE_SIG -> Obj_CREATE_SIG
    | DONT -> Obj_DONT
    | EOII -> Obj_EOII
    | EXIT -> Obj_EXIT
    | HELP -> Obj_HELP
    | IDB x -> Obj_IDB x
    | IDENTT x -> Obj_IDENTT x
    | LIST -> Obj_LIST
    | LOAD_DATA x -> Obj_LOAD_DATA x
    | LOAD_HELP -> Obj_LOAD_HELP
    | LOAD_OBJECT x -> Obj_LOAD_OBJECT x
    | LOAD_SCRIPT x -> Obj_LOAD_SCRIPT x
    | PARSE x -> Obj_PARSE x
    | PARSE_HELP -> Obj_PARSE_HELP
    | PRINT x -> Obj_PRINT x
    | QUERY x -> Obj_QUERY x
    | QUERY_HELP -> Obj_QUERY_HELP
    | REALIZE x -> Obj_REALIZE x
    | REALIZE_HELP -> Obj_REALIZE_HELP
    | REALIZE_SHOW x -> Obj_REALIZE_SHOW x
    | REALIZE_SHOW_HELP -> Obj_REALIZE_SHOW_HELP
    | SAVE x -> Obj_SAVE x
    | SAVE_HELP -> Obj_SAVE_HELP
    | SELECT -> Obj_SELECT
    | SEMICOLONN x -> Obj_SEMICOLONN x
    | TRACE -> Obj_TRACE
    | UNSELECT -> Obj_UNSELECT
    | WAIT -> Obj_WAIT
  let cons_table = Dyp.Tools.hashtbl_of_array Dyp_symbols_array.cons_array
end

module Dyp_priority_data =
struct
  let relations = [
  ]
end

let global_data = ()
let local_data = ()
let global_data_equal = (==)
let local_data_equal = (==)

let dyp_merge_Inh_dypgen__early_action_0 = Dyp.Tools.keep_zero
let dyp_merge_Lexeme_matched = Dyp.Tools.keep_zero
let dyp_merge_Obj_ADD = Dyp.Tools.keep_zero
let dyp_merge_Obj_ANALYSE = Dyp.Tools.keep_zero
let dyp_merge_Obj_CHECK = Dyp.Tools.keep_zero
let dyp_merge_Obj_IDB = Dyp.Tools.keep_zero
let dyp_merge_Obj_IDENTT = Dyp.Tools.keep_zero
let dyp_merge_Obj_LOAD_DATA = Dyp.Tools.keep_zero
let dyp_merge_Obj_LOAD_OBJECT = Dyp.Tools.keep_zero
let dyp_merge_Obj_LOAD_SCRIPT = Dyp.Tools.keep_zero
let dyp_merge_Obj_PARSE = Dyp.Tools.keep_zero
let dyp_merge_Obj_PRINT = Dyp.Tools.keep_zero
let dyp_merge_Obj_QUERY = Dyp.Tools.keep_zero
let dyp_merge_Obj_REALIZE = Dyp.Tools.keep_zero
let dyp_merge_Obj_REALIZE_SHOW = Dyp.Tools.keep_zero
let dyp_merge_Obj_SAVE = Dyp.Tools.keep_zero
let dyp_merge_Obj_SEMICOLONN = Dyp.Tools.keep_zero
let dyp_merge_Obj_all_commands = Dyp.Tools.keep_zero
let dyp_merge_Obj_command = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__early_action_0 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__epsilon = Dyp.Tools.keep_zero
let dyp_merge_Obj_optional_ident = Dyp.Tools.keep_zero
let dyp_merge_Obj_optional_idents = Dyp.Tools.keep_zero
let dyp_merge_Obj_zzcommands = Dyp.Tools.keep_zero
let dyp_merge = Dyp.keep_one
let dypgen_match_length = `shortest
let dypgen_choose_token = `first
let dypgen_keep_data = `both
let dypgen_use_rule_order = false
let dypgen_use_all_actions = false

# 36 "script_parser.dyp"
  
    open Dyp
    let local_data = (Environment.empty,fun _ -> failwith "Bug: Not yet defined")
    (* for global_data, the first projection describe whether when
       executing a script, the interpreter should wait between two
       commands. The second projection is used to specify whether to
       echo the current command. The third projection correspond to
       the include dirs, and the the fourth one indicates whether a
       svg output (if relevant) should be generated and in which file
       name (Some "filename") or not (None).  *)
      
    let global_data = false,true,[""],None

    let echo ((_,b,_,_):global_data_type) s = if b then Printf.printf "%s\n%!" s else ()
    let wait  ((b,_,_,_):global_data_type) f = 
      if b then ignore (f () )
    let svg ((_,_,_,b):global_data_type) = b


  
let _ = () (* dummy line to improve OCaml error location *)
# 436                "script_parser_temp.ml"
let __dypgen_ra_list, __dypgen_main_lexer, __dypgen_aux_lexer =
[
(("zzcommands",[Dyp.Ter "EOII"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] ->  let res = 
# 104 "script_parser.dyp"
(
       (let e,f = dyp.last_local_data in
	   (e,[Local_data (e,f)])):((Environment.t)) * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 446                "script_parser_temp.ml"
  in Obj_zzcommands(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("zzcommands",[Dyp.Non_ter ("0_1",Dyp.No_priority );Dyp.Non_ter ("command",Dyp.No_priority );Dyp.Non_ter ("dypgen__early_action_0",Dyp.No_priority );Dyp.Non_ter ("zzcommands",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_command ( (
# 107 "script_parser.dyp"
         (c:'dypgen__Obj_command)
# 456                "script_parser_temp.ml"
 as _1));Obj_dypgen__early_action_0 ( (
(_:'dypgen__Obj_dypgen__early_action_0)
# 459                "script_parser_temp.ml"
 as _2));Obj_zzcommands ( (
(_:'dypgen__Obj_zzcommands)
# 462                "script_parser_temp.ml"
 as _3))] ->  let res = 
# 123 "script_parser.dyp"
(
                 (let e,f = (dyp.last_local_data) in e,[Local_data (e,f)]):((Environment.t)) * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 467                "script_parser_temp.ml"
  in Obj_zzcommands(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[3,
(fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_inh_val (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_command ( (
# 107 "script_parser.dyp"
         (c:'dypgen__Obj_command)
# 475                "script_parser_temp.ml"
 as _1))] -> Inh_dypgen__early_action_0 
(
((_1)):'dypgen__Inh_dypgen__early_action_0)
# 479                "script_parser_temp.ml"
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl)])
;
(("dypgen__early_action_0",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Inh_dypgen__early_action_0( (
# 107 "script_parser.dyp"
         (c:'dypgen__Obj_command)
# 487                "script_parser_temp.ml"
 as _1))] ->  let res = 
# 106 "script_parser.dyp"
(
                (let e,f = (dyp.last_local_data) in
		  let e' = 
		    try
		      c e
		    with
		      | F.Not_yet_implemented s-> raise (Scripting_errors.Error (Scripting_errors.Not_yet_implemented s,(Lexing.dummy_pos,Lexing.dummy_pos)))
		      | Scripting_errors.Error (er,loc_er) ->
			  let () = Printf.fprintf stderr "Error: %s\n%!" (Scripting_errors.error_msg er loc_er) in
			  let _ = Script_lexer.reset_echo () in
			    e
		  in
		    try
		      let () = wait dyp.global_data read_line in
			(e',[Local_data (e',f)])
		    with
		      | Sys.Break 
		      | End_of_file -> raise F.Stop ):'dypgen__Obj_dypgen__early_action_0 * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 508                "script_parser_temp.ml"
  in Obj_dypgen__early_action_0(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "EXIT";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 128 "script_parser.dyp"
                 (l:(string))
# 518                "script_parser_temp.ml"
 as _2)] ->  let res = 
# 127 "script_parser.dyp"
(
                     (let g_d1,g_d2,g_d3,g_d4 = dyp.global_data in
			 (fun e -> let () = echo dyp.global_data l in let () = F.exit () in e),[Global_data (true,g_d2,g_d3,g_d4)]):'dypgen__Obj_command * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 524                "script_parser_temp.ml"
  in Obj_command(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "WAIT";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 130 "script_parser.dyp"
                 (l:(string))
# 534                "script_parser_temp.ml"
 as _2)] ->  let res = 
# 129 "script_parser.dyp"
(
                     (let g_d1,g_d2,g_d3,g_d4 = dyp.global_data in
			 (fun e -> let () = echo dyp.global_data l in let () = F.wait () in e),[Global_data (true,g_d2,g_d3,g_d4)]):'dypgen__Obj_command * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 540                "script_parser_temp.ml"
  in Obj_command(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "DONT";Dyp.Ter "WAIT";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2;Obj_SEMICOLONN  (
# 132 "script_parser.dyp"
                      (l:(string))
# 550                "script_parser_temp.ml"
 as _3)] ->  let res = 
# 131 "script_parser.dyp"
(
                          (let g_d1,g_d2,g_d3,g_d4 = dyp.global_data in
			      (fun e ->  let () = echo dyp.global_data l in let () = F.dont_wait () in e),[Global_data (false,g_d2,g_d3,g_d4)]):'dypgen__Obj_command * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 556                "script_parser_temp.ml"
  in Obj_command(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "LOAD_DATA"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LOAD_DATA  (
# 134 "script_parser.dyp"
           (s,loc,l:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 566                "script_parser_temp.ml"
 as _1)] -> Obj_command 
# 133 "script_parser.dyp"
(
                      (fun e ->  let () = echo dyp.global_data l in
			 let _,_,incl,_ = dyp.global_data in
			   F.load F.Data  s  incl e):'dypgen__Obj_command)
# 573                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "LOAD_OBJECT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LOAD_OBJECT  (
# 137 "script_parser.dyp"
             (s,loc,l:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 582                "script_parser_temp.ml"
 as _1)] -> Obj_command 
# 136 "script_parser.dyp"
(
                        (fun e ->  let () = echo dyp.global_data l in
			 let _,_,incl,_ = dyp.global_data in
			   F.load F.Object s incl e):'dypgen__Obj_command)
# 589                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "LOAD_SCRIPT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LOAD_SCRIPT  (
# 140 "script_parser.dyp"
             (s,loc,l:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 598                "script_parser_temp.ml"
 as _1)] -> Obj_command 
# 139 "script_parser.dyp"
(
                        (fun e ->  let () = echo dyp.global_data l in
			   let _,_,includes,_ = dyp.global_data in
			   let new_env = F.load (F.Script (snd dyp.last_local_data)) s includes e in
			     new_env):'dypgen__Obj_command)
# 606                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "LIST";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 144 "script_parser.dyp"
                 (l:(string))
# 615                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 143 "script_parser.dyp"
(
                     (fun e ->  let () = echo dyp.global_data l in let () = F.list e in e):'dypgen__Obj_command)
# 620                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "SELECT";Dyp.Ter "IDENTT";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_IDENTT  (
# 145 "script_parser.dyp"
               (name,loc:((string*Abstract_syntax.Abstract_syntax.location)))
# 629                "script_parser_temp.ml"
 as _2);Obj_SEMICOLONN  (
# 145 "script_parser.dyp"
                                     (l:(string))
# 633                "script_parser_temp.ml"
 as _3)] -> Obj_command 
# 144 "script_parser.dyp"
(
                                         (fun e ->  let () = echo dyp.global_data l in F.select name loc e):'dypgen__Obj_command)
# 638                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "UNSELECT";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 146 "script_parser.dyp"
                     (l:(string))
# 647                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 145 "script_parser.dyp"
(
                         ( let () = echo dyp.global_data l in F.unselect):'dypgen__Obj_command)
# 652                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "TRACE";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 147 "script_parser.dyp"
                  (l:(string))
# 661                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 146 "script_parser.dyp"
(
                      ( let () = echo dyp.global_data l in fun e -> let () = F.trace () in e):'dypgen__Obj_command)
# 666                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "DONT";Dyp.Ter "TRACE";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2;Obj_SEMICOLONN  (
# 148 "script_parser.dyp"
                       (l:(string))
# 675                "script_parser_temp.ml"
 as _3)] -> Obj_command 
# 147 "script_parser.dyp"
(
                           ( let () = echo dyp.global_data l in fun e -> let () = F.dont_trace () in e):'dypgen__Obj_command)
# 680                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_ident",Dyp.No_priority );Dyp.Ter "PRINT";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_ident ( (
# 149 "script_parser.dyp"
                (name:'dypgen__Obj_optional_ident)
# 689                "script_parser_temp.ml"
 as _1));Obj_PRINT  (
# 149 "script_parser.dyp"
                            (p:(Abstract_syntax.Abstract_syntax.location))
# 693                "script_parser_temp.ml"
 as _2);Obj_SEMICOLONN  (
# 149 "script_parser.dyp"
                                          (l:(string))
# 697                "script_parser_temp.ml"
 as _3)] -> Obj_command 
# 148 "script_parser.dyp"
(
                                              (
  let () = echo dyp.global_data l in fun e -> 
    let loc = 
      match name with
      | None -> p
      | Some (_,l) -> l in
    match name with
    | None -> let () = F.print e loc in e
    | Some (n,l) -> let () = F.print ~name:n e loc in e):'dypgen__Obj_command)
# 710                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_idents",Dyp.No_priority );Dyp.Ter "ANALYSE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_idents ( (
# 158 "script_parser.dyp"
                 (names:'dypgen__Obj_optional_idents)
# 719                "script_parser_temp.ml"
 as _1));Obj_ANALYSE  (
# 158 "script_parser.dyp"
                                (t,l,line:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 723                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 157 "script_parser.dyp"
(
                                             ( let () = echo dyp.global_data line in fun e -> 
					     match names with
					       | [] -> let () = F.analyse e t l in e
					       | _ -> let () = F.analyse  ~names e t l in e):'dypgen__Obj_command)
# 731                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_idents",Dyp.No_priority );Dyp.Ter "CHECK"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_idents ( (
# 162 "script_parser.dyp"
                 (names:'dypgen__Obj_optional_idents)
# 740                "script_parser_temp.ml"
 as _1));Obj_CHECK  (
# 162 "script_parser.dyp"
                              (t,l,line:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 744                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 161 "script_parser.dyp"
(
                                           ( 
  let () = echo dyp.global_data line in fun e -> 
    match names with
    | [] -> let () = F.check e t l in e
    | _ -> let () = F.check  ~names e t l in e):'dypgen__Obj_command)
# 753                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_idents",Dyp.No_priority );Dyp.Ter "REALIZE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_idents ( (
# 167 "script_parser.dyp"
                 (names:'dypgen__Obj_optional_idents)
# 762                "script_parser_temp.ml"
 as _1));Obj_REALIZE  (
# 167 "script_parser.dyp"
                                (t,l,line:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 766                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 166 "script_parser.dyp"
(
                                             ( 
  let () = echo dyp.global_data line in fun e -> 
    match names with
    | [] -> let () = F.realize ?svg_output:(svg dyp.global_data) e t l in e
    | _ -> let () = F.realize  ~names ?svg_output:(svg dyp.global_data) e t l in e):'dypgen__Obj_command)
# 775                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_idents",Dyp.No_priority );Dyp.Ter "REALIZE_SHOW"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_idents ( (
# 172 "script_parser.dyp"
                 (names:'dypgen__Obj_optional_idents)
# 784                "script_parser_temp.ml"
 as _1));Obj_REALIZE_SHOW  (
# 172 "script_parser.dyp"
                                     (t,l,line:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 788                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 171 "script_parser.dyp"
(
                                                  ( 
  let () = echo dyp.global_data line in fun e -> 
    match names with
    | [] -> let () = F.realize_show e t l in e
    | _ -> let () = F.realize_show  ~names e t l in e):'dypgen__Obj_command)
# 797                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_ident",Dyp.No_priority );Dyp.Ter "PARSE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_ident ( (
# 177 "script_parser.dyp"
                (name:'dypgen__Obj_optional_ident)
# 806                "script_parser_temp.ml"
 as _1));Obj_PARSE  (
# 177 "script_parser.dyp"
                            (t,l,line:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 810                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 176 "script_parser.dyp"
(
                                         (
  let () = echo dyp.global_data line in fun e -> 
    match name with
    | None ->  let () = F.parse  e t l in e
    | Some (n,lex_loc) ->  let () = F.parse ~name:n e t lex_loc in e):'dypgen__Obj_command)
# 819                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_ident",Dyp.No_priority );Dyp.Ter "QUERY"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_ident ( (
# 182 "script_parser.dyp"
                (name:'dypgen__Obj_optional_ident)
# 828                "script_parser_temp.ml"
 as _1));Obj_QUERY  (
# 182 "script_parser.dyp"
                            (t,l,line:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 832                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 181 "script_parser.dyp"
(
                                         (
  let () = echo dyp.global_data line in fun e -> 
    match name with
    | None ->  let () = F.query  e t l in e
    | Some (n,lex_loc) ->  let () = F.query ~name:n e t lex_loc in e):'dypgen__Obj_command)
# 841                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_ident",Dyp.No_priority );Dyp.Ter "IDB";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_ident ( (
# 187 "script_parser.dyp"
                (name:'dypgen__Obj_optional_ident)
# 850                "script_parser_temp.ml"
 as _1));Obj_IDB  (
# 187 "script_parser.dyp"
                          (p:(Abstract_syntax.Abstract_syntax.location))
# 854                "script_parser_temp.ml"
 as _2);Obj_SEMICOLONN  (
# 187 "script_parser.dyp"
                                        (l:(string))
# 858                "script_parser_temp.ml"
 as _3)] -> Obj_command 
# 186 "script_parser.dyp"
(
                                            (
  let () = echo dyp.global_data l in fun e -> 
    let loc = 
      match name with
      | None -> p
      | Some (_,l) -> l in
    match name with
    | None -> let () = F.idb e loc in e
    | Some (n,l) -> let () = F.idb ~name:n e loc in e):'dypgen__Obj_command)
# 871                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_idents",Dyp.No_priority );Dyp.Ter "ADD"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_idents ( (
# 196 "script_parser.dyp"
                 (names:'dypgen__Obj_optional_idents)
# 880                "script_parser_temp.ml"
 as _1));Obj_ADD  (
# 196 "script_parser.dyp"
                            (t,l,line:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 884                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 195 "script_parser.dyp"
(
                                         ( let () = echo dyp.global_data line in fun e -> 
					     match names with
					       | [] -> F.add e t l
					       | _ -> F.add  ~names e t l):'dypgen__Obj_command)
# 892                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "COMPOSE";Dyp.Ter "IDENTT";Dyp.Ter "IDENTT";Dyp.Ter "AS";Dyp.Ter "IDENTT";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_IDENTT  (
# 200 "script_parser.dyp"
                (n1:((string*Abstract_syntax.Abstract_syntax.location)))
# 901                "script_parser_temp.ml"
 as _2);Obj_IDENTT  (
# 200 "script_parser.dyp"
                            (n2:((string*Abstract_syntax.Abstract_syntax.location)))
# 905                "script_parser_temp.ml"
 as _3); _4;Obj_IDENTT  (
# 200 "script_parser.dyp"
                                          (n3:((string*Abstract_syntax.Abstract_syntax.location)))
# 909                "script_parser_temp.ml"
 as _5);Obj_SEMICOLONN  (
# 200 "script_parser.dyp"
                                                         (l:(string))
# 913                "script_parser_temp.ml"
 as _6)] -> Obj_command 
# 199 "script_parser.dyp"
(
                                                             ( let () = echo dyp.global_data l in fun e -> F.compose n1 n2 n3 e):'dypgen__Obj_command)
# 918                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 201 "script_parser.dyp"
                 (l:(string))
# 927                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 200 "script_parser.dyp"
(
                     (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help None) in e):'dypgen__Obj_command)
# 932                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("all_commands",Dyp.No_priority );Dyp.Ter "HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_all_commands ( (
# 202 "script_parser.dyp"
              (c:'dypgen__Obj_all_commands)
# 941                "script_parser_temp.ml"
 as _1)); _2;Obj_SEMICOLONN  (
# 202 "script_parser.dyp"
                                 (l:(string))
# 945                "script_parser_temp.ml"
 as _3)] -> Obj_command 
# 201 "script_parser.dyp"
(
                                     (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some c)) in e):'dypgen__Obj_command)
# 950                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "LOAD_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 203 "script_parser.dyp"
                      (l:(string))
# 959                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 202 "script_parser.dyp"
(
                          (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Load)) in e):'dypgen__Obj_command)
# 964                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "LOAD_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 206 "script_parser.dyp"
                      (l:(string))
# 973                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 205 "script_parser.dyp"
(
                          (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Load)) in e):'dypgen__Obj_command)
# 978                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "ANALYSE_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 207 "script_parser.dyp"
                         (l:(string))
# 987                "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 206 "script_parser.dyp"
(
                             (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Analyse)) in e):'dypgen__Obj_command)
# 992                "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "REALIZE_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 208 "script_parser.dyp"
                         (l:(string))
# 1001               "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 207 "script_parser.dyp"
(
                             (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Realize)) in e):'dypgen__Obj_command)
# 1006               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "REALIZE_SHOW_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 209 "script_parser.dyp"
                              (l:(string))
# 1015               "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 208 "script_parser.dyp"
(
                                  (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.RealizeShow)) in e):'dypgen__Obj_command)
# 1020               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "CHECK_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 210 "script_parser.dyp"
                       (l:(string))
# 1029               "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 209 "script_parser.dyp"
(
                           (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Check)) in e):'dypgen__Obj_command)
# 1034               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "PARSE_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 211 "script_parser.dyp"
                       (l:(string))
# 1043               "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 210 "script_parser.dyp"
(
                           (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Parse)) in e):'dypgen__Obj_command)
# 1048               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "QUERY_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 212 "script_parser.dyp"
                       (l:(string))
# 1057               "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 211 "script_parser.dyp"
(
                           (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Query)) in e):'dypgen__Obj_command)
# 1062               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "SAVE_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 213 "script_parser.dyp"
                      (l:(string))
# 1071               "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 212 "script_parser.dyp"
(
                          (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Save)) in e):'dypgen__Obj_command)
# 1076               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "ADD_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 214 "script_parser.dyp"
                     (l:(string))
# 1085               "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 213 "script_parser.dyp"
(
                         (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Add)) in e):'dypgen__Obj_command)
# 1090               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "CREATE_HELP";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_SEMICOLONN  (
# 215 "script_parser.dyp"
                        (l:(string))
# 1099               "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 214 "script_parser.dyp"
(
                            (let () = echo dyp.global_data l in fun e -> let () = F.help (F.Help (Some F.Create)) in e):'dypgen__Obj_command)
# 1104               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "CREATE_SIG";Dyp.Ter "IDENTT";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_IDENTT  (
# 217 "script_parser.dyp"
                   (n:((string*Abstract_syntax.Abstract_syntax.location)))
# 1113               "script_parser_temp.ml"
 as _2);Obj_SEMICOLONN  (
# 217 "script_parser.dyp"
                                 (l:(string))
# 1117               "script_parser_temp.ml"
 as _3)] -> Obj_command 
# 216 "script_parser.dyp"
(
                                     (let () = echo dyp.global_data l in fun e -> F.create_sig n e):'dypgen__Obj_command)
# 1122               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Ter "CREATE_LEX";Dyp.Ter "IDENTT";Dyp.Ter "IDENTT";Dyp.Ter "IDENTT";Dyp.Ter "SEMICOLONN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_IDENTT  (
# 218 "script_parser.dyp"
                   (n:((string*Abstract_syntax.Abstract_syntax.location)))
# 1131               "script_parser_temp.ml"
 as _2);Obj_IDENTT  (
# 218 "script_parser.dyp"
                             (n1:((string*Abstract_syntax.Abstract_syntax.location)))
# 1135               "script_parser_temp.ml"
 as _3);Obj_IDENTT  (
# 218 "script_parser.dyp"
                                        (n2:((string*Abstract_syntax.Abstract_syntax.location)))
# 1139               "script_parser_temp.ml"
 as _4);Obj_SEMICOLONN  (
# 218 "script_parser.dyp"
                                                        (l:(string))
# 1143               "script_parser_temp.ml"
 as _5)] -> Obj_command 
# 217 "script_parser.dyp"
(
                                                            (let () = echo dyp.global_data l in fun e -> F.create_lex ~abs:n1 ~obj:n2 n e):'dypgen__Obj_command)
# 1148               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("command",[Dyp.Non_ter ("optional_idents",Dyp.No_priority );Dyp.Ter "SAVE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_idents ( (
# 219 "script_parser.dyp"
                 (names:'dypgen__Obj_optional_idents)
# 1157               "script_parser_temp.ml"
 as _1));Obj_SAVE  (
# 219 "script_parser.dyp"
                             (filename,l,line:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 1161               "script_parser_temp.ml"
 as _2)] -> Obj_command 
# 218 "script_parser.dyp"
(
                                                 ( let () = echo dyp.global_data line in fun e -> 
						match names with
						  | [] -> let () = F.save filename e l in e
						  | _ -> let () = F.save ~names filename e l in e):'dypgen__Obj_command)
# 1169               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "WAIT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_all_commands 
# 224 "script_parser.dyp"
(
       (F.Wait):'dypgen__Obj_all_commands)
# 1179               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "DONT";Dyp.Ter "WAIT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2] -> Obj_all_commands 
# 225 "script_parser.dyp"
(
            (F.Dont_wait):'dypgen__Obj_all_commands)
# 1189               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "LIST"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_all_commands 
# 226 "script_parser.dyp"
(
       (F.List):'dypgen__Obj_all_commands)
# 1199               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "SELECT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_all_commands 
# 227 "script_parser.dyp"
(
         (F.Select):'dypgen__Obj_all_commands)
# 1209               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "UNSELECT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_all_commands 
# 228 "script_parser.dyp"
(
           (F.Unselect):'dypgen__Obj_all_commands)
# 1219               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "TRACE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_all_commands 
# 229 "script_parser.dyp"
(
        (F.Trace):'dypgen__Obj_all_commands)
# 1229               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "DONT";Dyp.Ter "TRACE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2] -> Obj_all_commands 
# 230 "script_parser.dyp"
(
             (F.Dont_trace):'dypgen__Obj_all_commands)
# 1239               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "PRINT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_PRINT  (
(_:(Abstract_syntax.Abstract_syntax.location))
# 1247               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 231 "script_parser.dyp"
(
        (F.Print):'dypgen__Obj_all_commands)
# 1252               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "ANALYSE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ANALYSE  (
(_:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 1260               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 232 "script_parser.dyp"
(
          (F.Analyse):'dypgen__Obj_all_commands)
# 1265               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "CHECK"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_CHECK  (
(_:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 1273               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 233 "script_parser.dyp"
(
        (F.Check):'dypgen__Obj_all_commands)
# 1278               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "REALIZE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_REALIZE  (
(_:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 1286               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 234 "script_parser.dyp"
(
          (F.Realize):'dypgen__Obj_all_commands)
# 1291               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "REALIZE_SHOW"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_REALIZE_SHOW  (
(_:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 1299               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 235 "script_parser.dyp"
(
               (F.RealizeShow):'dypgen__Obj_all_commands)
# 1304               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "PARSE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_PARSE  (
(_:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 1312               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 236 "script_parser.dyp"
(
        (F.Parse):'dypgen__Obj_all_commands)
# 1317               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "IDB"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_IDB  (
(_:(Abstract_syntax.Abstract_syntax.location))
# 1325               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 237 "script_parser.dyp"
(
      (F.Idb):'dypgen__Obj_all_commands)
# 1330               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "QUERY"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_QUERY  (
(_:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 1338               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 238 "script_parser.dyp"
(
        (F.Query):'dypgen__Obj_all_commands)
# 1343               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "ADD"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ADD  (
(_:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 1351               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 239 "script_parser.dyp"
(
      (F.Add):'dypgen__Obj_all_commands)
# 1356               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "COMPOSE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_all_commands 
# 240 "script_parser.dyp"
(
          (F.Compose):'dypgen__Obj_all_commands)
# 1366               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "HELP"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_all_commands 
# 241 "script_parser.dyp"
(
       (F.Help None):'dypgen__Obj_all_commands)
# 1376               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("all_commands",[Dyp.Ter "SAVE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_SAVE  (
(_:((string*Abstract_syntax.Abstract_syntax.location*string)))
# 1384               "script_parser_temp.ml"
 as _1)] -> Obj_all_commands 
# 242 "script_parser.dyp"
(
       (F.Save):'dypgen__Obj_all_commands)
# 1389               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_ident",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_optional_ident 
# 246 "script_parser.dyp"
(
  (None):'dypgen__Obj_optional_ident)
# 1399               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_ident",[Dyp.Ter "IDENTT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_IDENTT  (
# 248 "script_parser.dyp"
        (id:((string*Abstract_syntax.Abstract_syntax.location)))
# 1408               "script_parser_temp.ml"
 as _1)] -> Obj_optional_ident 
# 247 "script_parser.dyp"
(
             (Some id):'dypgen__Obj_optional_ident)
# 1413               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_idents",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_optional_idents 
# 251 "script_parser.dyp"
(
  ([]):'dypgen__Obj_optional_idents)
# 1423               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_idents",[Dyp.Ter "IDENTT";Dyp.Non_ter ("optional_idents",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_IDENTT  (
# 253 "script_parser.dyp"
        (id:((string*Abstract_syntax.Abstract_syntax.location)))
# 1432               "script_parser_temp.ml"
 as _1);Obj_optional_idents ( (
# 253 "script_parser.dyp"
                            (ids:'dypgen__Obj_optional_idents)
# 1436               "script_parser_temp.ml"
 as _2))] -> Obj_optional_idents 
# 252 "script_parser.dyp"
(
                                  (id::ids):'dypgen__Obj_optional_idents)
# 1441               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("0_1",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__epsilon 
(
():'dypgen__Obj_dypgen__epsilon)
# 1450               "script_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])],

(["dummy_entry",Dyp.RE_Eof_char],
[0,(fun _ -> Lexeme_matched "")]),

[]

let __dypgen_regexp_decl = []

let dyp_merge_Inh_dypgen__early_action_0 l =
  match dyp_merge_Inh_dypgen__early_action_0 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Lexeme_matched l =
  match dyp_merge_Lexeme_matched l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_ADD l =
  match dyp_merge_Obj_ADD l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_ANALYSE l =
  match dyp_merge_Obj_ANALYSE l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_CHECK l =
  match dyp_merge_Obj_CHECK l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_IDB l =
  match dyp_merge_Obj_IDB l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_IDENTT l =
  match dyp_merge_Obj_IDENTT l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_LOAD_DATA l =
  match dyp_merge_Obj_LOAD_DATA l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_LOAD_OBJECT l =
  match dyp_merge_Obj_LOAD_OBJECT l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_LOAD_SCRIPT l =
  match dyp_merge_Obj_LOAD_SCRIPT l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_PARSE l =
  match dyp_merge_Obj_PARSE l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_PRINT l =
  match dyp_merge_Obj_PRINT l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_QUERY l =
  match dyp_merge_Obj_QUERY l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_REALIZE l =
  match dyp_merge_Obj_REALIZE l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_REALIZE_SHOW l =
  match dyp_merge_Obj_REALIZE_SHOW l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_SAVE l =
  match dyp_merge_Obj_SAVE l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_SEMICOLONN l =
  match dyp_merge_Obj_SEMICOLONN l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_all_commands l =
  match dyp_merge_Obj_all_commands l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_command l =
  match dyp_merge_Obj_command l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__early_action_0 l =
  match dyp_merge_Obj_dypgen__early_action_0 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__epsilon l =
  match dyp_merge_Obj_dypgen__epsilon l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_optional_ident l =
  match dyp_merge_Obj_optional_ident l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_optional_idents l =
  match dyp_merge_Obj_optional_idents l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_zzcommands l =
  match dyp_merge_Obj_zzcommands l with
    | ([],_,_) -> dyp_merge l
    | res -> res

let __dypgen_merge_list = [(fun l -> (
  let f1 (o,gd,ld) = match o with Inh_dypgen__early_action_0 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Inh_dypgen__early_action_0"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Inh_dypgen__early_action_0 l in
  let f2 o = Inh_dypgen__early_action_0 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Lexeme_matched ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Lexeme_matched"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Lexeme_matched l in
  let f2 o = Lexeme_matched o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_ADD ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_ADD"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_ADD l in
  let f2 o = Obj_ADD o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_ANALYSE ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_ANALYSE"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_ANALYSE l in
  let f2 o = Obj_ANALYSE o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_CHECK ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_CHECK"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_CHECK l in
  let f2 o = Obj_CHECK o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_IDB ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_IDB"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_IDB l in
  let f2 o = Obj_IDB o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_IDENTT ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_IDENTT"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_IDENTT l in
  let f2 o = Obj_IDENTT o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_LOAD_DATA ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_LOAD_DATA"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_LOAD_DATA l in
  let f2 o = Obj_LOAD_DATA o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_LOAD_OBJECT ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_LOAD_OBJECT"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_LOAD_OBJECT l in
  let f2 o = Obj_LOAD_OBJECT o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_LOAD_SCRIPT ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_LOAD_SCRIPT"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_LOAD_SCRIPT l in
  let f2 o = Obj_LOAD_SCRIPT o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_PARSE ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_PARSE"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_PARSE l in
  let f2 o = Obj_PARSE o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_PRINT ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_PRINT"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_PRINT l in
  let f2 o = Obj_PRINT o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_QUERY ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_QUERY"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_QUERY l in
  let f2 o = Obj_QUERY o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_REALIZE ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_REALIZE"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_REALIZE l in
  let f2 o = Obj_REALIZE o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_REALIZE_SHOW ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_REALIZE_SHOW"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_REALIZE_SHOW l in
  let f2 o = Obj_REALIZE_SHOW o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_SAVE ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_SAVE"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_SAVE l in
  let f2 o = Obj_SAVE o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_SEMICOLONN ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_SEMICOLONN"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_SEMICOLONN l in
  let f2 o = Obj_SEMICOLONN o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_all_commands ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_all_commands"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_all_commands l in
  let f2 o = Obj_all_commands o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_command ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_command"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_command l in
  let f2 o = Obj_command o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__early_action_0 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__early_action_0"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__early_action_0 l in
  let f2 o = Obj_dypgen__early_action_0 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__epsilon ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__epsilon"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__epsilon l in
  let f2 o = Obj_dypgen__epsilon o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_optional_ident ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_optional_ident"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_optional_ident l in
  let f2 o = Obj_optional_ident o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_optional_idents ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_optional_idents"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_optional_idents l in
  let f2 o = Obj_optional_idents o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_zzcommands ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_zzcommands"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_zzcommands l in
  let f2 o = Obj_zzcommands o in
  (List.map f2 ol, gd, ld)))]



let __dypgen_test_cons () =  [|
  (fun x -> match x with Inh_dypgen__early_action_0 _ -> true | _ -> false);
  (fun x -> match x with Lexeme_matched _ -> true | _ -> false);
  (fun x -> match x with Obj_ADD _ -> true | _ -> false);
  (fun x -> match x with Obj_ANALYSE _ -> true | _ -> false);
  (fun x -> match x with Obj_CHECK _ -> true | _ -> false);
  (fun x -> match x with Obj_IDB _ -> true | _ -> false);
  (fun x -> match x with Obj_IDENTT _ -> true | _ -> false);
  (fun x -> match x with Obj_LOAD_DATA _ -> true | _ -> false);
  (fun x -> match x with Obj_LOAD_OBJECT _ -> true | _ -> false);
  (fun x -> match x with Obj_LOAD_SCRIPT _ -> true | _ -> false);
  (fun x -> match x with Obj_PARSE _ -> true | _ -> false);
  (fun x -> match x with Obj_PRINT _ -> true | _ -> false);
  (fun x -> match x with Obj_QUERY _ -> true | _ -> false);
  (fun x -> match x with Obj_REALIZE _ -> true | _ -> false);
  (fun x -> match x with Obj_REALIZE_SHOW _ -> true | _ -> false);
  (fun x -> match x with Obj_SAVE _ -> true | _ -> false);
  (fun x -> match x with Obj_SEMICOLONN _ -> true | _ -> false);
  (fun x -> match x with Obj_all_commands _ -> true | _ -> false);
  (fun x -> match x with Obj_command _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__early_action_0 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__epsilon _ -> true | _ -> false);
  (fun x -> match x with Obj_optional_ident _ -> true | _ -> false);
  (fun x -> match x with Obj_optional_idents _ -> true | _ -> false);
  (fun x -> match x with Obj_zzcommands _ -> true | _ -> false)|]

let __dypgen_dummy_marker_2 = ()
let pp () = Dyp.make_parser
  __dypgen_ra_list Dyp_priority_data.relations global_data local_data
  (Dyp.Tools.make_nt_cons_map Dyp_symbols_array.nt_cons_list)
  Dyp_symbols_array.entry_points
  
  false 38 true
  
  Dyp_aux_functions.get_token_value
  Dyp_symbols.get_token_name Dyp_symbols.str_token
  global_data_equal local_data_equal (__dypgen_test_cons ())
  Dyp_symbols_array.str_cons
  Dyp_symbols_array.cons_array Dyp_aux_functions.cons_table
  (Dyp.Tools.array_of_list __dypgen_merge_list)
  dypgen_lexbuf_position __dypgen_regexp_decl __dypgen_main_lexer
  __dypgen_aux_lexer Dyp_symbols.ter_string_list
  (fun lexbuf -> Lexeme_matched (Dyp.lexeme lexbuf))
  false


let __dypgen_dummy_marker_5 = ()

let __dypgen_dummy_marker_3 = ()

let zzcommands ?(global_data=global_data) ?(local_data=local_data) f lexbuf =
  let pf = Dyp.parse (pp ()) "zzcommands" ~global_data:global_data
    ~local_data:local_data ~match_len:dypgen_match_length
    ~keep_data:dypgen_keep_data
    ~use_rule_order:dypgen_use_rule_order
    ~use_all_actions:dypgen_use_all_actions
    ~lexpos:dypgen_lexbuf_position f lexbuf in
  let aux1 (o,p) = match o with
    | Obj_zzcommands r -> (r,p)
    | _ -> failwith "Wrong type for entry result" in
  List.map aux1 pf


let __dypgen_dummy_marker_4 = ()


# 257 "script_parser.dyp"


  let rec parse_file ?(verbose=true) ?svg_output filename  includes env =
    try
      let in_ch =
	let fullname = Utils.find_file filename includes  in
	  open_in fullname in
      let lexbuf = Lexing.from_channel in_ch in
      let () = Printf.printf "Parsing script file \"%s\"...\n%!" filename in
      let new_env=
	try (fst (List.hd (zzcommands ~global_data:(F.should_wait (),verbose,includes,svg_output) ~local_data:(env,parse_file ~verbose ?svg_output)  Script_lexer.lexer lexbuf))) with
	  |  Dyp.Syntax_error -> raise (Error.dyp_error lexbuf filename) in
      let () = Printf.printf "Done.\n%!" in
	new_env
    with
      | Utils.No_file(f,msg) -> let e = Error.System_error (Printf.sprintf "No such file \"%s\" in %s" f msg) in
	let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg e filename) in
	let _ = Script_lexer.reset_echo () in
	  env
      | Sys_error s -> let e = Error.System_error s in
	let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg e filename) in
	let _ = Script_lexer.reset_echo () in
	  env
      | Error.Error e -> 
	  let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg e filename) in
	  let _ = Script_lexer.reset_echo () in
	    env
      | Scripting_errors.Error (e,p) ->
	  let () = Printf.fprintf stderr "Error: %s\n%!" (Scripting_errors.error_msg e p) in
	  let _ = Script_lexer.reset_echo () in
	    env
	      
  let commented_regexp = Str.regexp "^[ \t#]*#"

  let is_fully_commented_line s = Str.string_match commented_regexp s 0

  let read_line_from_in_ch in_ch =
    let () = flush stdout in
    input_line in_ch
    

  let bufferize in_ch =
    let () = Printf.printf "# " in
    let buf = Buffer.create 16 in
    let no_semi_colon=ref true in
    let () =
      while !no_semi_colon do
	let input = read_line_from_in_ch in_ch in
	  if not (is_fully_commented_line input) then
	    try
	      let semi_colon_index=String.index input ';' in
	      let () = Buffer.add_string buf (String.sub input 0 (semi_colon_index+1)) in
		no_semi_colon:=false
	    with
	      | Not_found ->
		  Buffer.add_string buf input ;
		  Buffer.add_char buf '\n';
		  Printf.printf "  "
	  else
	    ()
      done in
      Buffer.contents buf
	

	
  let parse_entry ~resize ?svg_output ?(verbose=true) in_ch includes env =
    let in_str = bufferize in_ch in
    let lexbuf = Lexing.from_string in_str in
    let () = 
      if resize then
	let () = Utils.sterm_set_size () in
	Utils.term_set_size ()
      else
	() in
  let new_env=
    try
      try (fst (List.hd (zzcommands ~global_data:(false,verbose,includes,svg_output) ~local_data:(env,parse_file ~verbose ?svg_output)  Script_lexer.lexer lexbuf))) with
      |  Dyp.Syntax_error -> raise (Error.dyp_error lexbuf "stdin")
    with
    | F.Stop -> env
    | Failure f when f="lexing: empty token" -> env
    | Error.Error e -> 
      let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg e "stdin") in
      let _ = Script_lexer.reset_echo () in
      env
    | Scripting_errors.Error (e,p) ->
      let () = Printf.fprintf stderr "Error: %s\n%!" (Scripting_errors.error_msg e p) in
      let _ = Script_lexer.reset_echo () in
      env in
  new_env
	

end
let _ = () (* dummy line to improve OCaml error location *)
# 1914               "script_parser_temp.ml"
