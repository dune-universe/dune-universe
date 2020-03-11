(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

(*
-----------------------------------------------------------------------------
   Line and column info
-----------------------------------------------------------------------------
*)

type info = FINFO of int*int*string | UNKNOWN

type 'a withinfo = {i: info; v: 'a}

let create_info l c fn = FINFO(l,c,fn)

let withinfo i v = {i=i; v=v}

let noinfo v = {i=UNKNOWN; v=v}

let info_string (i:info): string =
  match i with
    FINFO(l,c,fn) ->
      fn ^ ":" ^ (string_of_int l) ^ ":" ^ (string_of_int c) ^ ":"
  | UNKNOWN    ->
      ""

let info_from_position (pos:Lexing.position) =
  let l = pos.Lexing.pos_lnum
  and c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1
  and fn = pos.Lexing.pos_fname
  in
  create_info l c fn


exception Error_string of string
exception Error_info of info*string
exception Error_fileinfo of string*info*string
exception NYI
exception Reject
exception Undecidable


let error_string (str:string) = raise (Error_string str)

let error_info (i:info) (str:string) =
  raise (Error_info (i,str))

let not_yet_implemented (i:info) (str:string) =
  error_info i (str ^ " not yet implemented")


(*
-----------------------------------------------------------------------------
   Syntax error function
-----------------------------------------------------------------------------
*)
let parse_error_fun : (string->unit) ref =
  ref (fun str -> Printf.eprintf "%s\n" str;  failwith "Syntax error")


module Parse_info: sig
  val file_name:     unit -> string
  val set_file_name:     string -> unit
end = struct
  let fname:string ref              = ref ""
  let file_name (): string =
    !fname

  let set_file_name (fn: string): unit =
    fname := fn
end (* Parse_info *)


(*
-----------------------------------------------------------------------------
   Symbol table
-----------------------------------------------------------------------------
*)

module ST : sig
  val symbol: string -> int
  val string: int    -> string
  val count:  unit   -> int
  val domain:          int
  val tuple:           int
  val first:           int
  val second:          int
  val singleton:       int
  val class_boolean:   int
  val class_any:       int
  val class_tuple:     int
  val class_function:  int
  val class_predicate: int
  val generic_a:       int
  val generic_b:       int
  val generic_g:       int
  val generic_h:       int
end = struct
  open Container
  let kt                   = Key_table.empty ()
  let symbol (str:string)  = Key_table.index str kt
  let string (i:int)       = Key_table.key   i   kt
  let count ()             = Key_table.count kt
  let domain          = symbol "domain"
  let tuple           = symbol "tuple"
  let first           = symbol "first"
  let second          = symbol "second"
  let singleton       = symbol "singleton"
  let class_tuple     = symbol "TUPLE"
  let class_boolean   = symbol "BOOLEAN"
  let class_any       = symbol "ANY"
  let class_function  = symbol "FUNCTION"
  let class_predicate = symbol "PREDICATE"
  let generic_a       = symbol "A"
  let generic_b       = symbol "B"
  let generic_g       = symbol "G"
  let generic_h       = symbol "H"
end

let n_names_with_start (c:char) (size:int): int array =
  let code = Char.code c in
  Array.init size (fun i -> ST.symbol (String.make 1 (Char.chr (i + code))))

let standard_fgnames (size:int): int array =
  n_names_with_start 'A' size

let standard_argnames (size:int): int array =
  n_names_with_start 'a' size

let anon_argnames (size:int): int array =
  Array.init size (fun i -> ST.symbol ("$" ^ (string_of_int i)))

let empty_argnames (size:int): int array =
  Array.make size (ST.symbol "_")

(*
-----------------------------------------------------------------------------
   Parsing types
-----------------------------------------------------------------------------
*)

(* Utility functions *)

let rec string_of_path (p: int list) =
  match p with
    [] -> ""
  | f::t -> (ST.string f) ^ "." ^ (string_of_path t)

let string_of_option  (o: 'a option) (sfun: 'a -> string) =
  match o with
    None -> ""
  | Some e -> sfun e

let rec string_of_list (l: 'a list) (sfun: 'a -> string) (sep: string) =
  String.concat sep (List.map sfun l)

(* more efficient but more complicated
let rec string_of_list (l: 'a list) (sfun: 'a -> string) (sep: string) =
  match l with
    [] -> ""
  | f::t ->
      (sfun f) ^ (List.fold_left (fun str el -> str ^ sep ^ (sfun el))"" t)
*)


let rec split_list (l: 'a list) (sep: 'a -> bool): 'a list list =
  match l with
    [] -> []
  | f::t ->
      let res = split_list t sep
      in
      match res with
        [] -> [[f]]
      | f1::t1 ->
          if sep f
          then [f]::res
          else (f::f1)::t1



(* Types *)

type type_t =
    Normal_type of (int list) * int * type_t list   (* kernel.ANY,
                                                kernel.ARRAY[NATURAL] *)
  | Arrow_type of type_t * type_t        (* A -> B              *)
  | Tuple_type of type_t list
  | Brace_type of type_t
  | Star_type  of type_t
  | List_type  of type_t
  | Paren_type of type_t


let rec string_of_type (t:type_t) =
  let actuals l =
    match l with
      [] -> ""
      | _::_ ->
          "["
          ^ (string_of_list l string_of_type ",")
          ^ "]"
  in
  match t with
    Normal_type (p,n,l) ->
      let ps = string_of_path p
      in
      ps ^ (ST.string n) ^ (actuals l)
  | Arrow_type (t1,t2) ->
      (string_of_type t1) ^ "->" ^ (string_of_type t2)
  | Tuple_type l -> actuals l
  | Brace_type t -> "{" ^ (string_of_type t) ^ "}"
  | Star_type t  -> (string_of_type t) ^ "*"
  | List_type t  -> "[" ^ (string_of_type t) ^ "]"
  | Paren_type t -> "(" ^ (string_of_type t) ^ ")"


type return_type = (type_t*bool*bool) withinfo option (* tp,proc,ghost *)

let string_of_return_type (rt:return_type): string =
  match rt with
    None -> ""
  | Some rt ->
      let tp,proc,ghost = rt.v in
      ":" ^
      (if ghost then "ghost " else "") ^
      (string_of_type tp) ^
      (if proc then "!" else "")

(* Formal arguments *)

type entities =
    Untyped_entities of int list
  | Typed_entities   of int list * type_t withinfo


let string_of_entities (args: entities) =
  match args with
    Typed_entities (l,t) ->
      (string_of_list l ST.string ",") ^ ":" ^ string_of_type t.v
  | Untyped_entities l -> string_of_list l ST.string ","


let string_of_formals (args: entities list) =
  string_of_list args string_of_entities ","




(* Expressions *)

type quantifier =
    Universal
  | Existential

type operator =
    Plusop
  | Minusop
  | Timesop
  | Divideop
  | Modop
  | Caretop
  | Commaop
  | Dotop
  | Eqop
  | NEqop
  | Eqvop
  | NEqvop
  | LTop
  | LEop
  | GTop
  | GEop
  | Asop
  | Andop
  | Orop
  | Oldop
  | Notop
  | Barop
  | DBarop
  | Arrowop
  | Bracketop
  | Parenop
  | DArrowop
  | DColonop
  | Inop
  | Notinop
  | Allop (* Allop and Someop still necessary? "Feature_table.term_to_string" *)
  | Someop
  | Freeop  of int
  | RFreeop of int

type associativity = Left | Right | Nonassoc

let operator_data op =
  match op with
    Plusop    -> "+",   45,  Left
  | Minusop   -> "-",   45,  Left
  | Timesop   -> "*",   50,  Left
  | Divideop  -> "/",   50,  Left
  | Modop     -> "mod", 50,  Left
  | Caretop   -> "^",   55,  Right
  | Commaop   -> ",",   18,  Right
  | Dotop     -> ".",   70,  Left
  | Eqop      -> "=",   35,  Nonassoc
  | NEqop     -> "/=",  35,  Nonassoc
  | Eqvop     -> "~",   35,  Nonassoc
  | NEqvop    -> "/~",  35,  Nonassoc
  | LTop      -> "<",   35,  Nonassoc
  | LEop      -> "<=",  35,  Nonassoc
  | GTop      -> ">",   35,  Nonassoc
  | GEop      -> ">=",  35,  Nonassoc
  | Inop      -> "in",  35,  Nonassoc
  | Notinop   -> "/in", 35,  Nonassoc
  | Asop      -> "as",  35,  Nonassoc
  | Andop     -> "and", 25,  Left
  | Orop      -> "or",  25,  Left
  | Oldop     -> "old", 65,  Nonassoc
  | Notop     -> "not", 65,  Nonassoc
  | Barop     -> "|",   40,  Left
  | DBarop    -> "||",  40,  Left
  | Arrowop   -> "->",  13,  Right
  | Parenop   -> "()",  80, Nonassoc
  | Bracketop -> "[]",  70, Nonassoc
  | DArrowop  -> "==>", 20,  Right
  | DColonop  -> "::",  55,  Right
  | Allop     -> "all",  8,  Nonassoc
  | Someop    -> "some", 8, Nonassoc
  | Freeop  i -> ST.string i, 60,  Left
  | RFreeop i -> ST.string i, 61,  Right


let is_binary (op:operator): bool =
  match op with
    Plusop | Minusop | Timesop | Divideop | Caretop | Eqop | NEqop
  | Eqvop  | NEqvop  | LTop    | LEop     | GTop    | GEop | Andop
  | Orop   | Barop   | DBarop  | Inop     | Notinop
  | DArrowop -> true
  | Freeop i | RFreeop i -> true
  | _ -> false


let is_unary (op:operator): bool =
  match op with
    Plusop | Minusop | Notop | Oldop | Allop | Someop -> true
  | Freeop i | RFreeop i -> true
  | _ -> false


let is_nary (op:operator): bool =
  match op with
    Parenop | Bracketop -> true
  | _ -> false




let is_letter ch =
  let ic = Char.code ch
  in
  ((Char.code 'A') <= ic && ic <= (Char.code 'Z')) ||
  ((Char.code 'a') <= ic && ic <= (Char.code 'z'))


let operator_to_rawstring op =
  let s,_,_ = operator_data op
  in
  s

let operator_to_string op =
  match op with
    Andop ->  " and "
  | Orop  ->  " or "
  | Oldop ->  "old "
  | Notop ->  "not "
  | Inop  ->  " in "
  | Notinop -> " /in "
  | Asop   -> " as "
  | _     ->
      let s,_,_ = operator_data op
      in
      s


type is_do_block = bool

type application_mode =
    AMmath
  | AMoo
  | AMop


type expression0 =
    Identifier    of int
  | Expanon
  | Expnumber     of int
  | ExpResult
  | Exptrue
  | Expfalse
  | Expparen      of expression
  | Exparrow      of entities list withinfo * expression
  | Expagent      of entities list withinfo * return_type * compound * expression
  | Expop         of operator
  | Funapp        of expression * expression list * application_mode
  | Expset        of expression
  | Exppred       of entities list withinfo * expression
  | Expindset     of entities list withinfo * expression list
  | Tupleexp      of expression * expression
  | Typedexp      of expression * type_t withinfo
  | Expcolon      of expression * expression
  | Expif         of expression * expression * expression
  | Expas         of expression * expression
  | Expinspect    of expression * (expression*expression) list
  | Expquantified of quantifier * entities list withinfo * expression

and
      expression = expression0 withinfo
and
      compound        = expression list
and
      implementation  =
    Impdeferred
  | Impbuiltin
  | Impevent
  | Impdefined of locals option * is_do_block * compound

and local_declaration =
    Unassigned of entities list
  | Assigned   of entities list * expression
  | Local_feature of int * entities list * return_type * feature_body

and locals          = local_declaration list

and feature_body = compound * implementation option * compound



let expression_list_rev (e:expression): expression list =
  (* break up a tuple into a list, note: the returned list contains the
     elements reversed *)
  let rec list (e:expression) (lst:expression list): expression list =
    match e.v with
      Tupleexp (a,b) ->
        list b (a::lst)
    | _ -> e::lst
  in
  list e []


let expression_list_length (e:expression): int =
  List.length (expression_list_rev e)


let expression_list (e:expression): expression list =
  (* break up a tuple into a list.*)
  List.rev (expression_list_rev e)



let expression_of_list (lst:expression list): expression =
  let rec tuple lst =
    match lst with
      [a]      -> a
    | [a;b]    ->
        withinfo UNKNOWN (Tupleexp (a,b))
    | a :: lst ->
        withinfo UNKNOWN (Tupleexp (a,tuple lst))
    | _ ->
        assert false
  in
  tuple lst

let rec string_of_expression  ?(wp=false) (e:expression) =
  let strexp e         = string_of_expression ~wp:wp e
  and withparen str wp = if wp then "(" ^ str ^ ")" else str
  in
  let strexplst lst =
    String.concat "," (List.map strexp lst)
  in
  match e.v with
    Identifier id -> ST.string id

  | Expanon       -> "_"

  | Expnumber num  -> ST.string num

  | ExpResult     -> "Result"

  | Exptrue       -> "true"

  | Expfalse      -> "false"

  | Expparen e   -> "(" ^ (strexp e) ^")"

  | Exparrow  (l,e) ->
      "(" ^ (string_of_formals l.v) ^ ")->" ^ (string_of_expression e)

  | Expagent (l,rt,pres,exp) ->
      "agent(" ^ (string_of_formals l.v) ^ ")" ^ (string_of_return_type rt) ^
      " require " ^ (string_of_compound pres) ^
      " ensure -> " ^ (string_of_expression exp) ^
      " end"
  | Expop op     -> "(" ^ (operator_to_rawstring op) ^ ")"

  | Funapp ({v = Expop Bracketop; i = _},args,_) ->
      begin
        match args with
          tgt ::args ->
            (strexp tgt) ^ "[" ^ (strexplst args) ^ "]"
        | _ ->
            assert false (* Cannot happen *)
      end
  | Funapp ({v = Expop op; i = _},args,AMop) ->
      begin
        match args with
          [e] ->
            withparen ((operator_to_string op) ^ (strexp e)) wp
        | [e1;e2] ->
            withparen ((strexp e1) ^ (operator_to_string op) ^ (strexp e2)) wp
        | _ ->
            assert false (* Cannot happen *)
      end
  | Funapp (f,args,AMoo) ->
      begin
        match args with
          tgt::args ->
            withparen ((strexp tgt) ^ "." ^ (strexp f)) wp ^
            (if args = [] then ""
            else "(" ^ strexplst args ^ ")" )
        | _ ->
            assert false (* Cannot happen *)
      end
  | Funapp (f,args,_) ->
      (strexp f) ^ "(" ^ (strexplst args) ^ ")"

  | Expset s ->
      "{" ^  (strexp s) ^ "}"

  | Exppred (elist,exp) ->
      "{" ^ (string_of_formals elist.v) ^ ":" ^ (string_of_expression exp)^ "}"

  | Expindset (elist,rules) ->
      "{" ^ "(" ^ string_of_formals elist.v ^ "):" ^
      (string_of_list rules string_of_expression ",") ^ "}"

  | Typedexp (e,t) ->
      withparen ((strexp e) ^ ":" ^ (string_of_type t.v)) wp

  | Tupleexp (e1,e2) ->
      "(" ^ (strexp e1) ^ "," ^ (strexp e2) ^ ")"

  | Expcolon (e1,e2) ->
      withparen ((strexp e1) ^ ":" ^ (strexp e2)) wp

  | Expif (cond,e1,e2) ->
      "if " ^
      (string_of_expression cond) ^
      " then " ^
      (string_of_expression e1) ^
      " else " ^
      (string_of_expression e2)

  | Expas (e,pat) ->
      string_of_expression e ^
      " as " ^
      string_of_expression pat

  | Expinspect (inspexp,caselist) ->
      "inspect "
      ^ (string_of_expression inspexp)
      ^ (string_of_list
           caselist
           (fun (pat,exp) ->
             " case " ^ (string_of_expression pat)
             ^ " then " ^  (string_of_expression exp))
           "")
      ^ " end"
  | Expquantified (q,elist,exp) ->
      (match q with Universal -> "all" | Existential -> "some")
      ^ "(" ^ (string_of_formals elist.v) ^ ") "  ^ (string_of_expression exp)

and string_of_compound comp =
  string_of_list comp string_of_expression ";"

and string_of_locals loc =
  string_of_list
    loc
    (fun el ->
      match el with
        Unassigned elist -> string_of_formals elist
      | Assigned (elist,exp) ->
          (string_of_formals elist) ^ ":=" ^ (string_of_expression exp)
      | Local_feature (id,elist,rt,body) ->
          (ST.string id) ^ "(" ^ (string_of_formals elist) ^")"
          ^
          (match rt with Some t ->
            let tp,exclam,ghost = t.v
            in
            (if exclam then "!:" else ":")
            ^ (if ghost then " ghost " else "")
            ^ (string_of_type tp)
          | None -> "")
          ^ " " ^ (string_of_body body)
    )
    ";"

and string_of_implementation imp =
  match imp with
    Impdeferred -> "deferred"
  | Impbuiltin  -> "note built_in"
  | Impevent    -> "note event"
  | Impdefined (loc_opt,dochk,comp) ->
      (match loc_opt with
        None -> ""
      | Some loc -> "local " ^ (string_of_locals loc) ^ " ")
      ^
        (if dochk then "do " else "check ")
      ^
        (string_of_compound comp)

and string_of_body b =
  let rl,imp_opt,el = b in
  (" require " ^ (string_of_compound rl))
  ^
    (match imp_opt with
      Some imp -> (string_of_implementation imp)
    | None -> "")
  ^ (" ensure " ^ (string_of_compound el))
  ^ " end"




(* Header mark *)

type header_mark = No_hmark | Mutable_hmark | Deferred_hmark

let hmark2string (hm:header_mark) =
  match hm with
    No_hmark -> ""
  | Mutable_hmark -> "immutable"
  | Deferred_hmark  -> "deferred"

let hmark2string_wblank (hm:header_mark) =
  let str = hmark2string hm in
  if str = "" then str
  else str ^ " "


(* Features *)

type classname = (int list * int) withinfo

let string_of_classname (path:int list) (cn:int): string =
  if path = [] then
    ST.string cn
  else
    let strlst = List.rev_map ST.string path in
    (String.concat "." strlst) ^ "." ^ (ST.string cn)


type formal_generics = int list withinfo

type feature_name =
    FNname of int
  | FNoperator of operator
  | FNtrue
  | FNfalse
  | FNnumber of int


module Feature_map = Map.Make(struct
  type t = feature_name
  let compare = Pervasives.compare
end)


let feature_name_to_string (fn:feature_name): string =
  match fn with
    FNname i | FNnumber i -> ST.string i
  | FNoperator op -> operator_to_rawstring op
  | FNtrue ->  "true"
  | FNfalse -> "false"

type rename_item = feature_name * feature_name

type parent = bool * type_t withinfo * rename_item list

type inherit_clause = parent list

type create_clause = (feature_name withinfo * entities list) list withinfo


type source_proof =
    SP_Axiom
  | SP_Deferred
  | SP_Proof of proof_step list * info_proof_expression option

and  info_proof_expression = proof_expression withinfo

and proof_expression =
    PE_If of
      expression * source_proof * info * source_proof
  | PE_Guarded_If of
      expression * source_proof * expression * source_proof
  | PE_Inspect of
      expression * one_case list
  | PE_Existential of
      entities list withinfo * expression * source_proof
  | PE_Contradiction of
      expression * source_proof
  | PE_Transitivity of expression list

and  one_case = expression * source_proof

and proof_step =
    PS_Simple of expression
  | PS_Structured of
      entities list withinfo * compound * expression * source_proof



type declaration =
    Theorem of
      entities list withinfo * compound * compound * source_proof
  | Named_feature of
      feature_name withinfo
        * entities list withinfo
        * return_type
        * bool  (* is function without explicit return type *)
        * feature_body option
        * expression option
  | Formal_generic of int withinfo * classname
  | Class_declaration of
      header_mark withinfo
        * int withinfo option  (* optional class variable *)
        * classname
        * formal_generics
        * create_clause
  | Inheritance_declaration of
      header_mark withinfo
        * classname
        * formal_generics
        * inherit_clause
  | Class_list   of declaration list withinfo
  | Feature_list of declaration list withinfo


type library_name = int list (* reversed *)

type module_name = int * library_name


let string_of_library (lib:library_name): string =
  String.concat "." (List.rev_map ST.string lib)


let string_of_module ((m,lib):module_name): string =
  if lib = [] then
    ST.string m
  else
    let libstr = string_of_library lib in
    libstr ^ "." ^ (ST.string m)


module Library_map = Map.Make(struct
  type t = library_name
  let compare = Pervasives.compare
end)

module Module_map = Map.Make(struct
  type t = module_name
  let compare = Pervasives.compare
end)



type use_block = (int*int list) withinfo list

type module_declaration = use_block * declaration list
