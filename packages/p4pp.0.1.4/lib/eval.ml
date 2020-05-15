open Core_kernel
open Ast

type env =
  { file : string;
    defines : (string * Int64.t) list }
let empty file =
   { file; defines = [] }
let is_defined env m =
  not (Option.is_none (List.Assoc.find ~equal:String.equal env.defines m))
let define env m =
  if is_defined env m then env
  else { env with defines = List.Assoc.add ~equal:String.equal env.defines m Int64.zero }
let undefine env m =
  { env with
    defines = List.Assoc.remove ~equal:String.equal env.defines m }
let get_file env = env.file
let set_file env file = { env with file }

let rec find includes file =
  match includes with
  | [] ->
     failwith ("Error: " ^ file ^ " could not be found")
  | h::t ->
     let path = Filename.concat h file in
     if Sys.file_exists path then path
     else find t file

let eval_binop (bop:bop) =
  let open Int64 in
  match bop with
  | Add -> ( + )
  | Sub -> ( - )
  | Mult -> ( * )
  | Div -> ( / )
  | Eq -> (fun n1 n2 -> if n1 = n2 then one else zero)
  | Neq -> (fun n1 n2 -> if n1 <> n2 then one else zero)
  | Lt -> (fun n1 n2 -> if n1 < n2 then one else zero)
  | Gt -> (fun n1 n2 -> if n1 > n2 then one else zero)
  | Le -> (fun n1 n2 -> if n1 <= n2 then one else zero)
  | Ge -> (fun n1 n2 -> if n1 >= n2 then one else zero)
  | BAnd -> bit_and
  | BOr -> bit_or
  | BXor -> bit_xor
  | BShl -> (fun n1 n2 -> shift_left n1 (Int64.to_int_exn n2))
  | BShr -> (fun n1 n2 -> shift_right n1 (Int64.to_int_exn n2))
  | And -> (fun n1 n2 -> if n1 <> zero && n2 <> zero then one else zero)
  | Or -> (fun n1 n2 -> if n1 <> zero || n2 <> zero then one else zero)

let eval_uop (uop:uop) =
  let open Int64 in
  match uop with
  | BNot -> bit_not
  | Not -> (fun n -> if n = zero then one else zero)

let rec eval_test (env:env) (test:test) : Int64.t =
  match test with
  | Int(n) -> n
  | Defined(m) ->
     if is_defined env m then Int64.zero
     else Int64.one
  | Ident(m) ->
     Int64.zero
  | BinOp(test1,bop,test2) ->
     eval_binop bop (eval_test env test1) (eval_test env test2)
  | UnOp(uop,test1) ->
     eval_uop uop (eval_test env test1)

let rec eval (includes:string list) (env:env) (buf:Buffer.t) (term:term) (file_io:bool): env =
  let current = get_file env in
  match term with
  | String(s) ->
     Buffer.add_string buf (Printf.sprintf "\"%s\"" s);
     env
  | Text(s) ->
     Buffer.add_string buf s;
     env
  | Include(line,search,file) ->
     let env =
      if file_io then begin
        let path = find includes file in
        let env = set_file env path in
        let env = preprocess_file includes env buf path in
        set_file env current
      end
      else begin
        let path = file in
        let env = set_file env path in
        let contents =
          if String.equal file "core.p4" then Bake.core_p4_str
          else if String.equal file "v1model.p4" then Bake.core_v1_model_str
          else failwith ("Error: " ^ file ^ " could not be found in bake") in
          preprocess_string includes env buf file contents
      end in
     let env = set_file env current in
     Buffer.add_string buf "\n";
     Buffer.add_string buf (Printf.sprintf "#line %d \"%s\" %d\n" line current 2);
    env
  | Define(m) ->
     let env = define env m in
     Buffer.add_string buf "\n";
     env
  | Undef(m) ->
     let env = undefine env m in
     Buffer.add_string buf "\n";
     env
  | IfDef(macro,line_tru,tru,line_fls,fls,line_end) ->
     let b = is_defined env macro in
     cond includes env buf b line_tru tru line_fls fls line_end file_io
  | IfNDef(macro,line_tru,tru,line_fls,fls,line_end) ->
     let b = not(is_defined env macro) in
     cond includes env buf b line_tru tru line_fls fls line_end file_io
  | If(test,line_tru, tru, line_fls, fls, line_end) ->
     let b = Int64.(zero = eval_test env test) in
     cond includes env buf b line_tru tru line_fls fls line_end file_io

and cond includes env buf b line_tru tru line_fls fls line_end file_io =
  let current = get_file env in
  let env =
    if b then
      begin
        Buffer.add_string buf (Printf.sprintf "#line %d \"%s\"\n" line_tru current);
        List.fold_left ~init:env ~f:(fun env term -> eval includes env buf term file_io) tru
      end
    else
      begin
        Buffer.add_string buf (Printf.sprintf "#line %d \"%s\"\n" line_fls current);              List.fold_left ~init:env ~f:(fun env term -> eval includes env buf term file_io) fls
      end in
  Buffer.add_string buf (Printf.sprintf "#line %d \"%s\"\n" line_end current);
  env

and preprocess_string (includes:string list) (env:env) (buf:Buffer.t) (file:string) (file_contents:string) : env =
  let () = Buffer.add_string buf (Printf.sprintf "#line %d \"%s\" %d\n" 1 file 1) in
  let lexbuf = Lexing.from_string file_contents in
  let () = Prelexer.reset file in
  let string = Prelexer.lex lexbuf in
  let lexbuf = Lexing.from_string string in
  let terms =
    try Parser.program Lexer.token lexbuf
    with _ -> failwith ("Error parsing " ^ "typed input" ^ " : " ^ string_of_int (!Lexer.current_line)) in
  List.fold_left ~init:env ~f:(fun env term -> eval includes env buf term false) terms

and preprocess_file (includes:string list) (env:env) (buf:Buffer.t) (file:string) : env =
  let () = Buffer.add_string buf (Printf.sprintf "#line %d \"%s\" %d\n" 1 file 1) in
  let channel = In_channel.create file in
  let lexbuf = Lexing.from_channel channel in
  let () = Prelexer.reset file in
  let string = Prelexer.lex lexbuf in
  let () = In_channel.close channel in
  let lexbuf = Lexing.from_string string in
  let terms =
    try Parser.program Lexer.token lexbuf
    with _ -> failwith ("Error parsing " ^ file ^ " : " ^ string_of_int (!Lexer.current_line)) in
  List.fold_left ~init:env ~f:(fun env term -> eval includes env buf term true) terms
