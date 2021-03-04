open Core_kernel
open Ast

type env =
  { file : string;
    includes : string list;
    defines : (string * string) list }
let empty file includes defines =
   { file; includes; defines }
let is_defined env m =
  not (Option.is_none (List.Assoc.find ~equal:String.equal env.defines m))
let define env m n =
  if is_defined env m then env
  else { env with defines = List.Assoc.add ~equal:String.equal env.defines m n }
let undefine env m =
  { env with
    defines = List.Assoc.remove ~equal:String.equal env.defines m }
let get_file env = env.file
let set_file env file = { env with file }
let get_includes env = env.includes
let get_defines env = env.defines

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

module type F = sig
  val exists : string -> bool
  val load : string -> string
end

module type S = sig
  include F
  val preprocess : env -> string -> string -> string * env
end

module Make(F:F) = struct
  include F
  let rec eval (env:env) (buf:Buffer.t) (term:term) : env =
    let current = get_file env in
    match term with
    | String(s) ->
       Buffer.add_string buf (Printf.sprintf "\"%s\"" s);
       env
    | Text(s) ->
       begin 
         match List.Assoc.find (get_defines env) s ~equal:String.equal with
         | Some s' -> 
            Buffer.add_string buf s'
         | None -> 
            Buffer.add_string buf s
       end;
       env
    | Include(line,search,file) ->
       let path = match resolve (get_includes env) file with 
         | None -> failwith ("Error: " ^ file ^ " could not be found")
         | Some path -> path in
       let contents = F.load path in  
       let env = set_file env file in
       let str,env = preprocess env path contents in
       Buffer.add_string buf str;
       let env = set_file env current in
       Buffer.add_string buf "\n";
       Buffer.add_string buf (Printf.sprintf "#line %d \"%s\" %d\n" line current 2);
       env
    | Define(m,b) ->
       let env = define env m b in
       Buffer.add_string buf "\n";
       env
    | Undef(m) ->
       let env = undefine env m in
       Buffer.add_string buf "\n";
       env
    | IfDef(macro,line_tru,tru,line_fls,fls,line_end) ->
       let b = is_defined env macro in
       cond env buf b line_tru tru line_fls fls line_end
    | IfNDef(macro,line_tru,tru,line_fls,fls,line_end) ->
       let b = not(is_defined env macro) in
       cond env buf b line_tru tru line_fls fls line_end
    | If(test,line_tru, tru, line_fls, fls, line_end) ->
       let b = Int64.(zero = eval_test env test) in
       cond env buf b line_tru tru line_fls fls line_end 
       
  and cond env buf b line_tru tru line_fls fls line_end =
    let current = get_file env in
    let env =
      if b then
        begin
          Buffer.add_string buf (Printf.sprintf "#line %d \"%s\"\n" line_tru current);
          List.fold_left ~init:env ~f:(fun env term -> eval env buf term) tru
        end
      else
        begin
          Buffer.add_string buf (Printf.sprintf "#line %d \"%s\"\n" line_fls current);
          List.fold_left ~init:env ~f:(fun env term -> eval env buf term) fls
        end in
    Buffer.add_string buf (Printf.sprintf "#line %d \"%s\"\n" line_end current);
    env
    
  and resolve includes (filename:string) : string option = 
    match includes with
      | [] ->
        None
      | h::t ->
         let path = Filename.concat h filename in
         if F.exists path then Some path 
         else resolve t filename 

  and preprocess (env:env) (filename:string) (contents:string) : string * env =
    let buf = Buffer.create 101 in
    let () = Buffer.add_string buf (Printf.sprintf "#line %d \"%s\" %d\n" 1 filename 1) in
    let lexbuf = Lexing.from_string contents in
    let () = Prelexer.reset filename in
    let prelex_contents = Prelexer.lex lexbuf in
    let lexbuf = Lexing.from_string prelex_contents in
    let terms =
      try 
        let r = Parser.program Lexer.token lexbuf in
        r
      with _ -> 
        failwith ("Error parsing " ^ filename ^ " : " ^ string_of_int (!Lexer.current_line)) in
    let env = set_file env filename in
    let env = List.fold_left ~init:env ~f:(fun env term -> eval env buf term) terms in
    (Buffer.contents buf, env)
end

module FileSystem = Make(struct
  let exists path = Sys.file_exists path
  let load filename = In_channel.(with_file filename ~f:input_all) 
end)

module Web = Make(struct
  let exists = function
    | "/core.p4" 
    | "/v1model.p4" -> 
      true
    | str -> 
      false
  let load = function
    | "/core.p4" -> Bake.core_p4_str
    | "/v1model.p4" -> Bake.v1model_p4_str
    |  fn -> failwith (fn ^ ": not found")
end)
