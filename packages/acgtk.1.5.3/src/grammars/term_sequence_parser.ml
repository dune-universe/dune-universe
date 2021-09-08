type location = Lexing.position*Lexing.position


type associativity =
  | Left
  | Right
  | NonAss

type fixity =
  | Prefix
  | Infix of (int * associativity)

                                  
type term =
  | Var of string
  | Cst of string
  | App of (term*term)
  | Abs of (string*term)


let rec to_string = function
  | Var x -> x
  | Cst x -> x
  | App (t,u) -> Printf.sprintf "(%s %s)" (to_string t) (to_string u)
  | Abs (x,t) -> Printf.sprintf "(lambda %s. %s)" x (to_string t)

module SMap = UtilsLib.Utils.StringMap
                                
type sig_info = fixity SMap.t


let test_sig =
  List.fold_left
    (fun acc (name,prec) ->
      SMap.add name prec acc)
    SMap.empty
    [ ("+",Infix (5,Left)) ;
      ("-",Infix (4,Left)) ;
      ("*",Infix (7,Left)) ;
      ("/",Infix (6,Left)) ;
      ("~",Prefix) ;
      ("!",Prefix) ;
    ]
    

let get_fixity sym signature = SMap.find sym signature

type token =
  | Term of term
  | Op of (string * fixity)

let tok_to_string = function
  | Term t -> to_string t
  | Op (x,_) -> x
            
let is_infix = function
  | Infix _ -> true
  | _ -> false

let is_prefix = function
  | Prefix -> true
  | _ -> false

let lower_than f1 f2 =
  match f1,f2 with
  | Infix (p1,_), Infix (p2,_) -> p1 < p2
  | _ -> false
    
            
let next = function
  | [] -> None,[]
  | a::tl -> Some a,tl
            
let rec parse_sequence stack token stream =
  let () =
    Printf.printf "stack: '%s', token: '%s', stream: '%s'\n" (UtilsLib.Utils.string_of_list " ; " tok_to_string stack) (match token with Some t -> tok_to_string t | None -> "None") (UtilsLib.Utils.string_of_list " ; " tok_to_string stream) in
  match stack,token with
  | [] , Some t ->
     (* shift to initiate the process *)
     let token',stream' = next stream in
     parse_sequence (t::stack) token' stream'
  | [Term t],None ->
     (* sucessful parse *)
     t
  | [Term _],Some tok ->
     (* shift *)
     let token',stream' = next stream in
     parse_sequence (tok::stack) token' stream'
  | (Term t)::(Op (o,f))::tl,_ when is_prefix f ->
     (* reduce: prefix operators have the highest precedence *)
     parse_sequence ((Term (App (Cst o,t)))::tl) token stream 

  | (Op (_o,_f))::_tl,Some tok ->
     (* shift. It makes sens to shift *)
     let token',stream' = next stream in
     parse_sequence (tok::stack) token' stream'

  | (Term t2)::(Op (o1,f1))::(Term t1)::tl,Some (Op (_o2,f2)) when
         (is_infix f1) && (is_infix f2) && (lower_than f2 f1) ->
     (* reduce: there are two different operators, *)
     (* and the first one has the highest precedence *)
     parse_sequence ((Term (App (App (Cst o1,t1),t2)))::tl) token stream

  | (Term _t2)::(Op (_o1,f1))::(Term _t1)::_,Some ((Op (_o2,f2)) as tok) when
         (is_infix f1) && (is_infix f2) && (lower_than f1 f2) ->
     (* shift: there are two different operators, *)
     (* and the second one has the highest precedence *)
     let token',stream' = next stream in
     parse_sequence (tok::stack) token' stream'

  | (Term t2)::(Op (o1,f1))::(Term t1)::tl,Some (Op (o2,f2)) when
         (is_infix f1) && (f1 = f2) ->
     (* there is a sequence with the same operator *)
     (match f1 with
      | Infix (_,Left) ->
         (* reduce: it is left associative *)
        parse_sequence ((Term (App (App (Cst o1,t1),t2)))::tl) token stream
                                    
     | Infix (_,Right) ->
         (* shift: it is right associative *)
        let token',stream' = next stream in
        parse_sequence ((Op (o2,f2))::stack) token' stream'

     | Infix (_,NonAss) ->
        (* error: since it is not associative, there *)
        (* should not be such a sequence *)
        failwith (Printf.sprintf "Syntax error: Operator \"%s\" is non-associative, but here is used as associative" o1)

     | Prefix -> failwith "Bug: Shouldn't happen")
                                                  
  | (Term t2)::(Op (o1,f1))::(Term t1)::tl, _ when
         (is_infix f1) ->
     (* reduce: the operator has precedence over application *)
     parse_sequence ((Term (App (App (Cst o1,t1),t2)))::tl) token stream

  | (Term _t2)::(Op (o1,_f1))::_,_  ->
     failwith (Printf.sprintf "Parse error on \"%s\"" o1)
         
  | (Term _t2)::(Term _t1)::_tl,Some (Op (o,f)) ->
     (* shift: the operator will take precedence over application *)
     let token',stream' = next stream in 
     parse_sequence ((Op (o,f))::stack) token' stream'

  | (Term t2)::(Term t1)::tl,Some (Term _) 
  | (Term t2)::(Term t1)::tl,None  ->
     (* reduce: application can be perfomed *)
     parse_sequence ((Term (App(t1,t2)))::tl) token stream
  | _,None ->
     (* unsuccessful parse: no token left and no reduction was performed *)
     failwith "Parse error"
