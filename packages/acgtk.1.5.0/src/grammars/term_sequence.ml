open Logic.Abstract_syntax
open AcgData.Environment
open AcgData

module Log = (val Logs.src_log (Logs.Src.create "ACGtkLib.term_sequence" ~doc:"logs ACGtkLib term_sequence events") : Logs.LOG) 
       
type token =
  | Term of (Abstract_syntax.term * Abstract_syntax.location)
  | Op of ( Abstract_syntax.term * Abstract_syntax.syntactic_behavior * Abstract_syntax.location)

let rec abs_term_to_string _ = function
  | Abstract_syntax.Var (x,_)
    | Abstract_syntax.Const (x,_) -> x
  | Abstract_syntax.Abs (x,_,t,_)-> Printf.sprintf "(Lambda %s. %a)" x abs_term_to_string t
  | Abstract_syntax.LAbs (x,_,t,_)-> Printf.sprintf "(lambda %s. %a)" x abs_term_to_string t
  | Abstract_syntax.App (t1,t2,_) -> Printf.sprintf "(%a %a)" abs_term_to_string t1 abs_term_to_string t2


let to_string = abs_term_to_string ()
                                   
let tok_to_string = function
  | Term (t,_) 
  | Op (t,_,_) -> to_string t
            
let is_infix = function
  | Abstract_syntax.Infix _ -> true
  | _ -> false

let is_prefix = function
  | Abstract_syntax.Prefix -> true
  | _ -> false

let lower_than f1 f2 =
  match f1,f2 with
  | (Abstract_syntax.Infix (_,p1)), (Abstract_syntax.Infix (_,p2)) -> p1 < p2
  | _ -> false
    
            
let next = function
  | [] -> None,[]
  | a::tl -> Some a,tl

let new_loc (s,_) (_,e) = (s,e)
                      
let rec parse_sequence_aux stack token stream sg =
  Log.debug
    (fun m ->
      m
        "stack: '%s', token: '%s', stream: '%s'\n"
        (UtilsLib.Utils.string_of_list " ; " tok_to_string stack)
        (match token with Some t -> tok_to_string t | None -> "None")
        (UtilsLib.Utils.string_of_list " ; " tok_to_string stream));
  match stack,token with
  | [] , Some t ->
     (* shift to initiate the process *)
     let token',stream' = next stream in
     parse_sequence_aux (t::stack) token' stream' sg
  | [Term (t,loc)],None ->
     (* sucessful parse *)
     t,loc
  | [Term _],Some tok ->
     (* shift *)
     let token',stream' = next stream in
     parse_sequence_aux (tok::stack) token' stream' sg
  | (Term (t,loc_t))::(Op (t_op,f,loc_o))::tl,_ when is_prefix f ->
     (* reduce: prefix operators have the highest precedence *)
     let loc = new_loc loc_o loc_t in
     let new_term = Abstract_syntax.App (t_op,t,loc) in
     parse_sequence_aux ((Term (new_term,loc))::tl) token stream sg

(*  | (Term _)::(Term _)::tl,Some ((Op _) as tok) ->
     (* shift: prefix operators have higher precedence than application *)
     (* application has higher precedence than infix operators *)
     let token',stream' = next stream in 
     parse_sequence_aux (tok::stack) token' stream' sg *)

  | (Term (t2,l2))::(Term (t1,l1))::tl,_  ->
     (* reduce: application can be perfomed *)
     (* prefix operators have higher precedence than application *)
     (* application has higher precedence than infix operators *)
     let loc = new_loc l1 l2 in
     let term = Abstract_syntax.App (t1,t2,loc) in
     parse_sequence_aux ((Term (term,loc))::tl) token stream sg

  | (Op (o1,f1,l_o1))::(Op (_,f2,_))::_,Some tok when (is_prefix f2) && (is_prefix f1)->
     (* shift *)
     let token',stream' = next stream in
     parse_sequence_aux (tok::stack) token' stream' sg

                   
  | (Op (o1,f1,l_o1))::(Op (_,f2,_))::_,_ when (is_prefix f2) ->
     raise (Error.(Error (Parse_error (Infix_missing_first_arg (to_string o1),l_o1))))

           
(*  | (Op (_,f1,_))::(Op (o2,f2,l_o2))::_,_ when (is_prefix f2) && (is_infix f1) ->
     raise (Error.(Error (Parse_error (Infix_missing_second_arg (to_string o2),l_o2)))) *)

  | (Op _)::tl,Some tok ->
     (* shift. It makes sense to shift *)
     let token',stream' = next stream in
     parse_sequence_aux (tok::stack) token' stream' sg

  | (Term (t2,l2))::(Op (o1,f1,l_o1))::(Term (t1,l1))::tl,Some (Op (o2,f2,l_o2)) when
         (is_infix f1) && (is_infix f2) && (lower_than f2 f1) ->
     (* reduce: there are two different operators, *)
     (* and the first one has the highest precedence *)
     let loc = new_loc l1 l_o1 in
     let partial_term = Abstract_syntax.App (o1,t1,loc) in
     let loc = new_loc loc l2 in
     let new_term = Abstract_syntax.App (partial_term,t2,loc) in     
     parse_sequence_aux ((Term (new_term,loc))::tl) token stream sg

  | (Term _)::(Op (_,f1,_))::(Term _)::_,Some ((Op (_,f2,_)) as tok) when
         (is_infix f1) && (is_infix f2) && (lower_than f1 f2) ->
     (* shift: there are two different operators, *)
     (* and the second one has the highest precedence *)
     let token',stream' = next stream in
     parse_sequence_aux (tok::stack) token' stream' sg

  | (Term (t2,l2))::(Op (o1,f1,l_o1))::(Term (t1,l1))::tl,Some ((Op (_,f2,_)) as tok) when
         (is_infix f1) && (f1 = f2) ->
     (* there is a sequence with the same operator *)
     (match f1 with
      | Infix (Left,_) ->
         (* reduce: it is left associative *)
         let loc = new_loc l1 l_o1 in
         let partial_term = Abstract_syntax.App (o1,t1,loc) in
         let loc = new_loc loc l2 in
         let new_term = Abstract_syntax.App (partial_term,t2,loc) in     
        parse_sequence_aux ((Term (new_term,loc))::tl) token stream sg
     | Infix (Right,_) ->
         (* shift: it is right associative *)
        let token',stream' = next stream in
        parse_sequence_aux (tok::stack) token' stream' sg
     | Infix (NonAss,_) ->
        (* error: since it is not associative, there *)
        (* should not be such a sequence *)
        raise (Error.(Error (Parse_error (Not_associative (to_string o1),l_o1))))
     | _ -> failwith "Bug: shouldn't happen")

  | (Term (t2,l2))::(Op (o1,f1,l_o1))::(Term (t1,l1))::tl, Some ((Term _) as t) when
         (is_infix f1) ->
     (* shift: application has precedence over the operator *)
(*     let loc = new_loc l1 l_o1 in
     let partial_term = Abstract_syntax.App (o1,t1,loc) in
     let loc = new_loc loc l2 in
     let new_term = Abstract_syntax.App (partial_term,t2,loc) in     
     parse_sequence_aux ((Term (new_term,loc))::tl) token stream sg *)
     let token',stream' = next stream in
     parse_sequence_aux  (t::stack) token' stream' sg

  | (Term (t2,l2))::(Op (o1,f1,l_o1))::(Term (t1,l1))::tl,None when
         (is_infix f1)   ->
     (* reduce *)
     let loc = new_loc l1 l_o1 in
     let partial_term = Abstract_syntax.App (o1,t1,loc) in
     let loc = new_loc loc l2 in
     let new_term = Abstract_syntax.App (partial_term,t2,loc) in     
     parse_sequence_aux ((Term (new_term,loc))::tl) token stream sg
  (*     failwith (Printf.sprintf "Parse error on \"%s\"" o1) *)
  (*     raise (Error.(Error (Parse_error (Not_infix (to_string o1),l_o1))))*)
  | (Term _)::(Op _)::(Term _)::_ , Some (Op _) -> failwith "Bug: Should not happen"
  | (Term _)::(Op (o1,_,l_o1))::(Term _)::_ , _ ->
     (* o1 is not infix *)
     raise (Error.(Error (Parse_error (Not_infix (to_string o1),l_o1))))

  | (Term _)::(Op (o1,_,l_o1))::(Op _)::_,_  ->
     (*     failwith (Printf.sprintf "Parse error on \"%s\"" o1) *)
     raise (Error.(Error (Parse_error (Infix_missing_first_arg (to_string o1),l_o1))))

  | (Term _)::[(Op (o1,_,l_o1))],_  ->
     (*     the case where o1 is infox is already matched. Then o1 is infix *)
     raise (Error.(Error (Parse_error (Infix_missing_first_arg (to_string o1),l_o1))))

  | (Op (o,f,l))::_,None when (is_infix f) ->
     raise (Error.(Error (Parse_error (Infix_missing_second_arg (to_string o),l))))

  | (Op (o,f,l))::_,None  ->
     (* Op is necesseraly prefix *)
     raise (Error.(Error (Parse_error (Prefix_missing_arg (to_string o),l))))

  | [],None ->
          failwith "Bug: an empty list should not be parsed"


let parse_sequence stream =
  let tok,stream' = next stream in
  parse_sequence_aux [] tok stream'
