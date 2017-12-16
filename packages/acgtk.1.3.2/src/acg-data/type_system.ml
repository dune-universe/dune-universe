(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

open UtilsLib
open Logic.Abstract_syntax
open Logic.Lambda

let get_location = function
  | Abstract_syntax.Var (_,l) -> l
  | Abstract_syntax.Const (_,l) -> l
  | Abstract_syntax.Abs (_,_,_,l) -> l 
  | Abstract_syntax.LAbs (_,_,_,l) -> l
  | Abstract_syntax.App (_,_,l) -> l


module type SIG_ACCESS =
sig
  exception Not_found
  type t

  val expand_type :  Lambda.stype -> t -> Lambda.stype 
  val find_term : string -> t -> Lambda.term *Lambda.stype
  val type_to_string : Lambda.stype -> t -> string
  val term_to_string : Lambda.term -> t -> string
(*  val id_to_string : t -> int -> Abstract_syntax.syntactic_behavior*string *)
end


module Type_System =
struct

	
	
  module Make(Signature:SIG_ACCESS) =
  struct

  exception Not_functional_type
  exception Functional_type of Abstract_syntax.abstraction
  exception Not_normal_term
  exception Vacuous_abstraction of (string * Abstract_syntax.location * Abstract_syntax.location)
  exception Non_empty_linear_context of (string*Abstract_syntax.location)


  exception Not_linear of (Abstract_syntax.location * Abstract_syntax.location)
  exception Type_mismatch of (Abstract_syntax.location * Lambda.stype * Lambda.stype)

    
(*  let rec expand_type ty sg = 
    match ty with
      | Lambda.Atom _ -> ty
      | Lambda.DAtom i -> expand_type (Signature.unfold_type_definition i sg) sg
      | Lambda.LFun (ty1,ty2) -> Lambda.LFun(expand_type ty1 sg,expand_type ty2 sg)
      | Lambda.Fun (ty1,ty2) -> Lambda.Fun(expand_type ty1 sg,expand_type ty2 sg)
      | _ -> failwith "Not yet implemented" *)

  let decompose_functional_type ty sg =
    match Signature.expand_type ty sg with
      | Lambda.LFun (ty1,ty2) -> ty1,ty2,Abstract_syntax.Linear
      | Lambda.Fun (ty1,ty2) -> ty1,ty2,Abstract_syntax.Non_linear
(*      | Lambda.DAtom i -> decompose_functional_type (Signature.unfold_type_definition i sg) sg *)
      | _ -> raise Not_functional_type



  (* [get_typing x loc typing_env] returns [l,t,e] where [l] is the
     level of [x] and [t] its type in the typing environment
     [typing_env] when [x] is a variable located at [loc]. [e] is the
     new environment where [x] as been marked as used at location
     [l]. If [x] has been used alread once (this is marked by the
     [Some l], 3rd projection in the association list), the function
     raises [Not_linear l] where [l] is the location of the former
     usage of [x]. If [x] is not in the typing environment
     [typing_env], it raises Not_found *)
	    
  let get_typing x loc lst =
    let rec get_typing_aux lst k =
      match lst with
	| [] -> raise Not_found
	| (s,(level,ty,None))::tl when s=x->
	    k(level,ty,((s,(level,ty,Some loc))::tl))
	| (s,(_,_,Some l))::_ when s=x -> raise (Not_linear (l,loc))
	| hd::tl -> get_typing_aux tl (fun (level,ty,r) -> k (level,ty,(hd::r))) in
      get_typing_aux lst (fun (level,ty,env) -> (level,ty,env))
	
	
  let put_env x l lst =
    let rec change_env_aux lst k =
      match lst with
	| [] -> raise Not_found
	| (s,_)::tl when s=x -> k ((s,l)::tl)
	| hd::tl -> change_env_aux tl (fun r -> k (hd::r)) in
      change_env_aux lst (fun x -> x)
	
	
  let var i = function
    | Abstract_syntax.Linear -> Lambda.LVar i
    | Abstract_syntax.Non_linear -> Lambda.Var i 


  let print_env (l_env,env,lin) f =
    let () = Printf.printf "Linear environment:\n%s\n" (Utils.string_of_list "\n" (fun (x,(l,ty,u)) -> Printf.sprintf "%s (%d): %s (%s)" x l (Lambda.type_to_string ty f) (match u with | None -> "Not used" | Some _ -> "Used")) l_env) in
    let () = Printf.printf "Non linear environment:\n%s\n" (Utils.string_of_list "\n" (fun (x,(l,ty)) -> Printf.sprintf "%s (%d): %s" x l (Lambda.type_to_string ty f) ) env) in
      Printf.printf "Next usage:\n%s\n" (Utils.string_of_list "\n" (fun (x,ab) -> Printf.sprintf "%s : %s" x (match ab with | Abstract_syntax.Linear -> "Linear" | _ -> "Non Linear")) lin)
	
  type typing_env_content = 
    | Delay of (int -> int -> Abstract_syntax.location -> Lambda.term)
    | Eval of (int -> int -> Abstract_syntax.location -> (Lambda.term * typing_env_content))

  type typing_environment =
      {linear_level:int; (* The depth of the term with respect to the
			    number of linear abstraction. Starting at 0 *)
       level:int; (* The depth of the term with respect to the number
		     of non linear abstraction. Starting at 0 *)
       env : (typing_env_content*Lambda.stype*Abstract_syntax.abstraction) Utils.StringMap.t;
       wrapper : (Abstract_syntax.location*Lambda.stype*Abstract_syntax.location) option}


  let remove_lin_context ({env=e} as tenv) =
    {tenv with
       env=Utils.StringMap.fold
	(fun k ((_,_,abs) as v) acc ->
	   match abs with
	     | Abstract_syntax.Linear -> acc
	     | Abstract_syntax.Non_linear -> Utils.StringMap.add k v acc)
	e
	Utils.StringMap.empty}

  let insert_lin_var (ty:Lambda.stype) (env:typing_environment) =
    Eval (fun l_level _ loc -> 
(*	    let i = (l_level - 1 - env.linear_level) in
	    let () = Printf.printf "Inserting variable %d - 1 - %d = %d\n%!" l_level env.linear_level i in *)
	      Lambda.LVar (l_level - 1 - env.linear_level),
	    Delay (fun _ _ l -> raise (Not_linear (l,loc)))),
    ty,
    Abstract_syntax.Linear

  let insert_non_lin_var (ty:Lambda.stype) (env:typing_environment) =
    Delay (fun  _ level _ -> Lambda.Var (level - 1 - env.level)),
    ty,
    Abstract_syntax.Non_linear

  let compute (k:typing_env_content) (env:typing_environment) (l:Abstract_syntax.location) =
    match k with
      | Delay f -> f env.linear_level env.level l,Delay f
      | Eval f -> f env.linear_level env.level l


  let get_binding x map = 
    try
      Some (Utils.StringMap.find x map)
    with
      | Not_found -> None

  let replace_binding x v map =
    match v with
      | None -> Utils.StringMap.remove x map
      | Some b -> Utils.StringMap.add x b map




    let typecheck t ty sg =
    let local_expand ty = Signature.expand_type ty sg in
    let rec typecheck_aux t ty (tenv:typing_environment) =
      match t with
	| Abstract_syntax.Var (x,l) -> 
	    (try
	       let f_var,var_type,lin = Utils.StringMap.find x tenv.env in
	       let var,new_f=compute f_var tenv l in
		 match ty with
		   | None ->
		       var,
		       var_type,
		       {tenv with env=Utils.StringMap.add x (new_f,var_type,lin) tenv.env}
		   | Some l_ty when l_ty=local_expand var_type ->
		       var,
		       var_type,
		       {tenv with env=Utils.StringMap.add x (new_f,var_type,lin) tenv.env}
		   | Some l_ty -> raise (Type_mismatch (l,l_ty,var_type))
	     with
	       | Not_found -> raise (Non_empty_linear_context (x,l)))
	| Abstract_syntax.Const (x,l) ->
	    (try
	       let l_term,l_type = Signature.find_term x sg in
	       (match ty with
		 | None -> l_term,l_type,tenv
		 | Some l_ty when (local_expand l_type)=l_ty -> l_term,l_type,tenv
		 | Some l_ty -> raise (Type_mismatch (l,l_ty,l_type)))
	     with
	       | Signature.Not_found -> failwith "Bug")
	| Abstract_syntax.LAbs (x,l_x,u,l_u) ->
	    (match ty with
	       | None -> raise Not_normal_term
	       | Some l_ty ->
		   let b = get_binding x tenv.env in
		     (try
			let ty1,ty2,lin = decompose_functional_type l_ty sg in
			  match lin with
			    | Abstract_syntax.Linear ->
				let u_term,u_type,new_typing_env = 
				  typecheck_aux
				    u
				    (Some (local_expand ty2))
				    {tenv with
				       linear_level=tenv.linear_level+1;
				       env=Utils.StringMap.add x (insert_lin_var ty1 tenv) tenv.env} in
				let f_var,_,_ = Utils.StringMap.find x new_typing_env.env in
				  (try
				     let _ = compute f_var new_typing_env l_x in 
				       (* if the Not_linear exception is not raised, it means the variable
					  was not
					  used in
					  u_term *)
				       raise (Vacuous_abstraction (x,l_x,get_location u))
				   with
				     | Not_linear _ -> Lambda.LAbs(x,u_term),l_ty,{new_typing_env with env= replace_binding x b new_typing_env.env ; linear_level=tenv.linear_level})
			    | Abstract_syntax.Non_linear as l -> raise (Functional_type l)
		      with
			| Not_functional_type 
			| Functional_type Abstract_syntax.Non_linear  ->
			    raise (Error.Error (Error.Type_error (Error.Is_Used (Signature.type_to_string l_ty sg,"\"'a -> 'b\" (linear abstraction)" ),l_u)))))
	| Abstract_syntax.Abs (x,l_x,u,l_u) ->
	    (match ty with
	       | None -> raise Not_normal_term
	       | Some l_ty ->
		   let b = get_binding x tenv.env in
		     (try
			let ty1,ty2,lin = decompose_functional_type l_ty sg in
			  match lin with
			    | Abstract_syntax.Non_linear ->
				let u_term,u_type,new_typing_env = 
				  typecheck_aux
				    u
				    (Some (local_expand ty2))
				    {tenv with
				       level=tenv.level+1;
				       env=Utils.StringMap.add x (insert_non_lin_var ty1 tenv) tenv.env} in
				  Lambda.Abs(x,u_term),l_ty,{new_typing_env with env= replace_binding x b new_typing_env.env;level=tenv.level}
			    | Abstract_syntax.Linear as l -> raise (Functional_type l)
		      with
			| Not_functional_type 
			| Functional_type _ -> raise (Error.Error (Error.Type_error (Error.Is_Used (Signature.type_to_string l_ty sg,"\"'a => 'b\" (non-linear abstraction)"),l_u)))))
	| Abstract_syntax.App (u,v,l) ->
	    let u_term,u_type,new_typing_env = 
	      try
		typecheck_aux u None tenv
	      with
		| Not_normal_term -> raise (Error.Error (Error.Type_error (Error.Not_normal,l))) in
	    let ty1,ty2,lin = 
	      try 
		decompose_functional_type u_type sg
	      with 
		| Not_functional_type -> let u_loc = get_location u in
		    raise (Error.Error (Error.Type_error (Error.Is_Used (Signature.type_to_string u_type sg,"\"'a -> 'b\" or \"'a => 'b\" in order to enable application"),(fst u_loc,snd u_loc)))) in
	    let v_term,_,new_new_typing_env = 
	      match lin with
		| Abstract_syntax.Linear -> typecheck_aux v (Some (local_expand ty1)) new_typing_env
		| Abstract_syntax.Non_linear ->
		    let non_lin_env = remove_lin_context new_typing_env in
		      (*			let () = Printf.printf "Inserting wrapper\n%!" in *)
		    let wrapper = get_location u,u_type,get_location v in
		      try
			let v_t,v_ty,{wrapper=w} =
			  typecheck_aux v (Some (local_expand ty1)) {non_lin_env with wrapper=Some wrapper} in
			  v_t,v_ty,{new_typing_env with wrapper=w} 
		      with
			| Non_empty_linear_context (x,l) ->
			    let func_loc,func_st,v_loc = match tenv.wrapper with
			      | None -> wrapper
			      | Some a  -> a in
			      (*		    let v_loc = get_location v in*)
			      raise (Error.Error (Error.Type_error (Error.Non_empty_context (x,l,func_loc,Signature.type_to_string func_st sg),v_loc)))
	    in
	      match ty with
		| None -> Lambda.App (u_term,v_term),ty2,new_new_typing_env
		| Some l_ty when l_ty=local_expand ty2 -> Lambda.App (u_term,v_term),l_ty,new_new_typing_env
		| Some l_ty -> raise (Type_mismatch (l,l_ty,ty2))  in 
      try
	let t_term,t_type,(_:typing_environment) = 
	  typecheck_aux t (Some (local_expand ty)) {linear_level=0;level=0;env=Utils.StringMap.empty;wrapper=None} in    
	LOG "Type-checked %s : %s"  (Signature.term_to_string t_term sg ) (Signature.type_to_string t_type sg ) LEVEL TRACE ;
	LOG "Type-checked %s : %s"  (Lambda.raw_to_string t_term ) (Lambda.raw_type_to_string t_type ) LEVEL TRACE ;
	LOG "Type-checked %s : %s"  (Lambda.raw_to_caml t_term ) (Lambda.raw_type_to_caml t_type ) LEVEL TRACE ;
	t_term
      with
	| Type_mismatch ((p1,p2),t1,t2) -> raise (Error.Error (Error.Type_error (Error.Is_Used (Signature.type_to_string t1 sg,Printf.sprintf "\"%s\"" (Signature.type_to_string t2 sg)),(p1,p2))))
	| Not_linear ((s1,e1),(s2,e2)) -> raise (Error.Error (Error.Type_error (Error.Two_occurrences_of_linear_variable (s2,e2),(s1,s1))))
	| Vacuous_abstraction (x,l1,l2) -> raise (Error.Error (Error.Type_error (Error.Vacuous_abstraction (x,l2),l1)))

  end

end
