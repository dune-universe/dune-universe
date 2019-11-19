(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008-2021 INRIA                             *)
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
open Table
open Logic.Lambda
open Logic.Abstract_syntax

type sig_entry =
  | Type_declaration of string * int * Lambda.kind
  | Type_definition of string * int * Lambda.kind * Lambda.stype
  | Term_declaration of string * int * Abstract_syntax.syntactic_behavior * Lambda.stype
  | Term_definition of string * int * Abstract_syntax.syntactic_behavior * Lambda.stype * Lambda.term
      
module Log = (val Logs.src_log (Logs.Src.create "ACGtkLib.signature" ~doc:"logs ACGtkLib signature events") : Logs.LOG) 

module Make(Symbols:TABLE with type key=String.t)(Id:TABLE with type key=int) =
struct


  exception Duplicate_type_definition
  exception Duplicate_term_definition

  exception Not_found

  exception Not_functional_type

  exception Not_yet_implemented

  type entry = sig_entry

  type t = {name:string*Abstract_syntax.location;
	    size:int;
	    terms:entry Symbols.t;
	    types:entry Symbols.t;
            precedence: (float * string option) Symbols.t ; (* indicates the precedence and a link to the previous one *)
            max_pred : float * string option ; (* indicates the greatest precedence and a link to the previous one *)
	    ids:entry Id.t;
	    is_2nd_order:bool;
	    extension_timestamp:float;
	    definition_timestamp:float}

  type term = Lambda.term

  type stype = Lambda.stype
  type data =
    | Type of stype
    | Term of (term*stype)

  let find_term sym {terms=syms} =
    (* TODO : Replace by an assert within IFDEFDEBUG *)
    match Symbols.find sym syms with
    | Term_declaration (x,id,_,const_type)  when sym=x-> Lambda.Const id,const_type
    | Term_declaration _ -> failwith "Bug in find_term" (* x should match the symbol *)
    | Term_definition (x,id,_,const_type,_) when sym=x-> Lambda.DConst id,const_type
    | Term_definition _ -> failwith "Bug in find_term"  (* x should match the symbol *)
    | _ -> failwith "Bug in find_term"  (* x should return a Term, not a type *)
    | exception Symbols.Not_found -> raise Not_found

  let id_to_string {ids=ids} i =
    match Id.find i ids with
      | Type_declaration(s,_,_) -> Abstract_syntax.Default,s
      | Type_definition(s,_,_,_) -> Abstract_syntax.Default,s
      | Term_declaration(s,_,behavior,_) -> behavior,s
      | Term_definition(s,_,behavior,_,_) -> behavior,s


  let type_to_string ty sg = Lambda.type_to_string ty (id_to_string sg)
    
  let term_to_string t sg = Lambda.term_to_string t (id_to_string sg)


  let type_to_formatted_string fmt ty sg = Lambda.type_to_formatted_string fmt ty (id_to_string sg)
    
  let term_to_formatted_string fmt t sg = Lambda.term_to_formatted_string fmt t (id_to_string sg)


  let empty n = 
    let timestamp = Unix.time () in
    {name=n;
     size=0;
     terms=Symbols.empty;
     types=Symbols.empty;
     ids=Id.empty;
     is_2nd_order=true;
     precedence = Symbols.empty;
     max_pred = 0., None ;
     definition_timestamp=timestamp;
     extension_timestamp=timestamp}

  let name {name=n} = n

  let find_atomic_type s {types=syms} = 
    (* TODO : Replace by an assert within IFDEFDEBUG *)
    match Symbols.find s syms with
    | Type_declaration (x,id,_) when x=s -> Lambda.Atom id
    | Type_declaration _ -> failwith "Bug in find_atomic_type" (* x and s should match if something is returned *)
    | Type_definition (x,id,_,_) when x=s -> Lambda.DAtom id
    | Type_definition _ -> failwith "Bug in find_atomic_type"   (* x and s should match if something is returned *)
    | _ -> failwith "Bug in find_atomic_type" (* A type, not a term should be returned *)
    | exception Symbols.Not_found -> failwith "Bug in find_atomic_type"
				    
  let find_type s sg = 
    (* TODO : Replace by an assert within IFDEFDEBUG *)
    match Symbols.find s sg.types with
    | Type_declaration (x,id,_) when x=s -> Lambda.Atom id
    | Type_definition (x,id,_,_) when x=s -> Lambda.DAtom id
    | _ -> failwith "Bug in find_type" (* A type, not a term should be returned and x and s should match  *)
    | exception Symbols.Not_found -> raise Not_found

  let rec convert_type ty ({types=syms} as sg) = 
    match ty with
      | Abstract_syntax.Type_atom (s,l,args) -> find_atomic_type s sg
      | Abstract_syntax.Linear_arrow (ty1,ty2,l) ->
           Lambda.LFun (convert_type ty1 sg,convert_type ty2 sg)
      | Abstract_syntax.Arrow (ty1,ty2,l) -> Lambda.Fun (convert_type ty1 sg,convert_type ty2 sg)

  let abstract_on_dependent_types lst sg=
    List.fold_right
      (fun x acc -> Lambda.Depend(convert_type x sg,acc))
      lst
      Lambda.Type

  let add_sig_type t e ({size=s;types=syms;ids=ids} as sg) =
    try
      (* First perform addition on the functional data structure *)
      let new_symbols = Symbols.add t e syms in
      (* timestamps modified at add_entry function *)
      {sg with
	size=s+1;
	types=new_symbols;
	ids=Id.add s e ids;}
    with
      | Symbols.Conflict -> raise Duplicate_type_definition
      | Id.Conflict  -> raise Duplicate_type_definition

  let add_sig_term t e ({size=s;terms=syms;ids=ids} as sg) =
    try
      (* First perform addition on the functional data structure *)
      let new_symbols = Symbols.add t e syms in
      (* timestamps modified at add_entry function *)
      {sg with size=s+1;terms= new_symbols;ids=Id.add s e ids}
    with
      | Symbols.Conflict -> raise Duplicate_term_definition
      | Id.Conflict  -> raise Duplicate_term_definition


  let rec expand_type ty ({ids=ids} as sg) = 
    match ty with
      | Lambda.Atom _ -> ty
      | Lambda.DAtom i ->
	  (match Id.find i ids with
	     | Type_definition (_,_,_,ty1) -> expand_type ty1 sg
	     | _ -> failwith "Bug in expand type")
      | Lambda.LFun (ty1,ty2) -> Lambda.LFun(expand_type ty1 sg,expand_type ty2 sg)
      | Lambda.Fun (ty1,ty2) -> Lambda.Fun(expand_type ty1 sg,expand_type ty2 sg)
      | _ -> failwith "Not yet implemented"


  let rec expand_term t ({ids=ids} as sg) = 
    match t with
      | (Lambda.Var _| Lambda.LVar _ | Lambda.Const _) -> t
      | Lambda.DConst i ->
	  (match Id.find i ids with
	     | Term_definition (_,_,_,_,u) -> expand_term u sg
	     | _ -> failwith "Bug in expand term")
      | Lambda.Abs (x,u) -> Lambda.Abs (x,expand_term u sg)
      | Lambda.LAbs (x,u) -> Lambda.LAbs (x,expand_term u sg)
      | Lambda.App (u,v) -> Lambda.App (expand_term u sg,expand_term v sg)
      | _ -> failwith "Not yet implemented"

  let unfold_type_definition i ({ids=ids} as sg) = 
    match Id.find i ids with
      | Type_definition (_,_,_,ty1) -> expand_type ty1 sg
      | _ -> failwith "Bug in unfold_type_definition"


  let unfold_term_definition i ({ids=ids} as sg) = 
    match Id.find i ids with
      | Term_definition (_,_,_,_,t) -> expand_term t sg
      | _ -> failwith "Bug in unfold_term_definition" 
	  
	  

  let get_type_of_const_id i ({ids=ids} as sg) =
    match Id.find i ids with
    | Term_declaration (_,_,_,ty) -> expand_type ty sg
    | Term_definition (_,_,_,ty,_) -> expand_type ty sg
    | _ -> failwith "Should be applied only on constants"
    | exception Id.Not_found -> failwith "Bug in get_type_of_const_id"

  let rec decompose_functional_type ty ({ids=ids} as sg) =
    match ty with
      | Lambda.LFun (ty1,ty2) -> ty1,ty2,Abstract_syntax.Linear
      | Lambda.Fun (ty1,ty2) -> ty1,ty2,Abstract_syntax.Non_linear
      | Lambda.DAtom i ->
	  (match Id.find i ids with
	     | Type_definition (_,_,_,ty1) -> decompose_functional_type ty1 sg
	     | _ -> failwith "Bug in decompose_functional_type")
      | _ -> raise Not_functional_type


  let rec get_binder_argument_functional_type x ({terms=terms} as sg) =
    let ty = 
      match Symbols.find x terms with
	| Term_declaration (_,_,_,ty) -> ty
	| Term_definition (_,_,_,ty,_) -> ty
	| _ -> failwith (Printf.sprintf "Bug: Request of the type of the non constant \"%s\"" x) in
      try
	let ty1,ty2,_ = decompose_functional_type ty sg in
	let _,_,lin = decompose_functional_type ty1 sg in
	  Some lin
      with
	| Not_functional_type -> None


	  
	  
	    
  (* We assume here that [term] is well typed and in beta-normal form
     and that types and terms definitions have been unfolded*)
	    
  let eta_long_form term stype sg =
    let expanded_type= expand_type stype sg in
    let expanded_term= expand_term term sg in
    let res = Lambda.eta_long_form expanded_term expanded_type (fun id -> get_type_of_const_id id sg) in
    Log.debug (fun m -> m "term: %s:%s" (term_to_string term sg) (type_to_string stype sg));
    Log.debug (fun m -> m "eta_long_form: %s:%s" (term_to_string res sg) (type_to_string expanded_type sg));
    res
			   

  let unfold t sg = Lambda.normalize (expand_term t sg)
 
  type temp_t=t
  type temp_entry=entry

  module Type_System=Type_system.Type_System.Make(
    struct
      exception Not_found
      type t=temp_t
      type entry=temp_entry
      type stype=Lambda.stype
      let expand_type = expand_type
      let find_term = find_term
      let type_to_string = type_to_string
      let term_to_string = term_to_string
    end)

  let typecheck=Type_System.typecheck
	  
  let stamp_declaration sg = {sg with extension_timestamp = Unix.time ()}

  let stamp_definition sg = {sg with definition_timestamp = Unix.time ()}
			   
  let add_entry e ({size=s} as sg) =
    match e with
      | Abstract_syntax.Type_decl (t,_,Abstract_syntax.K k) -> 
	  stamp_declaration (add_sig_type t (Type_declaration (t,s,abstract_on_dependent_types k sg)) sg)
      | Abstract_syntax.Type_def (t,_,ty,Abstract_syntax.K k) ->
	  stamp_definition (add_sig_type t (Type_definition (t,s,abstract_on_dependent_types k sg,convert_type ty sg)) sg)
      | Abstract_syntax.Term_decl (t,behavior,_,ty) ->
	  let t_type = convert_type ty sg in
	  let sg_is_2nd_order = sg.is_2nd_order && (Lambda.is_2nd_order t_type (fun i -> unfold_type_definition i sg)) in
	  stamp_declaration (add_sig_term t (Term_declaration (t,s,behavior,convert_type ty sg)) {sg with is_2nd_order=sg_is_2nd_order})
      | Abstract_syntax.Term_def (t,behavior,_,term,ty) ->
	  let t_type = convert_type ty sg in
	  let t_term = typecheck term t_type sg in
	    stamp_definition (add_sig_term t (Term_definition (t,s,behavior,t_type,t_term)) sg)
				     
  let is_type s {types=syms} =
    match Symbols.find s syms with
    | Type_declaration _ -> true
    | Type_definition _ -> true
    | _ -> false
    | exception Symbols.Not_found -> false
				       
  let is_constant s {terms=syms;precedence} =
    match Symbols.find s syms with
    | Term_declaration (_,_,behavior,_) -> true,Some behavior
    | Term_definition (_,_,behavior,_,_) -> true,Some behavior
    | _ -> false,None
    | exception Symbols.Not_found -> false,None

  let precedence s {precedence} =
    match Symbols.find s precedence with
    | f,_ -> Some f
    | exception Symbols.Not_found -> None

  let new_precedence ?before id sg =
    match before, sg.max_pred with
    | None, (f, None) ->
       let p = f +. 1. in
       p, {sg with
            max_pred = p, Some id;
            precedence = Symbols.add id (p,None) sg.precedence}
    | None, (f, (Some _ as max)) ->
       let p = f +. 1. in
       p, {sg with
            max_pred = p, Some id;
            precedence = Symbols.add id (p,max) sg.precedence}
    | Some upper_bound, (f, None) ->
       (* Strange to give an upper bound when there is no max. Behaves
          like introducing the first element *)
       let p = f +. 1. in
       p, {sg with
            max_pred = p, Some id;
            precedence = Symbols.add id (p,None) sg.precedence}
    | Some upper_bound, _ ->
       (match Symbols.find upper_bound sg.precedence with
        | f_up, None ->
           let f_down = 0. in
           let p =  (f_up +. f_down)/. 2. in
           let new_pred =
             Symbols.add ~overwrite:true upper_bound
                         (f_up,Some id)
                         (Symbols.add id (p,None) sg.precedence) in
             p, {sg with precedence = new_pred}
        | f_up, Some lower_bound ->
           let f_down,_ = Symbols.find lower_bound sg.precedence in
           let p =  (f_up +. f_down)/. 2. in
           let new_pred =
             Symbols.add upper_bound
                         (f_up,Some id)
                         (Symbols.add id (p,Some lower_bound) sg.precedence) in
           p, {sg with precedence = new_pred}
        | exception Not_found -> failwith "Bug: Shouldn't happen")
			                     
  let add_warnings _ sg = sg
			   
  let get_warnings _  = []
    
      
  let raw_to_string t = Lambda.raw_to_string t
    
  let behavior_to_string = function
    | Abstract_syntax.Default -> ""
    | Abstract_syntax.Prefix -> "prefix "
    | Abstract_syntax.Infix _ -> "infix "
    | Abstract_syntax.Binder -> "binder "
	
  let entry_to_string f decl = 
    let buff=Buffer.create 10 in
    let temp_str_formatter = Format.formatter_of_buffer buff in
    let () = Utils.fterm_set_size temp_str_formatter in
    let () =
      match decl with
      | Type_declaration(s,_,k) -> 
	let () = Utils.fformat temp_str_formatter "@[<hov 4>%s :@ @[%s" s (Lambda.kind_to_string k f) in
	Utils.fformat temp_str_formatter "@];@]"
      | Type_definition(s,_,k,ty) -> 
	let () = Utils.fformat temp_str_formatter "@[<hov 4>%s =@ @[" s in
	let () = Lambda.type_to_formatted_string temp_str_formatter ty f in
	let () = Utils.fformat temp_str_formatter " :@ @[" in
	let () = Lambda.kind_to_formatted_string  temp_str_formatter k f in
	Utils.fformat temp_str_formatter "@];@]@]"
    | Term_declaration(s,_,behavior,ty) -> 
	let () = Utils.fformat temp_str_formatter "@[<hov 4>%s%s :@ @[" (behavior_to_string behavior) s in
	let () = Lambda.type_to_formatted_string  temp_str_formatter ty f in
	Utils.fformat temp_str_formatter "@];@]"
    | Term_definition(s,_,behavior,ty,t) -> 
	let () = Utils.fformat temp_str_formatter "@[<hov 4>%s%s =@ @[" (behavior_to_string behavior) s in
	let () = Lambda.term_to_formatted_string  temp_str_formatter t f in
	let () = Utils.fformat temp_str_formatter " :@ @[" in
	let () = Lambda.type_to_formatted_string  temp_str_formatter ty f in
	Utils.fformat temp_str_formatter "@];@]@]" in
    let () = Format.pp_print_flush temp_str_formatter () in
    Buffer.contents buff


  let entry_to_formatted_string f decl = 
    let () =
      match decl with
      | Type_declaration(s,_,k) -> 
	let () = Utils.sformat "@[<hov 4>%s :@ @[" s in
	let () = Lambda.kind_to_formatted_string Format.str_formatter k f in
	Utils.sformat "@];@]"
    | Type_definition(s,_,k,ty) -> 
	let () = Utils.sformat "@[<hov 4>%s =@ @[" s in
	let () = Lambda.type_to_formatted_string  Format.str_formatter ty f in
	let () = Utils.sformat " :@ @[" in
	let () = Lambda.kind_to_formatted_string  Format.str_formatter k f in
	Utils.sformat "@];@]@]"
    | Term_declaration(s,_,behavior,ty) -> 
	let () = Utils.sformat "@[<hov 4>%s%s :@ @[" (behavior_to_string behavior) s in
	let () = Lambda.type_to_formatted_string  Format.str_formatter ty f in
	Utils.sformat "@];@]"
    | Term_definition(s,_,behavior,ty,t) -> 
	let () = Utils.sformat "@[<hov 4>%s%s =@ @[" (behavior_to_string behavior) s in
	let () = Lambda.term_to_formatted_string  Format.str_formatter t f in
	let () = Utils.sformat " :@ @[" in
	let () = Lambda.type_to_formatted_string  Format.str_formatter ty f in
	Utils.sformat "@];@]@]" in
    ()


  let to_string ({name=n;ids=ids} as sg) =
    Printf.sprintf "signature %s = \n%send\n"
      (fst n)
      (fst (Id.fold 
	      (fun _ e (acc,b) ->
		 match b with
		   | true -> Printf.sprintf "%s%s\n" acc (entry_to_string (id_to_string sg) e),true
		   | false -> Printf.sprintf "%s\n" (entry_to_string (id_to_string sg) e),true)
	      ("",false)
	      ids))

  let convert_term t ty sg =
    let t_type = convert_type ty sg in
    let t=typecheck t t_type sg in
      t,t_type

  let type_of_constant x {terms=syms} =
    match Symbols.find x syms with
    | Term_declaration (s,_,_,ty) when x = s -> ty
    | Term_definition (s,_,_,ty,_) when x = s -> ty
    | _ -> failwith "Bug in type_of_constant"
    | exception Symbols.Not_found -> failwith "Bug in type_of_constant"
                                              
  let fold f a {ids=ids} =
    Id.fold
      (fun _ att acc -> f att acc)
      a
      ids

  let is_declared e _ =
    match e with
      | Type_declaration (s,_,_) -> Some s
      | Term_declaration (s,_,_,_) -> Some s
      | _ -> None

  let is_2nd_order {is_2nd_order} = is_2nd_order


  let entry_to_data e = 
    match e with 
    | Type_declaration (_,id,_) -> Type(Lambda.Atom id)
    | Type_definition (_,_,_,stype) -> Type stype
    | Term_declaration (_,id,_,stype) -> Term(Lambda.Const id,stype)
    | Term_definition (_,_,_,stype,term) -> Term (term,stype)

end	  
  
module Table = Table.Make_table(struct let b = 10 end)
  (*module Table =
    struct
    module IntMap = Map.Make(struct type t=int let compare i j = i-j end)
    type 'a t = 'a IntMap.t
    type key = int

    exception Conflict
    let empty = IntMap.empty
    let add ?(overwrite=false) k v t = 
    try 
    let _ = IntMap.find k t in
    if overwrite then IntMap.add k v t else raise Conflict
    with
    | Not_found -> IntMap.add k v t


    exception Not_found

    let find k t = 
    try
    IntMap.find k t
    with
    | Not_found -> raise Not_found
    let fold f acc t = IntMap.fold f t acc
    end
  *)

module Data_Signature = Make(Tries.Tries)(Table)


