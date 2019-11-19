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

module Log = (val Logs.src_log (Logs.Src.create "ACGtkLib.entry" ~doc:"logs ACGtkLib entry events") : Logs.LOG)
               
module Entry =
struct
  type data = dec
  and dec = 
    | No_dec
    | Sig of sig_dec_id
    | Lex of lex_id 
    | Extend of extension
  and extension =
    | No_ext_type
    | Ext_sig_name
    | Ext_lex_name
    | Ext_sig_with
    | Ext_lex_with
  and sig_dec_id =
    | No_sig_dec_id
    | Sig_dec_id of sig_dec_equal
  and sig_dec_equal =
    | No_sig_dec_equal
    | Sig_dec_equal of entry
  and entry =
    | No_entry
    | Entry_id of entry_id_list
    | Prefix_infix of symbol
    | Binder of binder_id
  and entry_id_list =
    | No_entry_id_list
    | Comma_in_id_list
    | Colon_assignment of type_kwd_or_type_or_term
    | Equal_def of type_or_term_in_def
  and symbol =
    | No_symbol
    | Symbol 
  and binder_id =
    | No_binder_id
    | Binder_id
  and term_dec_or_def =
    | No_term_dec_or_def
    | Colon_term_dec of type_kwd_or_type_or_term
    | Equal_term_def of type_or_term_in_def
  and type_kwd_or_type_or_term =
    | No_type_kwd_or_type_or_term
    | Type_kwd_or_type_or_term of (type_or_term_kind*valuation)
  and type_or_term_in_def =
    | No_type_or_term_in_def
    | Type_or_term_in_def of (type_or_term_kind*valuation)
  and type_or_term_kind =
    | Nothing
    | Unset
    | Type
    | Term
  and valuation =
    | EOI
    | Sig_kwd
    | Lex_kwd
    | Ext_kwd
    | With_kwd
    | Id
    | Equal
    | Compose
    | Comma
    | Colon
    | Colon_equal
    | Type_kwd
    | End_kwd
    | Semi_colon
    | Type_or_term of type_or_term_tokens
    | Prefix_kwd
    | Infix_kwd
    | Binder_kwd
    | Sym
  and type_or_term_tokens =
    | LPAR
    | RPAR
    | DOT
    | LAMBDA
    | ARROW
  and lex_id =
    | No_lex_dec
    | Lex_id
    | Lex_eq
    | Lex_name
    | Lex_name_2
    | Lex_composition
    | Abstract_sig_opening
    | Abstract_sig_name
    | Abstract_sig_closing
    | Object_sig_opening
    | Object_sig_name
    | Lex_def of lex_entry
  and lex_entry =
    | No_lex_entry
    | Lex_entry_id of interpretation
  and interpretation =
    | No_interpretation
    | Interpretation of type_or_term_in_def

  type term = type_or_term_in_def
  type stype = type_or_term_in_def


  let valuation_to_string = function
    | EOI -> "End of input"
    | Sig_kwd -> "\"signature\" kwd"
    | Lex_kwd -> "\"lexicon\" kwd"
    | Ext_kwd -> "\"extend\" kwd"
    | With_kwd -> "\"with\" kwd"
    | Id -> "Identifier"
    | Equal -> "\"=\""
    | Compose -> "\"<<\""
    | Comma -> "\",\""
    | Colon -> "\":\""
    | Colon_equal -> "\":=\""
    | Type_kwd -> "\"type\" kwd"
    | End_kwd -> "\"end\" kwd"
    | Semi_colon -> "\";\""
    | Type_or_term (LPAR|RPAR) -> "type or term"
    | Type_or_term (DOT|LAMBDA) -> "term"
    | Type_or_term ARROW -> "type"
    | Prefix_kwd -> "\"prefix\""
    | Infix_kwd -> "\"infix\""
    | Binder_kwd -> "\"binder\""
    | Sym -> "symbol"

  exception Expect of valuation list

  let start_data () = No_dec

  let start_term () = No_type_or_term_in_def

  let start_type () = Type_or_term_in_def (Type,Colon)

  let start_sig_entry () = Sig (Sig_dec_id (Sig_dec_equal (No_entry)))

  let start_lex_entry () = Lex(Lex_def No_lex_entry)

let build_expectation lst =
    let rec build_expectation_rec lst k =
      match lst with
	| [] -> k []
	| (tok,res)::tl -> 
	    build_expectation_rec
	      tl
	      (fun acc ->
		 let a,f= k acc in
		 let expectation = tok::a in
		   expectation,
		 (fun x -> if x=tok then 
		    res
		  else
		    try
		      f x
		    with
		      | Expect e -> raise (Expect (tok::e)))) in
      build_expectation_rec lst (fun l -> (l,(fun x -> raise (Expect l))))

  let term_expectation = function
    | No_type_or_term_in_def -> let l = [Id;Type_or_term LAMBDA;Sym] in
	l,(function
	     | (Id|Sym|Type_or_term (LPAR|LAMBDA)) as a -> Type_or_term_in_def (Term,a)
	     | _ -> raise (Expect l))
    | Type_or_term_in_def ((Unset|Type),_) -> failwith "Bug: should not occur"
    | Type_or_term_in_def (Term,Id) -> let l = [Id;Type_or_term LAMBDA;Sym;EOI] in
	l,(function
	     | Type_or_term ARROW -> raise (Expect [Type_or_term DOT])
	     | (Id|Type_or_term _| Sym) as a -> Type_or_term_in_def (Term,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l)) 
    | Type_or_term_in_def (Term,Sym) -> let l = [Id;Type_or_term LAMBDA;Sym;EOI] in
	l,(function
	     | (Id|Type_or_term (LPAR|LAMBDA)|Sym) as a -> Type_or_term_in_def (Term,a)
	     | Type_or_term (RPAR|DOT|ARROW) -> raise (Expect l)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l)) 
    | Type_or_term_in_def (Term,Type_or_term DOT) -> let l = [Id;Type_or_term LAMBDA;Sym;EOI] in
	l,(function
	     | (Id|Type_or_term (LPAR|LAMBDA)|Sym) as a-> Type_or_term_in_def (Term,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,Type_or_term LAMBDA) -> let l = [Id] in
	l,(function
	     | Id as a -> Type_or_term_in_def (Term,a)
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,Type_or_term LPAR) -> let l = [Id;Type_or_term ARROW;Sym] in
	l,(function
	     | (Id|Sym| Type_or_term (LPAR|LAMBDA)) as a -> Type_or_term_in_def (Term,a)
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,Type_or_term RPAR) -> let l = [Id;Type_or_term ARROW;Sym;EOI] in
	l,(function
	     | (Id|Sym|Type_or_term (LPAR|RPAR|LAMBDA)) as a -> Type_or_term_in_def (Term,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,_) -> failwith "Bug: should not occur"
    | Type_or_term_in_def (Nothing,_) -> failwith "Bug: Mothing should not appear in term"

  let term_transition q v = q

  let type_transition q v = q
(*    let _,result = term_expectation q in
      result v *)

  let data_expectation = function
    | No_dec ->
       Log.debug (fun m -> m "Current state is No_dec\n");
       let l = [EOI;Sig_kwd;Lex_kwd;Ext_kwd] in
       l , (function
	     | EOI -> No_dec
	     | Sig_kwd -> Sig No_sig_dec_id
	     | Lex_kwd -> Lex No_lex_dec
	     | Ext_kwd -> Extend No_ext_type
	     | _ -> raise (Expect l))
    | Extend No_ext_type ->
       let l = [Sig_kwd;Lex_kwd] in
       l, (function
	    | Sig_kwd -> Extend Ext_sig_name
	    | Lex_kwd -> Extend Ext_lex_name
	    | _ -> raise (Expect l))
    | Extend Ext_sig_name ->
       let l = [Id] in
       l, (function
	    | Id -> Extend Ext_sig_with
	    | _ -> raise (Expect l))
    | Extend Ext_lex_name ->
       let l = [Id] in
       l, (function
	    | Id -> Extend Ext_lex_with
	    | _ -> raise (Expect l))
    | Extend Ext_sig_with ->
       let l = [With_kwd] in
       l, (function
	    | With_kwd -> Sig (Sig_dec_id (Sig_dec_equal No_entry))
	    | _ -> raise (Expect l))
    | Extend Ext_lex_with ->
       let l = [With_kwd] in
       l, (function
	    | With_kwd -> Lex (Lex_def No_lex_entry)
	    | _ -> raise (Expect l))
    | Sig No_sig_dec_id -> let l = [Id] in
	l,(function
	     | Id -> Sig (Sig_dec_id No_sig_dec_equal)
	     | _ ->  raise (Expect l))

    | Sig (Sig_dec_id No_sig_dec_equal) -> let l = [Equal] in
	l,(function
	     | Equal -> Sig (Sig_dec_id (Sig_dec_equal (No_entry)))
	     | _ -> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (No_entry))) -> let l = [Id;Prefix_kwd;Infix_kwd;Binder_kwd;End_kwd] in
	l,(function
	     | Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list)))
	     | Prefix_kwd -> Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol)))
	     | Infix_kwd -> Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol)))
	     | Binder_kwd -> Sig (Sig_dec_id (Sig_dec_equal (Binder No_binder_id)))
	     | End_kwd -> No_dec
	     | _ -> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list))) -> let l = [Comma;Colon;Equal] in
	l,(function
	     | Comma -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id Comma_in_id_list)))
	     | Colon -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
	     | Equal  -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def))))
	     | _ -> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id Comma_in_id_list))) -> let l = [Id] in
	l,(function
	     | Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list)))
	     | _-> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term)))) ->
	let l = [Type_kwd;Type_or_term ARROW;Id] in
	  l,(function
	       | Type_kwd as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (Nothing,a))))))
	       | (Id|Type_or_term LPAR) as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (Type,a))))))
	       | _ -> raise (Expect [Type_kwd;Type_or_term ARROW;Id]))
	    
    |  Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def)))) ->
	 let l = [Type_or_term LPAR;Id;Sym] in
	   l,(function
		| (Id|Type_or_term LPAR) as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Unset,a))))))
		| (Sym|Type_or_term LAMBDA) as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		| _ -> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def k_o_t))))) ->
	let l = [Colon;Type_or_term LPAR;Id;Sym] in
	  l,(function
	       | Colon -> 
		   (match k_o_t with
		      | (Unset|Term|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
		      | Unset, _ -> raise (Expect [Id;Type_or_term LPAR])
		      | Type, _ -> raise (Expect [Id;Type_or_term ARROW])
		      | Term, _ -> raise (Expect [Id;Type_or_term DOT])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | Type_or_term RPAR as a-> 
		   (match k_o_t with
		      | (Unset|Term|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (fst k_o_t,a))))))
		      | Unset, _ -> raise (Expect [Id;Type_or_term LPAR])
		      | Type, _ -> raise (Expect [Id;Type_or_term ARROW])
		      | Term, _ -> raise (Expect [Id;Type_or_term DOT])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | Type_or_term LPAR as a-> 
		   (match k_o_t with
		      | Term,(Id|Sym|Type_or_term (LPAR|DOT)) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		      | Unset,(Id|Sym|Type_or_term DOT) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		      | Unset,(Type_or_term LPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Unset,a))))))
		      | (Unset|Term) as k,(Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (k,a))))))
		      | (Unset|Type),(Type_or_term ARROW|Type_or_term LPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Type,a))))))
		      | Unset, _ -> raise (Expect [Id;Sym;Type_or_term LPAR])
		      | Type, _ -> raise (Expect [Type_or_term ARROW;Semi_colon])
		      | Term, _ -> raise (Expect [Id;Sym;Type_or_term DOT])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))		   
	       | Type_or_term DOT as a ->
		   (match k_o_t with
		      | Type,_ -> raise (Expect [Type_or_term ARROW;Semi_colon])
		      | (Unset|Term),Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		      | (Unset|Term),_ -> raise (Expect [Id])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | Type_or_term LAMBDA as a-> 
		   (match k_o_t with
		      | Type,_ -> raise (Expect [Type_or_term ARROW;Semi_colon])
		      | (Unset|Term),(Id|Sym|Type_or_term (LPAR|RPAR|DOT)) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		      | (Unset|Term),_ -> raise (Expect [Id;Sym;Type_or_term LAMBDA])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | Type_or_term ARROW as a -> 
		   (match k_o_t with
		      | Term,_ -> let () = Printf.printf "I know it is a term\n%!" in raise (Expect [Type_or_term DOT])
		      | (Unset|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Type,a))))))
		      | (Unset|Type),_ -> raise (Expect [Id;Type_or_term ARROW])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | Id as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (fst k_o_t,a))))))
	       | Sym as a -> 
		 (match k_o_t with
		 | Type,_ -> raise (Expect [Type_or_term ARROW;Semi_colon])
		 | (Unset|Term),(Id|Sym|Type_or_term (LPAR|RPAR|DOT)) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		 | (Unset|Term),_ -> raise (Expect [Id])
		 | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | _ -> raise (Expect l))
	    
    | Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol))) ->
      let l = [Sym] in
      l,(function
      | Sym -> Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix Symbol)))
      | _ -> raise (Expect l))
	
	
    | Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix Symbol))) ->
      let l =[Colon;Equal] in
      l,(function
      | Colon -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
      | Equal -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def))))
      | _ -> raise (Expect l))
	
    | Sig (Sig_dec_id (Sig_dec_equal (Binder No_binder_id))) ->
      let l = [Id] in
      l,(function
      | Id -> Sig (Sig_dec_id (Sig_dec_equal (Binder Binder_id)))
      | _ ->  raise (Expect l))
	
    | Sig (Sig_dec_id (Sig_dec_equal (Binder Binder_id))) ->
      let l = [Colon;Equal] in
      l, (function 
      | Equal -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def))))
      | Colon -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
      | _ -> raise (Expect l))
	
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term k_o_t))))) -> 
      let l = [End_kwd;Semi_colon] in
      l,(function
      | End_kwd -> 
	(match k_o_t with
	| Nothing,_ -> No_dec
	| (Unset|Type),(Id|Type_or_term RPAR) -> No_dec
	| _ -> raise (Expect [Id;Type_or_term ARROW]))
      | Semi_colon -> 
	(match k_o_t with
	| Nothing,_ -> Sig (Sig_dec_id (Sig_dec_equal No_entry))
	| (Unset|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal No_entry))
	| _ -> raise (Expect [Id;Type_or_term ARROW]))
      | Id as a ->
	(match k_o_t with
	| (Unset|Type),Type_or_term (LPAR|ARROW) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (fst k_o_t,a))))))
	| _ -> raise (Expect [Type_or_term ARROW;Semi_colon]))
      | Type_or_term LPAR as a ->
	(match k_o_t with
	| (Unset|Type),Type_or_term (LPAR|ARROW) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (fst k_o_t,a))))))
	| _ -> raise (Expect [Type_or_term ARROW;Semi_colon]))
      | Type_or_term RPAR -> 
	(match k_o_t with
	| (Unset|Type),(Id|Type_or_term LPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term k_o_t)))))
	| _ -> raise (Expect [Type_or_term ARROW;Semi_colon]))
      | Type_or_term DOT -> raise (Expect [Type_or_term ARROW;Semi_colon])
      | Type_or_term LAMBDA -> raise (Expect [Type_or_term ARROW;Semi_colon])
      | Type_or_term ARROW as a-> 
	(match k_o_t with
	| Term,_ -> raise (Expect [Type_or_term DOT])
	| (Unset|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (Type,a))))))
	| (Unset|Type),_ -> raise (Expect [Id;Type_or_term ARROW])
	| Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
      | Sym -> raise (Expect [Type_or_term ARROW;Semi_colon])
      | _ -> raise (Expect l))
    | Lex No_lex_dec -> build_expectation [Id,Lex Lex_id]
    (*    | Lex Lex_id -> build_expectation [Type_or_term LPAR,Lex Abstract_sig_opening] *)
    | Lex Lex_id -> build_expectation
      [Type_or_term LPAR,Lex Abstract_sig_opening;
       Equal,Lex Lex_eq]
    | Lex Lex_eq -> build_expectation [Id,Lex Lex_name]
    | Lex Lex_name -> build_expectation [Compose,Lex Lex_composition]
    | Lex Lex_composition -> build_expectation [Id,Lex Lex_name_2]
    | Lex Lex_name_2 -> build_expectation
      [Compose,Lex Lex_composition;
       Sig_kwd, Sig No_sig_dec_id ;
       Lex_kwd, Lex No_lex_dec ;
       EOI,No_dec
      ]
      
    | Lex Abstract_sig_opening -> build_expectation [Id,Lex Abstract_sig_name]
    | Lex Abstract_sig_name -> build_expectation [(Type_or_term RPAR),Lex Abstract_sig_closing]
    | Lex Abstract_sig_closing -> build_expectation [Colon,Lex Object_sig_opening]
    | Lex Object_sig_opening -> build_expectation [Id,Lex Object_sig_name]
    | Lex Object_sig_name -> build_expectation [Equal,Lex (Lex_def No_lex_entry)]
    | Lex (Lex_def No_lex_entry) -> build_expectation [Id,Lex (Lex_def (Lex_entry_id No_interpretation));End_kwd,No_dec;Sym,Lex (Lex_def (Lex_entry_id No_interpretation))]
    | Lex (Lex_def (Lex_entry_id No_interpretation)) -> 
      build_expectation [Comma,Lex (Lex_def No_lex_entry);
			 Colon_equal,Lex (Lex_def (Lex_entry_id (Interpretation No_type_or_term_in_def)))]
    | Lex (Lex_def (Lex_entry_id (Interpretation ty_o_te))) ->
      match ty_o_te with
      | No_type_or_term_in_def ->
	let l = [Id;Sym;Type_or_term LPAR;Type_or_term LAMBDA] in
	l,(function
	| (Id|Type_or_term LPAR) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Unset,a)))))
	| (Sym|Type_or_term LAMBDA) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| _ -> raise (Expect [Type_or_term LPAR]))
      | Type_or_term_in_def (Unset,Id) ->
	let l = [Type_or_term LPAR;Semi_colon] in
	l,(function
	| (Id|Sym|Type_or_term (LPAR|DOT|LAMBDA)) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| Type_or_term RPAR as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Unset,a)))))
	| Type_or_term ARROW as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Type,a)))))
	| Semi_colon -> Lex (Lex_def No_lex_entry)
	| End_kwd -> No_dec 
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Unset,Type_or_term LPAR) ->
	let l = [Type_or_term LPAR] in
	l,(function
	| (Sym|Type_or_term LAMBDA) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| (Id|Type_or_term LPAR) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Unset,a)))))
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Unset,Type_or_term RPAR) ->
	let l = [Type_or_term LPAR;Semi_colon] in
	l,(function
	| (Id|Sym|Type_or_term (LPAR|LAMBDA)) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| (Type_or_term RPAR) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Unset,a)))))
	| (Type_or_term ARROW) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Type,a)))))
	| Semi_colon -> Lex (Lex_def No_lex_entry)
	| End_kwd -> No_dec
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Unset,_) -> failwith "Bug: should not occur"
      | Type_or_term_in_def (Term,Id) -> 
	let l = [Semi_colon;Type_or_term ARROW] in
	l,(function
	| (Id|Sym|Type_or_term (LPAR|RPAR|DOT|LAMBDA)) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| Semi_colon -> Lex (Lex_def No_lex_entry)
	| End_kwd -> No_dec
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Term,Sym) -> 
	let l = [Type_or_term ARROW] in
	l,(function
	| (Id|Sym|Type_or_term (LPAR|LAMBDA)) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Term,Type_or_term LPAR) -> 
	let l = [Type_or_term ARROW] in
	l,(function
	| (Id|Sym|Type_or_term (LPAR|LAMBDA)) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Term,Type_or_term RPAR) -> 
	let l = [Semi_colon;Type_or_term ARROW] in
	l,(function
	| (Id|Sym|Type_or_term (LPAR|RPAR|LAMBDA)) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| Semi_colon -> Lex (Lex_def No_lex_entry)
	| End_kwd -> No_dec
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Term,Type_or_term DOT) -> 
	let l = [Type_or_term ARROW] in
	l,(function
	| (Id|Sym|Type_or_term (LPAR|LAMBDA)) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Term,Type_or_term LAMBDA) -> 
	let l = [Id] in
	l,(function
	| Id as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Term,a)))))
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Term,_) -> failwith "Bug: Should not occur"
      | Type_or_term_in_def (Type,Id) ->
	let l = [Semi_colon;Type_or_term RPAR;Type_or_term ARROW] in
	l,(function
	| End_kwd -> No_dec
	| Semi_colon -> Lex (Lex_def No_lex_entry)
	| Type_or_term (RPAR|ARROW) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Type,a)))))
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Type,Type_or_term LPAR) ->
	let l = [Id;Type_or_term LPAR] in
	l,(function
	| (Id|Type_or_term LPAR) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Type,a)))))
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Type,Type_or_term RPAR) ->
	let l = [Semi_colon;Type_or_term RPAR;Type_or_term ARROW] in
	l,(function
	| End_kwd -> No_dec
	| Semi_colon -> Lex (Lex_def No_lex_entry)
	| Type_or_term (RPAR|ARROW) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Type,a)))))
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Type,Type_or_term ARROW) ->
	let l = [Id;Type_or_term LPAR] in
	l,(function
	| (Id|Type_or_term LPAR) as a -> Lex (Lex_def (Lex_entry_id (Interpretation (Type_or_term_in_def (Type,a)))))
	| _ -> raise (Expect l))
      | Type_or_term_in_def (Type,_) -> failwith "Bug:should not occur"
      | Type_or_term_in_def (Nothing,_) -> failwith "Bug:should not occur"
	
	
	
	
	
  let data_transition q v =
    Log.debug (fun m -> m  "Will test transition on \"%s\"\n" (valuation_to_string v));
    let _,result = data_expectation q in
      result v 


end
