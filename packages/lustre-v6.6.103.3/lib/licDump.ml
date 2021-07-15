(* Time-stamp: <modified the 04/09/2019 (at 17:53) by Erwan Jahier> *)
(*

This module is used both for
(1) dealing with internal ident names
(2) generating lustre files
(3) building error messages

Because of (2), ident names may depend on the ec or the v4 option. But for (1)
and (3), we don't want to depend that those options. 

Hence, I've added a boolean flag to all "to_string" functions that
states whether the function is used for internal purposes, or for
generating lustre files.

 *)
open Lv6errors
open Printf
open Lxm
open Lic
open Lv6MainArgs

(* XXX changer le nom de cette fonction *)
let (dump_long : bool -> Lv6Id.long -> string) = Lv6Id.string_of_long_bis 
 

(*   fun id ->  *)
(*     let str = Lv6Id.string_of_long id in *)
(*       Str.global_replace (Str.regexp "::") "__" str *)

(******************************************************************************)    

let (dump_entete : out_channel -> unit) =
 fun oc -> if global_opt.kcg then 
     (Lv6util.entete oc "/*" "*/") 
   else 
     (Lv6util.entete oc "(*" "*)")  

(******************************************************************************)    
let  (get_rank : 'a -> 'a list -> int) =
  fun x l -> 
    let rec aux i l =
      match l with
        | [] -> assert false
        | y::l -> if x = y then i else aux (i+1) l
    in
      aux 1 l
let _ = assert (get_rank 5 [1;3;5] = 3)


(* check it is a non-singleton tuple *)
let rec is_a_tuple (e:Lic.val_exp) : bool =
   match e.ve_core with
    | CallByPosLic ({ it = TUPLE ; _}, [ve]) -> is_a_tuple ve
    | CallByPosLic ({ it = TUPLE ; _ }, vel) -> List.length vel > 1
    | _ -> false


(******************************************************************************)    
let string_of_ident forprint x =
 if global_opt.kcg then 
   Lv6Id.no_pack_string_of_long x
 else
  if global_opt.no_prefix
   then Lv6Id.no_pack_string_of_long x
  else Lv6Id.string_of_long forprint x


(* the flag forprint controls whether those functions are used for printing
 into a file or not (i.e. for internal use) *)
                                  
let rec string_of_const_eff forprint =
  function
  | Bool_const_eff true -> "true"
  | Bool_const_eff false -> "false"
  | Int_const_eff i -> (sprintf "%s" i)
  | Real_const_eff r -> r
  | Extern_const_eff (s,_t) -> (dump_long forprint s)
  | Abstract_const_eff (s,_t,v,_) -> 
     (dump_long forprint s) ^ (* XXX ? *)
       (string_of_const_eff forprint v)
  (*     | Abstract_const_eff (s,t,v,false) -> (dump_long forprint s)  *)
  | Enum_const_eff   (s,Enum_type_eff(_,ll)) -> Lic.enum_to_string s ll
  | Enum_const_eff   (_) -> assert false
  | Struct_const_eff (fl, t) -> (
    let string_of_field = 
      function (id, veff) -> 
               (Lv6Id.to_string id)^" = "^ (string_of_const_eff forprint veff) 
    in
    let flst = List.map string_of_field fl in
    (string_of_type_eff forprint t)^"{"^(String.concat "; " flst)^"}"
  )
  | Array_const_eff (ctab, _t) -> (
    let vl = List.map (string_of_const_eff forprint) ctab in
    "["^(String.concat ", " vl)^"]"
  )
  | Tuple_const_eff   cl ->  (
    string_of_const_eff_list forprint cl
  )

and string_of_const_eff_list forprint =
  function
  | [c] -> string_of_const_eff forprint c
  | cl -> "(" ^ (String.concat ", " (List.map (string_of_const_eff forprint) cl)) ^ ")"

(* modify numbers notations in such a way that they
   become "valid" identifiers. Policy:
   - minus (-) becomes "m"
   - plus (+) becomes "p"
   - dot (d) becomes "d"
 *)
and correct_num_string s = 
  let (s:Bytes.t)  = Bytes.of_string s in
  let (res:Bytes.t)= Bytes.copy s in
  let cpt = ref 0 in
  let f c = (
    let _ = match c with
      | '-' -> (Bytes.set res !cpt 'm')
      | '+' -> (Bytes.set res !cpt 'p')
      | '.' -> (Bytes.set res !cpt 'd')
      | _ -> ()
    in incr cpt
  ) in
  Bytes.iter f s;
  Bytes.to_string res

and string_ident_of_const_eff forprint c = 
  (* that version generates a string that is a valid lic ident, in order to use it
     to generate a node name using static parameters *)
  match c with
  | Int_const_eff _
  | 
    Real_const_eff _ ->
     correct_num_string(string_of_const_eff forprint c)
  | Bool_const_eff _
  | Extern_const_eff _
  | Abstract_const_eff _
  | Enum_const_eff _  -> string_of_const_eff forprint c
  | Struct_const_eff (_, t) -> (
    match t with 
    | Struct_type_eff (sn,_) -> Lv6Id.no_pack_string_of_long sn
    | _ -> assert false
  )
  | Array_const_eff (ctab, t) ->
     (string_of_type_eff forprint t) ^ "_" ^(string_of_int (List.length ctab))
  | Tuple_const_eff cl ->  string_ident_of_const_eff_list forprint cl

and string_ident_of_const_eff_list forprint cl =
  match cl with
  | [c] -> string_ident_of_const_eff forprint c
  | _ -> "" ^ (String.concat "_" (List.map (string_ident_of_const_eff forprint) cl)) ^ ""

and string_of_const_eff_opt forprint = function
  | None -> ""
  | Some val_exp_eff -> string_of_const_eff forprint val_exp_eff

and string_def_of_type_eff forprint = function
  | Bool_type_eff -> "bool"
  | Int_type_eff  -> "int"
  | Real_type_eff -> "real"
  | External_type_eff (i) -> dump_long forprint i
  | Abstract_type_eff (_i, t) -> string_def_of_type_eff forprint t
  | Enum_type_eff (_i, sl) ->
     assert (sl <>[]);
     let f sep acc s  = acc ^ sep ^ (dump_long forprint s) in
     (List.fold_left (f ", ")  (f "" "enum {" (List.hd sl)) (List.tl sl)) ^ "}"
  | Array_type_eff (ty, sz) -> sprintf "%s^%d" (string_of_type_eff forprint ty) sz
  | Struct_type_eff (_name, fl) -> 
     assert (fl <>[]);
     let f sep acc (id, (type_eff, const_eff_opt))  = 
       acc ^ sep ^ (Lv6Id.to_string id) ^ " : " ^
         (string_of_type_eff forprint type_eff) ^
           match const_eff_opt with
             None -> ""
           | Some ce -> " = " ^ (string_of_const_eff forprint ce)
     in
     if global_opt.kcg then
       (List.fold_left (f ", ")  (f "" " {" (List.hd fl)) (List.tl fl)) ^ "}"
     else
       "struct " ^
         (List.fold_left (f "; ")  (f "" " {" (List.hd fl)) (List.tl fl)) ^ "}"
                                                                              
  | TypeVar Any -> "a"
  | TypeVar AnyNum -> "o"


(* exported *)

(* On prend le meme que Lic *)
and string_of_type_eff forprint = function
  | Bool_type_eff -> "bool"
  | Int_type_eff  -> "int"
  | Real_type_eff -> "real"
  | External_type_eff (name) -> (string_of_ident forprint name)
  | Abstract_type_eff (name, _t) -> (string_of_ident forprint name)
  | Enum_type_eff (name, el) -> 
     (match global_opt.Lv6MainArgs.expand_enums with
      | AsEnum | AsConst -> string_of_ident forprint name
      | AsInt  -> if global_opt.kcg then dump_long forprint name else "int"
      | AsBool -> if global_opt.kcg then dump_long forprint name else
                    (*
                    let get_n x = (* returns the n s.t., 2^(n-1) < x <= 2^n *)
                      assert(x>0);
                      let rec f n acc = 
                        if x > acc then f (n+1) (2*acc) else n 
                      in
                      f 0 1
                    in
                    let size = get_n  (List.length el) in
                     *)
                    (* well, 1-hot encoding is efficient enough and easier
                     to understand afterwards *)
                    let size = (List.length el) in
                    ("bool^"^(string_of_int size))
     )
  | Array_type_eff (ty, sz) ->
     Printf.sprintf "%s^%d" (string_of_type_eff forprint ty) sz
  | Struct_type_eff (name, _) -> (if global_opt.kcg then dump_long forprint name else string_of_ident forprint name)
  | TypeVar Any -> "any"
  | (TypeVar AnyNum) -> "anynum"

and string_of_type_list forprint = function
  | []  -> ""
  | [x] -> string_of_type_eff forprint x
  | l   -> String.concat " * " (List.map (string_of_type_eff forprint) l)

and string_of_type_profile forprint (i, o) =
  (string_of_type_list forprint i)^" -> "^(string_of_type_list forprint o)

and string_of_const forprint = function
  | Bool_const_eff true -> "true"
  | Bool_const_eff false -> "false"
  | Int_const_eff i -> (sprintf "%s" i)
  | Real_const_eff r -> r
  | Extern_const_eff (s,_) -> (string_of_ident forprint s)
  | Abstract_const_eff (s,_t,_v,_) -> (string_of_ident forprint s)
  | Enum_const_eff   (s,Enum_type_eff(_,ll)) ->
     (string_of_int (Lv6util.pos_in_list 0 s ll))
  | Enum_const_eff _  -> assert false
  | Struct_const_eff (fl, t) -> 
     let string_of_field (id, veff) =
       (Lv6Id.to_string id)^" = "^ (string_of_const forprint veff)
     in
     Printf.sprintf "%s{%s}"
                    (string_of_type_eff forprint t)
                    (String.concat "; " (List.map string_of_field fl))
  | Array_const_eff (ctab, _t) ->
     Printf.sprintf "[%s]"
                    (String.concat ", " (List.map (string_of_const forprint) ctab))
  | Tuple_const_eff   cl ->
     Printf.sprintf "(%s)"
                    (String.concat ", " (List.map (string_of_const forprint)cl))

and string_of_var_info forprint x =
  (AstCore.string_of_var_nature x.var_nature_eff) ^ " " ^
    (Lv6Id.to_string x.var_name_eff) ^ ":"^(string_of_type_eff forprint x.var_type_eff)^
      (string_of_clock (snd x.var_clock_eff)^"("^ (Lv6Id.to_string (fst x.var_clock_eff)) ^","^
         (string_of_int x.var_number_eff)^")")

and string_of_var_list forprint vl =
  String.concat " ; " (List.map (string_of_var_info forprint) vl)

and string_of_node_key forprint = function
  | (ik, []) ->
     (string_of_ident forprint ik)
  | (ik, sargs) ->
     Printf.sprintf "%s<<%s>>"
                    (string_of_ident forprint ik)
                    (String.concat ", "
                                   (List.map (string_of_static_arg forprint) sargs))

and string_of_static_arg forprint = function
  | ConstStaticArgLic(id, ceff) ->
     Printf.sprintf "const %s = %s" id (string_of_const forprint ceff)
  | TypeStaticArgLic (id, teff) ->
     Printf.sprintf "type %s = %s" id (string_of_type_eff forprint teff)
  (* | NodeStaticArgLic (id, ((long,sargs), _, _), _) -> *)
  | NodeStaticArgLic (id, nk) ->
     Printf.sprintf "node %s = %s" id (string_of_node_key forprint nk)

and string_of_type_var forprint tv = string_of_type_eff forprint (TypeVar tv)
and string_of_type_matches forprint pm =
  let sotm (tv,t) = Printf.sprintf "%s <- %s"
                                   (string_of_type_var forprint tv)
                                   (string_of_type_eff forprint t)
  in
  String.concat ", " (List.map sotm pm)

(* for printing recursive node *)
and string_of_node_key_rec forprint (no_prefix:bool) (nkey: node_key) = 
  match nkey with
  | (ik, []) -> if global_opt.kcg then Lv6Id.no_pack_string_of_long ik else
                  if no_prefix 
                  then Lv6Id.no_pack_string_of_long ik 
                  else Lv6Id.string_of_long forprint ik
  | (ik, salst) ->
     if global_opt.kcg then  ((* recursive nodes have been unfold *)
	    (*assert (List.mem ik ["map"]);*)
	    (* not yet working : 
	      - cas des noeuds itérés prédéfinis
	      - il genere des alias des noeuds que scade ne comprend pas
	     *)
	    let rec get_node sl = 
	      match sl with 
	      | [] -> assert false
	      | s::sl -> (match s with
	                  | NodeStaticArgLic  (_,nk) -> nk,sl
	                  | ConstStaticArgLic (_, _) 
	                  | TypeStaticArgLic  (_,_) -> 
		                  let n,sl = get_node sl in
		                  n, s::sl
	                 )
	    in
	    let nk, salst = get_node salst in
	    let astrings = List.map (static_arg2string_kcg forprint) salst in
	    let name = sprintf "(%s %s <<%s>>)" (Lv6Id.no_pack_string_of_long ik)
	                       (string_of_node_key_rec forprint no_prefix nk)
                          (String.concat "," astrings) 
	    in
	    (FreshName.node_key nkey name)

     )	
     else 
       let astrings = List.map (static_arg2string_bis forprint) salst in
       let name = sprintf "%s_%s" (Lv6Id.no_pack_string_of_long ik)
                          (String.concat "_" astrings) in
       (FreshName.node_key nkey name)

(* for printing iterators *)
and string_of_node_key_iter forprint (nkey: node_key) = 
  match nkey with
  | (ik, []) -> dump_long forprint ik
  | (ik, salst) ->
     let astrings = List.map (static_arg2string forprint) salst in
     if forprint then
       sprintf "%s<<%s>>" (Lv6Id.string_of_long forprint ik) (String.concat ", " astrings)
     else
       sprintf "%s<<%s>>" (Lv6Id.string_of_long forprint ik) (String.concat ", " astrings)
(* pour ecrire UN NIVEAU d'arg statique (cf. LicMetaOp *)
and string_of_node_key_def forprint (nkey: node_key) = 
  match nkey with
  | (ik, []) -> dump_long forprint ik
  | (ik, salst) ->
     let astrings = List.map (string_of_static_arg forprint) salst in
     sprintf "%s<<%s>>" (Lv6Id.no_pack_string_of_long ik) (String.concat ", " astrings)

(* for inventing a name to parametrized nodes *)
and static_arg2string_bis forprint (sa : Lic.static_arg) =
  match sa with
  | ConstStaticArgLic (_, ceff) -> sprintf "%s" (string_ident_of_const_eff forprint ceff)
  | TypeStaticArgLic  (_, teff) -> sprintf "%s" (string_of_type_eff forprint teff)
  (* | NodeStaticArgLic  (id, ((long, _sargs), _, _), _) -> *)
  | NodeStaticArgLic  (_, (long,_)) ->
     sprintf "%s" (Lv6Id.no_pack_string_of_long long)

and static_arg2string_kcg forprint (sa : Lic.static_arg) =
  match sa with
  | ConstStaticArgLic (_, ceff) -> sprintf "%s" (string_ident_of_const_eff forprint ceff)
  | TypeStaticArgLic  (_, teff) -> sprintf "%s" (string_of_type_eff forprint teff)
  (* | NodeStaticArgLic  (id, ((long, _sargs), _, _), _) -> *)
  | NodeStaticArgLic  (_, (_long,_)) -> assert false (* should not occur *)


(* for printing recursive node and iterators *)
and static_arg2string forprint (sa : Lic.static_arg) =
  match sa with
  | ConstStaticArgLic (_, ceff) -> sprintf "%s" (string_ident_of_const_eff forprint ceff)
  | TypeStaticArgLic  (_, teff) -> sprintf "%s" (string_of_type_eff forprint teff)
  (* | NodeStaticArgLic  (id, ((long,sargs), _, _), _) -> *)
  | NodeStaticArgLic  (_, (long,sargs)) ->
     string_of_node_key_iter forprint (long,sargs)
(*      sprintf "%s" (dump_long forprint long) *)

and static_arg2string_rec forprint (sa : Lic.static_arg) =
  match sa with
  | ConstStaticArgLic (_, ceff) -> sprintf "%s" (string_ident_of_const_eff forprint ceff)
  | TypeStaticArgLic  (_, teff) -> sprintf "%s" (string_of_type_eff forprint teff)
  (* | NodeStaticArgLic  (id, ((long,sargs), _, _), _) -> *)
  | NodeStaticArgLic  (_, (long,sargs)) ->
     string_of_node_key_rec forprint global_opt.no_prefix (long,sargs)
(*      sprintf "%s" (dump_long forprint long) *)


and (string_of_var_info_eff: bool -> Lic.var_info -> string) =
  fun forprint x -> 
  (Lv6Id.to_string x.var_name_eff) ^ ":"^(string_of_type_eff forprint x.var_type_eff)

and (type_string_of_var_info_eff: bool -> Lic.var_info -> string) =
  fun forprint x -> (string_of_type_eff forprint x.var_type_eff) ^ 
             (string_of_clock2 (snd x.var_clock_eff))

and string_of_decl forprint var_info_eff = 
  let vt_str = 
    (Lv6Id.to_string var_info_eff.var_name_eff) ^ ":" ^ 
      (string_of_type_eff forprint var_info_eff.var_type_eff) 
  in  
  let clk_str = (string_of_clock (snd var_info_eff.var_clock_eff)) in
  if global_opt.ec then 
    if clk_str = "" then vt_str 
    else "("^vt_str ^")"^ clk_str 
  else vt_str ^ clk_str

and (string_of_type_decl_list : bool -> Lic.var_info list -> string -> string) =
  fun forprint tel sep -> 
  let str = String.concat sep (List.map (string_of_decl forprint) tel) in
  str

and string_of_slice_info_eff si_eff =
  "[" ^ (string_of_int si_eff.se_first) ^ " .. " ^ (string_of_int si_eff.se_last) ^
    (if si_eff.se_step = 1 then "" else " step " ^ (string_of_int si_eff.se_step)) ^
      "]"

and (string_of_leff : bool -> Lic.left -> string) = fun forprint -> 
  function
  | LeftVarLic  (vi_eff,_) -> Lv6Id.to_string vi_eff.var_name_eff  
  | LeftFieldLic(leff,id,_) ->
     (string_of_leff forprint leff) ^ "." ^ (Lv6Id.to_string id)
  | LeftArrayLic(leff,i,_)  ->
     (string_of_leff forprint leff) ^ "[" ^ (string_of_int i) ^ "]"
  | LeftSliceLic(leff,si,_) -> (string_of_leff forprint leff) ^
                                 (string_of_slice_info_eff si)

and (string_of_leff_list : bool -> Lic.left list -> string) =
  fun forprint l -> if global_opt.kcg then
             String.concat ", " (List.map (string_of_leff forprint) l) 

           else
             (if List.length l = 1 then "" else "(") ^ 
               (String.concat ", " (List.map (string_of_leff forprint) l)) ^ 
                 (if List.length l = 1 then "" else ")") 

and (array_of_size_one : Lic.val_exp -> bool) =
  function 
  | {ve_typ= [Array_type_eff(Bool_type_eff, size)] ; _ } -> size = 1
  | {ve_typ= [_];_ } -> true
  | _ ->  false
and (string_of_by_pos_op_eff: bool -> Lic.by_pos_op srcflagged -> Lic.val_exp list -> string) =
  fun forprint posop vel -> 
  let sov ve = string_of_val_exp_eff forprint ve in
  let tuple vel = (String.concat ", " (List.map (string_of_val_exp_eff forprint) vel)) in
  let tuple_par vel = "(" ^ (tuple vel) ^ ")" in
  let tuple_square vel = 
    "[" ^ (String.concat ", " (List.map (string_of_val_exp_eff forprint) vel)) ^ "]"
  in
  let str =
    match posop.it,vel with
    | CONST c,_ -> string_of_const_eff forprint c 
    | CALL ({it=("Lustre","not"),[]; _}), [ve1]
    | PREDEF_CALL ({it=("Lustre","not"),[]; _}), [ve1] ->
       ((op2string AstPredef.NOT_n) ^ " " ^ (tuple_par [ve1]))
         
    | CALL ({it=("Lustre","diese"),[]; _}), [ve1] 
    | PREDEF_CALL ({it=("Lustre","diese"),[]; _}), [ve1] ->
       if (global_opt.lv4) && array_of_size_one ve1
       then sov ve1 (* lv4 does no accept to apply # on One var only! *)
       (*else if global_opt.kcg then
	     ("#" ^ (dump_array_no_square ve1)) *)
	    (* do later *)
	    else
         ("#" ^ (tuple_par [ve1]))

    | CALL ({it=("Lustre","nor"),[];_}), [ve1] 
    | PREDEF_CALL ({it=("Lustre","nor"),[];_}), [ve1] ->
       (("nor") ^ (tuple_par [ve1]))
         
    | CALL ({it=("Lustre","if"),[];_}), [ve1; ve2; ve3] 
    | PREDEF_CALL ({it=("Lustre","if"),[];_}), [ve1; ve2; ve3] ->
       let ve2str = string_of_val_exp_eff forprint ve2 in 
       let ve2str = if is_a_tuple ve2 then "("^ve2str^")" else ve2str in
       let ve3str = string_of_val_exp_eff forprint ve3 in 
       let ve3str = if is_a_tuple ve3 then "("^ve3str^")" else ve3str in
       " if " ^ (string_of_val_exp_eff forprint ve1) ^ 
         " then " ^ ve2str ^ " else " ^ ve3str

    | CALL(op), vel 
    | PREDEF_CALL(op), vel -> (
      if AstPredef.is_a_predef_op (snd(fst op.it)) then
        let op_str = snd (fst op.it) in
        let op_short_str = op2string (AstPredef.string_to_op op_str) in
        if AstPredef.is_infix (AstPredef.string_to_op op_str) then (
          let ve1, ve2 = cut_list vel in
             "("^(sov ve1) ^ ") " ^ op_short_str ^ " (" ^ (sov ve2) ^ ")")
        else 
          (op_short_str ^
             (match op_str with
              | "true" | "false"  ->  tuple vel
              | _ -> tuple_par vel 
             )
          )
      else
        let nk = op.it in
         if not global_opt.lv4 then
          ((string_of_node_key forprint nk) ^ (tuple_par vel))
        else
          ((string_of_node_key_rec forprint global_opt.no_prefix nk) ^ (tuple_par vel))
    )
    | CONST_REF idl, _ -> dump_long forprint idl
    | VAR_REF id, _ -> id
    | PRE, _ -> "pre "  ^ (tuple_par vel)
    | ARROW, [ve1; ve2] -> (* if global_opt.kcg then (
	                               "fby(" ^
                                  (if is_a_tuple ve2 then tuple_par [ve2] else string_of_val_exp_eff forprint ve2)
                                  ^ ";1;" ^
                                  (if is_a_tuple ve2 then tuple_par [ve1] else string_of_val_exp_eff forprint ve1) ^ ")"
                                  
	                               ) 
	                               else( *)
       (if is_a_tuple ve1 then tuple_par [ve1] else string_of_val_exp_eff forprint ve1) ^
         " -> " ^ 
           (if is_a_tuple ve1 then tuple_par [ve2] else string_of_val_exp_eff forprint ve2)
	          
    | FBY, [ve1; ve2] -> 
	    (* dead code ? *)
       if global_opt.lv4 then
         (if is_a_tuple ve1 then tuple_par [ve1] else string_of_val_exp_eff forprint ve1)
         ^ " -> pre " ^ 
           (if is_a_tuple ve1 then tuple_par [ve2] else string_of_val_exp_eff forprint ve2)
       else

	      if global_opt.kcg then (
	        "fby(" ^
             (if is_a_tuple ve2 then tuple_par [ve2] else string_of_val_exp_eff forprint ve2)
             ^ ";1;" ^
               (if is_a_tuple ve2 then tuple_par [ve1] else string_of_val_exp_eff forprint ve1) ^ ")"
                                                                                           
	      ) 
	      else( 
           (if is_a_tuple ve1 then tuple_par [ve1] else string_of_val_exp_eff forprint ve1)
           ^ " fby " ^ 
             (if is_a_tuple ve1 then tuple_par [ve2] else string_of_val_exp_eff forprint ve2)
	      )

    | WHEN clk, vel -> (tuple vel) ^ (string_of_clock clk)

    | CURRENT Some _,_ -> (* transform to merge in kcg mode *) 
	    if global_opt.kcg then assert false 
	    else
	      "current " ^ tuple_par (if global_opt.ec then List.tl vel else vel)
    | CURRENT None,_ -> "current " ^ tuple_par vel
    | TUPLE,_ -> (tuple vel)
    | CONCAT, [ve1; ve2] ->  
       (string_of_val_exp_eff forprint ve1) ^ " | " ^ (string_of_val_exp_eff forprint ve2)
    | HAT (i), [ve] -> (string_of_val_exp_eff forprint ve) ^ "^" ^ (string_of_int i)
    | HAT (_i), _ -> assert false
    | ARRAY, vel -> tuple_square vel
    | STRUCT_ACCESS(id), [ve1] ->
       (string_of_val_exp_eff forprint ve1) ^ "." ^ (Lv6Id.to_string id)

    | ARRAY_ACCES(i), [ve1] ->
       (string_of_val_exp_eff forprint ve1) ^ "[" ^ (string_of_int i) ^ "]"

    | ARRAY_SLICE(si_eff), [ve1] -> 
       (string_of_val_exp_eff forprint ve1) ^ (string_of_slice_info_eff si_eff)

    | ARRAY_SLICE(_), _ -> assert false (* todo *)

    (* Cannot happen *)
    | ARROW, _ -> assert false
    | FBY, _ -> assert false
    | CONCAT, _ -> assert false
    | STRUCT_ACCESS(_), _ -> assert false
    | ARRAY_ACCES(_i), _ -> assert false
  in
  let do_not_parenthesize = function 
    | VAR_REF _,_
    | CONST_REF _,_
    | PREDEF_CALL({it=("Lustre","true"),[];_}),_
    | PREDEF_CALL({it=("Lustre","false"),[];_}),_
    | ARRAY_ACCES _,_
    | STRUCT_ACCESS _,_ -> true   
    | _,_ ->  false 
  in 
  if 
    (* already parenthesized *)
    ( Str.string_match (Str.regexp "^(") str 0 && 
        Str.string_match (Str.regexp ")$") str 0 ) 
    || 
      (* ident or predef constants *)
      (do_not_parenthesize (posop.it,vel)) 
    ||
      global_opt.one_op_per_equation
  then 
    str 
  else 
    ("(" ^ str ^ ")")

and (cut_list : val_exp list -> val_exp * val_exp) =
  function
  | [] | [_] -> assert false
  | [ve1;ve2] -> ve1,ve2
  | vel -> (* sometimes, the flatenning has been too effective, hence 
              we build back the tuple. Actually it should not occur, 
              but... sigh... *)
     let s = (List.length vel)/2 in
     let f (cpt, l1, l2) ve =
       if cpt < s then cpt+1, ve::l1, l2 else cpt+1, l1, ve::l2
     in
     let _,vel1,vel2 = List.fold_left f (0,[],[]) vel in
     let vel1 = List.rev vel1 in
     let vel2 = List.rev vel2 in
     let ve1,ve2 = List.hd vel1, List.hd vel2 in
     let ve1 = { ve1 with ve_core = CallByPosLic({src=ve1.ve_src;it=TUPLE}, vel1)} in
     let ve2 = { ve2 with ve_core = CallByPosLic({src=ve2.ve_src;it=TUPLE}, vel2)} in
     (* ve_type, ve_clock, and ve_src are wrong, but we build this tuple only
      for printing purpose *)
     ve1,ve2
  
and string_of_val_exp_eff forprint ve = string_of_val_exp_eff_core forprint ve.ve_core
and string_of_val_exp_eff_core forprint ve_core = 
  match ve_core with
  | CallByPosLic (by_pos_op_eff, vel) ->
     (* ICI : on pourrait afficher en commentaire l'éventuel type_matches ? *)
     (string_of_by_pos_op_eff forprint by_pos_op_eff vel) 

  | Merge (ve, [({it=Bool_const_eff true ;_}, ct); ({it=Bool_const_eff false;_}, cf)]) 
  | Merge (ve, [({it=Bool_const_eff false;_}, cf); ({it=Bool_const_eff true;_}, ct)]) -> 
     if global_opt.lv4 then (
       "if " ^ (string_of_val_exp_eff forprint ve) ^ " then current (" ^
         (string_of_val_exp_eff forprint ct) ^ ") else current (" ^
           (string_of_val_exp_eff forprint cf) ^")"
     ) else (
	    if global_opt.kcg then (
         "merge ( " ^ (string_of_val_exp_eff forprint ve) ^ ";" ^
           (string_of_val_exp_eff forprint ct) ^ "when " ^(string_of_val_exp_eff forprint ve) ^ ";" ^
             (string_of_val_exp_eff forprint cf) ^ "when not " ^ (string_of_val_exp_eff forprint ve) ^ ")"
       ) else (
         "merge " ^ (string_of_val_exp_eff forprint ve) ^ " (true -> " ^
           (string_of_val_exp_eff forprint ct) ^ ") (false -> "^  (string_of_val_exp_eff forprint cf) ^")"
       )
     )
  | Merge (ve, cl) -> (
    if global_opt.lv4 then (
      let c1, cl = match cl with c1::cl -> c1,cl | [] -> assert false (*sno*) in
      let get_cond_and_then (_id,ve) =
        let clk = match ve.ve_clk with
          | [On((_cc,cv,Bool_type_eff),_)] -> cv
          | _ -> assert false (* SNO *)
        in
        let expr = string_of_val_exp_eff forprint ve in
        clk, expr
      in
      let print_case c =
        let clk,expr = get_cond_and_then c in
        Printf.sprintf " if %s then current(%s) else " clk expr
      in
      let cl_str = List.map print_case cl in
      let clk1,expr1 = get_cond_and_then c1 in
      let last_case = "current("^expr1^") (*"^clk1^"*)\n" in
      let str = (String.concat "" cl_str) ^ last_case in
      str
    ) else (
    "merge " ^ (string_of_val_exp_eff forprint ve) ^ " " ^
      (String.concat
         " " (List.map 
                (fun (id,ve) -> "( "^(string_of_const_eff forprint id.it) ^ " -> " ^ 
                                  (string_of_val_exp_eff forprint ve)^" )")
                cl)
      )
    )
  )
  | CallByNameLic(by_name_op_eff, fl) -> 
     (match by_name_op_eff.it with
      | STRUCT (long) -> (Lv6Id.string_of_long forprint long)
      | STRUCT_with (long, _dft) -> (Lv6Id.string_of_long forprint long)
      | STRUCT_anonymous -> ""          
     ) ^ (
      "{" ^ (String.concat ";" 
                           (List.map 
                              (fun (id,veff) -> 
                               let str = string_of_val_exp_eff forprint veff in
                               (Lv6Id.to_string id.it) ^ "=" ^ 
                                 (if is_a_tuple veff then ("("^ str^")") else str)
                              )
                              fl)) ^
        "}")


and wrap_long_line str = 
  if String.length str < 75 then str else
    let str_list = Str.split (Str.regexp "[ \t]+") str in
    let new_str, reste =
      List.fold_left
        (fun (accl, acc_str) str ->
         let new_acc_str = acc_str ^ " " ^ str in
         if 
           String.length new_acc_str > 75
         then
           (accl ^ acc_str ^ "\n\t" , str)
         else
           (accl, new_acc_str)
        )
        ("","")
        str_list
    in
    new_str ^ " " ^ reste


and string_of_eq_info_eff forprint (leff_list, vee) = 
  let str = string_of_val_exp_eff forprint vee in
  wrap_long_line (
      (string_of_leff_list forprint leff_list) ^ " = " ^ 
        (if is_a_tuple vee then ("("^ str^")") else str) ^ ";")

and (string_of_assert : bool -> Lic.val_exp srcflagged -> string ) =
  fun forprint eq_eff -> 
  wrap_long_line (
      if global_opt.kcg then "assume " ^ FreshName.local_var "A" ^ ": "
                             ^ string_of_val_exp_eff forprint eq_eff.it ^ ";"
      else
        "assert(" ^ string_of_val_exp_eff forprint eq_eff.it ^ ");")

and (string_of_eq : bool -> Lic.eq_info srcflagged -> string) =
  fun forprint eq_eff ->
  string_of_eq_info_eff forprint eq_eff.it

and wrap_long_profile str = 
  if String.length str < 75 then str else
    "\n"^(Str.global_replace (Str.regexp "returns") "\nreturns" str)

and (profile_of_node_exp_eff: bool -> Lic.node_exp -> string) =
  fun forprint neff ->
  ("(" ^ (string_of_type_decl_list forprint neff.inlist_eff "; ") ^ ") returns (" ^
	  (string_of_type_decl_list forprint neff.outlist_eff "; ") ^ ")")

and (string_of_node_def : bool -> Lic.node_def -> string list) = fun forprint -> 
  function
  | ExternLic
  | MetaOpLic
  | AbstractLic _ -> []
  | BodyLic node_body_eff -> 
     List.append
       (List.map (string_of_assert forprint) node_body_eff.asserts_eff)
       (List.map (string_of_eq forprint)node_body_eff.eqs_eff)

       

(* exported *)
and (type_decl: bool -> Lv6Id.long -> Lic.type_ -> string) =
  fun forprint tname teff ->
  if global_opt.kcg then
    match teff with
    | Enum_type_eff (_) -> 
       "type " ^ (dump_long forprint tname) ^ " = " ^ (string_def_of_type_eff forprint teff) ^ ";\n"
    | External_type_eff (_) 
    | Abstract_type_eff(_,External_type_eff (_)) -> 
       "type imported " ^ (dump_long forprint tname) ^  ";\n"
    | _ -> "type " ^ (dump_long forprint tname) ^ " = " ^ (string_def_of_type_eff forprint teff) ^ ";\n"
  else
    "type " ^ (dump_long forprint tname) ^ 
      (match teff with
       | Enum_type_eff (_) -> 
          " = " ^ (string_def_of_type_eff forprint teff) ^ ";\n"
       | External_type_eff (_) 
       | Abstract_type_eff(_,External_type_eff (_)) -> ";\n"
       | _ ->  " = " ^ (string_def_of_type_eff forprint teff) ^ ";\n"
      )
                          
(* exported *)
and (const_decl: bool -> Lv6Id.long -> Lic.const -> string) =
  fun forprint tname ceff -> 
  let begin_str = ("const " ^ (dump_long forprint tname)) in
  let end_str = (string_of_const_eff forprint ceff) ^ ";\n" in
  (match ceff with 
   | Enum_const_eff(_id, _t)  -> "" 
   | Extern_const_eff _
   | Abstract_const_eff _ ->
      if global_opt.kcg then 
	     "const imported " ^ (dump_long forprint tname) ^ " : " ^ 
          (string_of_type_eff forprint (Lic.type_of_const ceff)) ^ (";\n")
	   else
        begin_str ^ " : " ^ (string_of_type_eff forprint (Lic.type_of_const ceff)) ^ 
          (*                (if global_opt.ec then ".\n" else  *)
          (";\n")
   | Struct_const_eff _
   | Array_const_eff _
   | Bool_const_eff _
   | Int_const_eff _
   | Real_const_eff _ -> 
      if global_opt.kcg then 
        begin_str ^ ":" ^ (string_of_type_eff forprint (Lic.type_of_const ceff)) ^
          " = " ^ end_str  else begin_str ^ " = " ^ end_str
	| Tuple_const_eff _ ->
		print_internal_error "LicDump.const_decl" "should not have been called for a tuple";
		assert false
  ) 

    
(* exported *)
and node_of_node_exp_eff forprint (neff: Lic.node_exp): string =
  wrap_long_profile (
      (
        if neff.is_safe_eff then "" else "unsafe "
      )^(
        if neff.def_eff = ExternLic && not (global_opt.lv4) && not (global_opt.kcg)
        (* no extern kwd in v4 and in "scade"... *)
        then "extern " else ""
      )^(
         if global_opt.lv4 || global_opt.kcg then (
           (* node and function does not have the same meaning in scade and in lv4... *)
           if neff.def_eff = ExternLic then "function " else "node "
         ) else (
           if neff.has_mem_eff  then "node " else "function "
         )
       )^(if global_opt.kcg then
	         if neff.def_eff = ExternLic then "imported " else ""
	       else "")
	   ^(string_of_node_key_rec forprint  global_opt.no_prefix neff.node_key_eff)^(
        profile_of_node_exp_eff forprint neff
	   )
      ^
	     (match neff.def_eff with
         | ExternLic ->  ";\n"
         | MetaOpLic -> (

           (* on écrit juste un alias *)
           " = " ^(string_of_node_key_def forprint neff.node_key_eff)^ ";\n"
         )
         | AbstractLic _ -> "; \n"
         | BodyLic _ -> (
           (if global_opt.kcg then "\n" else ";\n") ^
             (match neff.loclist_eff with
              | None -> ""
              | Some [] -> ""
              | Some l ->
                 let l = Lic.sort_var_info l in
	              ("var\n   " ^ (string_of_type_decl_list forprint l ";\n   ") ^ ";\n"
	              )
             ) 
	          ^ "let\n   " ^
	            (String.concat "\n   " (string_of_node_def forprint neff.def_eff)) ^
	              "\ntel\n-- end of node " ^
	                (string_of_node_key_rec forprint
                      (not global_opt.no_prefix) neff.node_key_eff) ^ "\n")
	     )
    )
                    
and (string_of_clock_exp : AstCore.clock_exp -> string) = 
  function
  | AstCore.Base -> ""
  | AstCore.NamedClock clk -> 
     " when " ^ (string_of_ident_clk clk.it)

and (string_of_ident_clk : Lv6Id.clk -> string) =
  fun clk -> 
  let (cc,v) = clk in
  let clk_exp_str =
    match cc with
    | "Lustre","true" -> (Lv6Id.to_string v)
    | "Lustre","false" ->  "not " ^ (Lv6Id.to_string v)
    | _ ->
       (* if global_opt.lv4 || global_opt.ec then *)
       (* raise (Lv6errors.Global_error *)
       (* ("Cannot generate V4 style Lustre for programs with enumerated "^ *)
       (* "clocks (yet), sorry.")) *)
       (*  else *)
       Lv6Id.string_of_clk clk
  in
  clk_exp_str


(* exported *)
and string_of_clock2 (ck : Lic.clock) =
  match ck with
  | BaseLic -> " on base"
  | On((cc,cv,_),ceff) ->
     let clk_exp_str = string_of_ident_clk (cc,cv) in
     " on " ^ clk_exp_str ^ (string_of_clock2 ceff)
  | ClockVar i ->  " on 'CV" ^ string_of_int i
                                        
                                        
and string_of_clock (ck : Lic.clock) =
  match ck with
  | BaseLic -> ""
  | On((cc,cv,_),_) -> 
     let clk_exp_str = string_of_ident_clk (cc,cv) in
     " when " ^ clk_exp_str
  | ClockVar _ ->  
     "" (* it migth occur that (unused) constant remain with a clock var.
              But in that case, it is ok to consider then as on the base clock.
         *)
(*     | ClockVar i -> "_clock_var_" ^ (string_of_int i) *)

and op2string op = 
  (* Une verrue pour être compatible avec les outils qui mangent du ec...  *)
  if global_opt.ec && op = AstPredef.INT2REAL_n then "real"  else 
  if global_opt.ec && op = AstPredef.REAL2INT_n then "int"  else 
    AstPredef.op2string op

(*---------------------------------------------------------------------
Formatage standard des erreurs de compil
----------------------------------------------------------------------*)
let node_error_string _lxm nkey = (
   Printf.sprintf "While checking %s" (string_of_node_key_iter false nkey)
)

(*---------------------------------------------------------------------
Message d'erreur (associé à un lexeme) sur stderr
----------------------------------------------------------------------*)
let print_compile_node_error nkey lxm msg = (
  Printf.eprintf "%s\n" (node_error_string lxm nkey);
  Lv6errors.print_compile_error lxm msg ;
  flush stderr
)

let print_global_node_error lxm nkey msg = (
  Printf.eprintf "%s\n" (node_error_string lxm nkey);
  Lv6errors.print_global_error msg ;
  flush stderr
)

