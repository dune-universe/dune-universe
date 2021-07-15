(* Time-stamp: <modified the 06/03/2020 (at 13:35) by Erwan Jahier> *)

open Soc2cIdent
open Data


type var_kind = (* XXX poor names: fixme! *)
  | ML_IO of Soc.key (* For memoryless soc in Heap mode *)
  | M_IO (* *)
  | Local (* for soc local variables *)

let (type_to_string : Data.t -> string -> string) = 
  fun v n -> 
    (* in order to print arrays of arrays type size in the good order, 
       we accumulate the array size when v of of type array, and
       we print it otherwise (finish).
    *)
    let rec finish acc str =
      match acc with
        |[] -> str
        | s::ts -> Printf.sprintf "%s[%d]" (finish ts str) s 
    in 
    let rec aux acc v n =
      match v with
        | Bool -> finish acc ("_boolean "^n)
        | Int -> finish acc ("_integer "^n)
        | Real-> finish acc ("_real "^n)
        | String -> finish acc ("_string "^n)
        | Extern s -> finish acc ((id2s s)^" "^n)
        | Enum  (s, _sl) -> finish acc (id2s s ^" "^n)
        | Struct (sid,_) -> finish acc ((id2s sid)^" "^n)
        | Array (ty, sz) -> aux (sz::acc) ty n
        | Alpha nb -> finish acc ("alpha_"^(string_of_int nb)^" "^n) 
        | Alias(a,_) -> finish acc (a^" "^n)
    in
    aux [] v n

let string_of_flow_decl (id, t) = 
  Printf.sprintf "   %s;\n" (type_to_string t (id2s id)) 

open Soc


let string_of_flow_decl_w7annot gaol (id, t) = 
  let decl = string_of_flow_decl (id, t) in
  let get_pre_var inst =
    let rec aux gaol =
      match gaol with
      | [] -> None
      | gao::rest -> (
        match gao with
        | Case(_,l,_) -> (
           let f acc (_,gaol) =
             match acc with
             | None -> aux gaol
             | Some x -> Some x
           in
           match List.fold_left f None l with
           | Some x -> Some x
           | None -> aux rest
        )
        | Call(_, Method((inst2,("Lustre::pre",_,_)),"set"),outs,_) ->
           if inst2 = inst then
             let output_list = List.map SocUtils.lustre_string_of_var_expr outs in
             let id2 = String.concat "," output_list in
             Some id2
           else 
             aux rest
        | _ ->
           aux rest
      )
    in
    match aux gaol with
    | None ->
       Printf.printf "Did not find the def of the pre of %s (%s)\n" id inst ;
       flush stdout;
       assert false (* SNO *)
    | Some x -> x
  in
  let string_of_atomic_operation ao ins =
    let input_list = List.map SocUtils.lustre_string_of_var_expr ins in
    match ao, input_list with
    | Assign,_ -> None
    | Procedure ("Lustre::ruminus",_,_), [a] -> Some (Printf.sprintf "-%s" a)
    | Procedure ("Lustre::iuminus",_,_), [a] -> Some (Printf.sprintf "-%s" a)
    | Procedure ("Lustre::uminus",_,_), [a] -> Some (Printf.sprintf "-%s" a)
    | Procedure ("Lustre::not",_,_), [a] -> Some (Printf.sprintf "not(%s)" a)
    | Procedure ("Lustre::real2int",_,_), [a] -> Some (Printf.sprintf "real2int(%s)" a)
    | Procedure ("Lustre::int2real",_,_), [a] -> Some (Printf.sprintf "int2real(%s)" a)
    | Procedure ("Lustre::mod",_,_), [a;b] -> Some (Printf.sprintf "%s mod %s" a b)
    | Procedure ("Lustre::iplus" ,_,_), [a;b] -> Some (Printf.sprintf "%s+%s" a b)
    | Procedure ("Lustre::rplus" ,_,_), [a;b] -> Some (Printf.sprintf "%s+%s" a b)
    | Procedure ("Lustre::plus" ,_,_), [a;b] -> Some (Printf.sprintf "%s+%s" a b)
    | Procedure ("Lustre::times",_,_), [a;b] -> Some (Printf.sprintf "%s*%s" a b)
    | Procedure ("Lustre::itimes",_,_), [a;b] -> Some (Printf.sprintf "%s*%s" a b)
    | Procedure ("Lustre::rtimes",_,_), [a;b] -> Some (Printf.sprintf "%s*%s" a b)
    | Procedure ("Lustre::slash",_,_), [a;b] -> Some (Printf.sprintf "%s/%s" a b)
    | Procedure ("Lustre::islash",_,_), [a;b] -> Some (Printf.sprintf "%s/%s" a b)
    | Procedure ("Lustre::rslash",_,_), [a;b] -> Some (Printf.sprintf "%s/%s" a b)
    | Procedure ("Lustre::div",_,_), [a;b] -> Some (Printf.sprintf "%s/%s" a b)
    | Procedure ("Lustre::idiv",_,_), [a;b] -> Some (Printf.sprintf "%s/%s" a b)
    | Procedure ("Lustre::rdiv",_,_), [a;b] -> Some (Printf.sprintf "%s/%s" a b)
    | Procedure ("Lustre::minus",_,_), [a;b] -> Some (Printf.sprintf "%s-%s" a b)
    | Procedure ("Lustre::iminus",_,_), [a;b] -> Some (Printf.sprintf "%s-%s" a b)
    | Procedure ("Lustre::rminus",_,_), [a;b] -> Some (Printf.sprintf "%s-%s" a b)
    | Procedure ("Lustre::lt" ,_,_), [a;b] -> Some (Printf.sprintf "%s<%s" a b)
    | Procedure ("Lustre::gt" ,_,_), [a;b] -> Some (Printf.sprintf "%s>%s" a b)
    | Procedure ("Lustre::lte",_,_), [a;b] -> Some (Printf.sprintf "%s<=%s" a b)
    | Procedure ("Lustre::gte",_,_), [a;b] -> Some (Printf.sprintf "%s>=%s" a b)
    | Procedure ("Lustre::ilt" ,_,_), [a;b] -> Some (Printf.sprintf "%s<%s" a b)
    | Procedure ("Lustre::igt" ,_,_), [a;b] -> Some (Printf.sprintf "%s>%s" a b)
    | Procedure ("Lustre::ilte",_,_), [a;b] -> Some (Printf.sprintf "%s<=%s" a b)
    | Procedure ("Lustre::igte",_,_), [a;b] -> Some (Printf.sprintf "%s>=%s" a b)
    | Procedure ("Lustre::rlt" ,_,_), [a;b] -> Some (Printf.sprintf "%s<%s" a b)
    | Procedure ("Lustre::rgt" ,_,_), [a;b] -> Some (Printf.sprintf "%s>%s" a b)
    | Procedure ("Lustre::rlte",_,_), [a;b] -> Some (Printf.sprintf "%s<=%s" a b)
    | Procedure ("Lustre::rgte",_,_), [a;b] -> Some (Printf.sprintf "%s>=%s" a b)
    | Procedure ("Lustre::impl",_,_), [a;b] -> Some (Printf.sprintf "%s=>%s" a b)
    | Procedure ("Lustre::xor",_,_), [a;b] -> Some (Printf.sprintf "%s xor %s" a b) 
    | Procedure ("Lustre::neq",_,_), [a;b] -> Some (Printf.sprintf "not(%s=%s)" a b) 
    | Procedure ("Lustre::eq",_,_), [a;b] -> Some (Printf.sprintf "%s=%s" a b) 
    | Procedure ("Lustre::or",_,_), [a;b] -> Some (Printf.sprintf "%s or %s" a b) 
    | Procedure ("Lustre::and",_,_), [a;b] -> Some (Printf.sprintf "%s and %s" a b)
    | Procedure ("Lustre::if",_,_),[c;t;e] ->
       Some (Printf.sprintf "if %s then %s else %s" c t e)
            
    | Method((_,("Lustre::arrow",_,_)),_), [a;b] -> Some (Printf.sprintf "%s -> %s" a b)
    | Method((_,("Lustre::current",_,_)),_), [_;b] ->
       Some (Printf.sprintf "current(%s)" b)
            
    | Method((inst,("Lustre::pre",_,_)),"get"),_ ->
       let id = get_pre_var inst in
        Some (Printf.sprintf "pre(%s)" id)
            
    | Method((_,(id,_,_)),_),_ 
    | Procedure(id,_,_),_ ->
       let inputs = String.concat "," input_list in
       let id = if String.length id > 8 && String.sub id 0 8 = "Lustre::" then
                  String.sub id 8 ((String.length id) - 8) else id
       in
       let id = if id = "diese" then "#" else id in
       if inputs = "" then None else
         Some (Printf.sprintf "%s(%s)" id inputs)
  in
  let rec get_def_gao id gao =
    match gao with
    | Case(id2,l,_) ->
       let f acc (cv,gaol) =
         match acc with
         | None -> (
             match get_def id gaol, cv with
             | None,_ -> None
             | Some res, "true" -> (* XXX: non !*)
                 Some ("("^res ^ ") when " ^ id2) 
             | Some res, "false" ->
                 Some ("("^res ^ ") when not " ^ id2) 
             | Some res, _ -> 
                 Some (Printf.sprintf "(%s) when %s(%s)" res id2 cv) 
            )
         | Some x -> Some x
       in
       List.fold_left f None l
    | Call([Var(v,_)],Assign,[Var(v2,_)],_) ->
       if v = id then Some (Printf.sprintf "%s=%s" v v2) else None
    | Call(_::_,Assign,_ins,_) -> 
       (*        Printf.printf " no def found for %s (tuple assign)\n" id;flush stdout; *)
       None
    | Call([Var(v,_)],ao,ins,_) ->
       if v = id then string_of_atomic_operation ao ins else None
    | Call([_],_,_,_) -> None (* can something be done here? *)     
    | Call([],_,_,_) -> None (* and here? *)
    | _ -> None
  and get_def id gaol =
    match gaol with
    | [] -> None
    | gao::rest ->
       (match get_def_gao id gao with
        | None -> get_def id rest
        | Some x -> Some x
       )
  in
  match get_def id gaol with
  | None ->
     (*      Printf.printf " no def found for %s\n" id;flush stdout; *)
     decl
  | Some expr -> 
     Printf.sprintf
       "%s
/*
LUSDEF:%s
%s
LUSEND
*/
" 
       decl id expr


let (data_type_to_c: Data.t  -> string -> string) =
  fun v n -> 
    let rec finish acc str =
      match acc with
        |[] -> str
        | s::ts -> Printf.sprintf "%s[%d]" (finish ts str) s
    in 
    let rec aux acc v n =
      match v with
        | Bool -> finish acc ("_boolean "^n)
        | Int -> finish acc ("_integer "^n)
        | Real-> finish acc ("_real "^n)
        | String-> finish acc ("_string "^n)
        | Extern s -> finish acc ((id2s s)^" "^n)
        | Enum  (s, _sl) -> finish acc (id2s s ^" "^n)
        | Struct (sid,_) -> finish acc ((id2s sid)^" "^n)
        | Array (ty, sz) -> aux (sz::acc) ty n
        | Alpha nb -> finish acc ("alpha_"^(string_of_int nb)^" "^n) 
        | Alias(a,_) -> finish acc (a^" "^n)
    in
    aux [] v n

let (lic_type_to_c: Lic.type_  -> string -> string) =
  fun t n -> 
    match t with
    | Lic.Struct_type_eff (_name, fl) ->
      let field_to_c (id,(tf,_opt)) = 
        Printf.sprintf "\n   %s;" (data_type_to_c (Lic2soc.lic_to_data_type tf) (id2s id)) 
      in
      ((Printf.sprintf "struct { %s\n  }" 
          (String.concat "" (List.map field_to_c fl)))^ " " ^ n) 
    | Lic.Enum_type_eff (_name, _l) -> "_integer"^ " " ^ n              
    | _ -> type_to_string (Lic2soc.lic_to_data_type t) n


let (_lic_type_to_c_old: Lic.type_  -> string -> string) =
  fun t n -> match t with
    | Lic.Bool_type_eff -> "_boolean"^ " " ^ n
    | Lic.Int_type_eff  -> "_integer"^ " " ^ n
    | Lic.Real_type_eff -> "_real"^ " " ^ n
    | Lic.External_type_eff (name) -> (long2s name)^ " " ^ n
    | Lic.Abstract_type_eff (_name, t) -> lic_type_to_c t n
    | Lic.Enum_type_eff (_name, _l) -> "_integer"^ " " ^ n
    | Lic.Array_type_eff (ty, sz) -> 
      Printf.sprintf "%s %s[%d]" (lic_type_to_c ty "") n sz
    | Lic.Struct_type_eff (name, fl) ->
      let field_to_c (id,(tf,_opt)) = 
        Printf.sprintf "  %s %s;\n" (lic_type_to_c tf "")  (id2s id)
      in
      (Printf.sprintf "struct %s { %s }" 
        (long2s name) 
        (String.concat "" (List.map field_to_c fl)))^ " " ^ n
    | Lic.TypeVar Lic.Any -> assert false
    | Lic.TypeVar Lic.AnyNum -> assert false


(* exported *)
let (gen_c_switch : string -> (string * string) list -> string) = 
  fun cond_var cases ->
    if Lv6MainArgs.global_opt.Lv6MainArgs.soc2c_no_switch then
      let l = List.map
                (fun (v,code) ->
                 Printf.sprintf "  if (%s == %s) {\n%s\n  } else {\n" cond_var v code) 
                cases
      in
      let switch = String.concat "" l in
      let closing_curly = String.concat "" (List.map (fun _ -> "}") cases) in
      Printf.sprintf "%s  %s" switch closing_curly
    else
        let case_list = List.map
          (fun (v, code) -> Printf.sprintf "  case %s:\n%s  break;\n" v code) cases
        in
        let cases = String.concat "" case_list in
        Printf.sprintf "  switch (%s){\n%s}\n" cond_var cases


