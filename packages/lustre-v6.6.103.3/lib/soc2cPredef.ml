(* Time-stamp: <modified the 29/08/2019 (at 17:05) by Erwan Jahier> *)

(* A local exception used to check if a predef is supported. 
   The idea is that when gen_call_do is called  with empty lists,
   it means that it is called to check if the predef is supported.

   This (ugly but local) trick is to avoid to duplicate the match
   done in gen_call_do 
*)
exception Is_supported

let stdbin op ll rl =
	match (ll,rl) with
	| ([l], [r1;r2]) -> Printf.sprintf "  %s = %s %s %s;\n" l r1 op r2
	| ([],[]) -> raise Is_supported
	| _ -> assert false

(* to deal with == and <> over arrays *)
let stdbin_eq_array is_eq (_op,tl,_) ll rl =
  let cast = "(const void *)" in
  let ctype = match tl with
    | t::_ -> Printf.sprintf "sizeof(%s)" (Soc2cUtil.data_type_to_c t "")
    | [] -> assert false
  in
  let not_opt = if is_eq then  "" else  "!" in
	match (ll,rl) with
	| ([l], [r1;r2]) ->
      Printf.sprintf "  %s = %smemcmp(%s %s, %s %s, %s)==0;\n"
                     l not_opt cast r1 cast r2 ctype
	| ([],[]) -> raise Is_supported
	| _ -> assert false

(* to deal with == and <> over structs *)
let stdbin_eq_struct is_eq  (_op,tl,_) ll rl =
  let cast = "(const void *)" in
  let ctype = match tl with
    | t::_ -> Printf.sprintf "sizeof(%s)" (Soc2cUtil.data_type_to_c t "")
    | [] -> assert false
  in
  let not_opt = if is_eq then  "" else  "!" in
	match (ll,rl) with
	| ([l], [r1;r2]) ->
      Printf.sprintf "  %s = %smemcmp(%s &%s, %s &%s, %s)==0;\n"
                     l not_opt cast r1 cast r2 ctype
	| ([],[]) -> raise Is_supported
	| _ -> assert false

let stduna op ll rl = 
	match (ll,rl) with
	| ([l], [r]) -> Printf.sprintf "  %s = %s %s;\n" l op r
	| ([],[]) -> raise Is_supported
	| _ -> assert false

let stdimpl ll rl = 
	match (ll,rl) with
	| ([l], [r1;r2]) -> Printf.sprintf   "%s = !%s | %s;\n" l r1 r2
	| ([],[]) -> raise Is_supported
	| _ -> assert false


(* exported *)
(* ZZZ code dupl with SocPredef2cHeap.get_predef_op *)
let (gen_call_do :  Soc.key -> string list -> string list -> string) =
  fun sk vel_in vel_out ->
  (*   let (op,tl,_) = sk in *)
  let lstduna str = stduna str vel_in vel_out in
  let lstdbin str = stdbin str vel_in vel_out in
  let lstdimpl () = stdimpl vel_in vel_out in
  match sk with
  | "Lustre::uminus",_,_ -> lstduna "-"
  | "Lustre::iuminus",_,_ -> lstduna "-"
  | "Lustre::ruminus",_,_ -> lstduna "-"
  | "Lustre::not",_,_     -> lstduna "!"

  | "Lustre::mod",_,_    -> lstdbin "%" 
  | "Lustre::plus",_,_   -> lstdbin "+" 
  | "Lustre::iplus",_,_  -> lstdbin "+" 
  | "Lustre::rplus",_,_  -> lstdbin "+" 
  | "Lustre::times",_,_  -> lstdbin "*"
  | "Lustre::itimes",_,_ -> lstdbin "*"
  | "Lustre::rtimes",_,_ -> lstdbin "*"
  | "Lustre::div",_,_    -> lstdbin "/"
  | "Lustre::idiv",_,_   -> lstdbin "/"
  | "Lustre::rdiv",_,_   -> lstdbin "/"
  | "Lustre::slash",_,_  -> lstdbin "/"
  | "Lustre::islash",_,_ -> lstdbin "/"
  | "Lustre::rslash",_,_ -> lstdbin "/"
  | "Lustre::minus",_,_  -> lstdbin "-"
  | "Lustre::iminus",_,_ -> lstdbin "-"
  | "Lustre::rminus",_,_ -> lstdbin "-"

  | "Lustre::real2int",_,_ -> lstduna "(_integer)"
  | "Lustre::int2real",_,_ -> lstduna "(_real)"

  | "Lustre::lt",_,_  -> lstdbin "<"
  | "Lustre::ilt",_,_ -> lstdbin "<"
  | "Lustre::rlt",_,_ -> lstdbin "<"
  | "Lustre::gt",_,_  -> lstdbin ">"
  | "Lustre::igt",_,_ -> lstdbin ">"
  | "Lustre::rgt",_,_ -> lstdbin ">"
  | "Lustre::lte",_,_ -> lstdbin "<="
  | "Lustre::ilte",_,_-> lstdbin "<="
  | "Lustre::rlte",_,_-> lstdbin "<="
  | "Lustre::gte",_,_ -> lstdbin ">="
  | "Lustre::igte",_,_-> lstdbin ">="
  | "Lustre::rgte",_,_-> lstdbin ">="

  | "Lustre::and",_,_ -> lstdbin "&"
  | "Lustre::or",_,_  -> lstdbin "|"
  | "Lustre::xor",_,_ -> lstdbin "!="
  | "Lustre::impl",_,_ -> lstdimpl ()

  | (("Lustre::eq"|"Lustre::equal"),(Data.Array _)::_,_) ->
     stdbin_eq_array true sk vel_in vel_out 
  | (("Lustre::neq"|"Lustre::diff"),(Data.Array _)::_,_) ->
     stdbin_eq_array false sk vel_in vel_out
  | (("Lustre::eq"|"Lustre::equal"),(Data.Struct _)::_,_)->
     stdbin_eq_struct true sk vel_in vel_out
  | (("Lustre::neq"|"Lustre::diff"),(Data.Struct _)::_,_)->
     stdbin_eq_struct false sk vel_in vel_out

  | "Lustre::eq",_,_ -> lstdbin "=="
  | "Lustre::equal",_,_ -> lstdbin "=="
  | "Lustre::neq",_,_ -> lstdbin "!="
  | "Lustre::diff",_,_ -> lstdbin "!="

  | _ -> raise Not_found

let (gen_call :  Soc.key -> Soc.t -> string list -> string list -> string) =
  fun sk _soc vel_in vel_out -> 
    assert ((vel_in,vel_out) <> ([],[]));
    gen_call_do sk vel_in vel_out

let (is_call_supported : Soc.key -> bool) =
  fun sk -> 
    try ignore (gen_call_do sk [] []); assert false (* sno *)
    with 
      | Is_supported -> true
      | Not_found -> false  
