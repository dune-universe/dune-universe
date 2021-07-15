(* Time-stamp: <modified the 29/08/2019 (at 14:17) by Erwan Jahier> *)

open SocExecValue
open Data
open Soc

(* A boring but simple module... *)

let type_error v1 v2 =
  Printf.eprintf "Runtime error: '%s' and '%s' have different types.\n" 
    (Data.val_to_string string_of_float v1)  (Data.val_to_string string_of_float v2);
  flush stderr;
  exit 2

let type_error1 v1 str =
  Printf.eprintf "Runtime error: '%s' is not a '%s'\n" 
    (Data.val_to_string string_of_float v1) str;
  flush stderr;
  exit 2

let type_error2 v1 v2 str =
  Printf.eprintf "Runtime error: '%s' and/or '%s' are/is not a %s\n" 
    (Data.val_to_string string_of_float v1)  (Data.val_to_string string_of_float v2) str;
  flush stderr;
  exit 2

let (lustre_plus : ctx -> ctx) =
  fun ctx -> 
    let l = [get_val "i1" ctx; get_val "i2" ctx] in
    let (vn,vv) =
      match l with
        | [I x1; I x2] -> "out"::ctx.cpath,I(x1+x2)
        | [F i1; F i2] -> "out"::ctx.cpath,F(i1+.i2)
        | [U; _] | [_;U] -> "out"::ctx.cpath,U
        | [v1;v2]  ->  type_error v1 v2 (* ; "out"::ctx.cpath,U *)
        | _ -> assert false
    in
    { ctx with s = sadd ctx.s vn vv }

let lustre_times ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [I x1; I x2] -> "out"::ctx.cpath,I(x1 * x2)
      | [F x1; F x2] -> "out"::ctx.cpath,F(x1 *. x2)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error v1 v2 (* ; "out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }


let lustre_div ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [I x1; I x2] -> "out"::ctx.cpath,I(x1 / x2)
      | [F x1; F x2] -> "out"::ctx.cpath,F(x1 /. x2)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error v1 v2 (* ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_slash ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [I x1; I x2] -> "out"::ctx.cpath,I(x1 / x2)
      | [F x1; F x2] -> "out"::ctx.cpath,F(x1 /. x2)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error v1 v2
      (*                    ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }


let lustre_minus ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [I x1; I x2] -> "out"::ctx.cpath,I(x1 - x2)
      | [F x1; F x2] -> "out"::ctx.cpath,F(x1 -. x2)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error v1 v2
      (*                    ; "out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_mod ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [I x1; I x2] -> "out"::ctx.cpath,I(x1 mod x2)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error v1 v2
      (*                    ; "out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }


let lustre_eq ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [x1; x2] -> "out"::ctx.cpath,B(x1 = x2)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_uminus ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx]) with
      | [I x1] -> "out"::ctx.cpath,I(- x1)
      | [F x1] -> "out"::ctx.cpath,F(-. x1)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1]  ->  type_error1 v1 "numeric"
      (*                 ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_real2int ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx]) with
      | [F x1] -> "out"::ctx.cpath,I(int_of_float x1)
      | [U] -> "out"::ctx.cpath,U
      | [v1]  ->  type_error1 v1 "real"
      (*                 ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }


let lustre_int2real ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx]) with
      | [I x1] -> "out"::ctx.cpath,F(float_of_int x1)
      | [U] -> "out"::ctx.cpath,U
      | [v1]  ->  type_error1 v1 "int"
      (*                 ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }


let lustre_not ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx]) with
      | [B x1] -> "out"::ctx.cpath,B(not x1)
      | [U] -> "out"::ctx.cpath,U
      | [v1]  ->  type_error1 v1 "bool"
      (*                 ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }


let lustre_lt ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [x1; x2] -> "out"::ctx.cpath,B(x1 < x2)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }
 
let lustre_gt ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [x1; x2] -> "out"::ctx.cpath,B(x1 > x2)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_lte ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [x1; x2] -> "out"::ctx.cpath,B(x1 <= x2)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_gte ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [x1; x2] -> "out"::ctx.cpath,B(x1 >= x2)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_and ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [B x1; B x2] -> "out"::ctx.cpath,B(x1 && x2)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error2 v1 v2 "bool"
      (*                    ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_xor ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [B x1; B x2] -> "out"::ctx.cpath,B(x1 <> x2)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error2 v1 v2 "bool"
      (*                    ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }


let lustre_neq  ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [x1; x2] -> "out"::ctx.cpath,B(x1 <> x2)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }


let lustre_or  ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [B x1; B x2] -> "out"::ctx.cpath,B(x1 || x2)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error2 v1 v2 "bool"
      (*                    ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }


let lustre_impl ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [B x1; B x2] -> "out"::ctx.cpath,B(not x1 || x2)
      | [U; _] | [_;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error2 v1 v2 "bool"
      (*                    ;"out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_if ctx =
  let (vn,vv) = 
    match ([get_val "cond" ctx; get_val "xthen" ctx; get_val "xelse" ctx]) with
      | [B c; v1; v2]  -> "out"::ctx.cpath, if c then v1 else v2
      | [U;_; _] | [_;U;U] -> "out"::ctx.cpath,U
      | [v1;v2]  ->  type_error v1 v2
      (*                    ; "out"::ctx.cpath,U *)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }
 

let make_names str start stop = 
  let res = ref [] in
  for k=stop downto start do
    res:= (str^(string_of_int k)) :: !res;
  done;
  !res


let lustre_array tl ctx =
  let _t,size = match List.hd (List.rev tl) with
    | Data.Array(t,i) -> t,i
    | _ -> assert false
  in
  let inames = make_names "i" 1 size in
  let l = List.map (fun name -> get_val name ctx) inames in
  let a = Array.of_list l in
  { ctx with s = sadd ctx.s ("out"::ctx.cpath) (A a) }

let lustre_slice tl si_opt ctx =
  let _t,size = match List.hd (List.rev tl) with
    | Data.Array(t,i) -> t,i
    | _ -> assert false
  in
  let (vn,vv) = 
    match ([get_val "i1" ctx], si_opt) with
      | [A a],Slic(b,e,step) -> 
        let a_res = Array.make size a.(0) in
        let j=ref 0 in
        for i = b to e do
          if (i-b) mod step = 0 then (
            a_res.(!j) <- a.(i);
            incr j
          );
        done;
        "out"::ctx.cpath, A (a_res)
      | [U],_ -> "out"::ctx.cpath, U
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_concat ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx]) with
      | [A a1; A a2]  -> "out"::ctx.cpath, A (Array.append a1 a2)
      | [U;_] | [_;U] -> "out"::ctx.cpath, U
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }

let lustre_arrow ctx =
  let (vn,vv) = 
    match ([get_val "i1" ctx; get_val "i2" ctx;
            get_val "_memory" { ctx with cpath=ctx.cpath}]) 
    with
      | [v1;v2; fs]  -> "out"::ctx.cpath, if fs=B false then v2 else v1
      | _  -> assert false
  in
  let ctx = { ctx with s = sadd ctx.s vn vv } in
  let ctx = { ctx with s = sadd ctx.s ("_memory"::ctx.cpath) (B false) } in
  ctx

let lustre_hat tl ctx =
  let i = match tl with
    | [_;Data.Array(_,i)] -> i
    | _ -> assert false
  in
  let (vn,vv) = 
    match ([get_val "i1" ctx]) with
      | [U]  -> "out"::ctx.cpath,U
      | [v]  -> "out"::ctx.cpath,A(Array.make i v)
      | _  -> assert false
  in
  { ctx with s = sadd ctx.s vn vv }
 
(* exported *)
let (get: Soc.key -> (ctx -> ctx)) =
  fun (n,tl,si_opt) -> 
    match n with
    | "Lustre::rplus" 
    | "Lustre::iplus" 
    | "Lustre::plus" -> lustre_plus
    | "Lustre::itimes"
    | "Lustre::rtimes"
    | "Lustre::times"-> lustre_times 
    | "Lustre::idiv"
    | "Lustre::rdiv"
    | "Lustre::div"  -> lustre_div 
    | "Lustre::slash" | "Lustre::islash" | "Lustre::rslash" -> lustre_slash
    | "Lustre::iminus"
    | "Lustre::rminus"
    | "Lustre::minus"-> lustre_minus

    | "Lustre::mod" -> lustre_mod
    | "Lustre::iuminus"
    | "Lustre::ruminus"
    | "Lustre::uminus" -> lustre_uminus
    | "Lustre::not" -> lustre_not 
    | "Lustre::real2int" -> lustre_real2int
    | "Lustre::int2real" -> lustre_int2real

    | "Lustre::lt"| "Lustre::rlt" | "Lustre::ilt"  -> lustre_lt
    | "Lustre::gt"| "Lustre::rgt" | "Lustre::igt"  -> lustre_gt
    | "Lustre::lte"| "Lustre::rlte" | "Lustre::ilte" -> lustre_lte
    | "Lustre::gte"| "Lustre::rgte"| "Lustre::igte" -> lustre_gte

    | "Lustre::xor" -> lustre_xor
    | "Lustre::and" -> lustre_and 
    | "Lustre::eq" -> lustre_eq 
    | "Lustre::neq" -> lustre_neq 
    | "Lustre::or"  -> lustre_or 

    | "Lustre::impl" -> lustre_impl

    | "Lustre::if"| "Lustre::rif"| "Lustre::iif" -> lustre_if

    | "Lustre::hat" -> lustre_hat tl
    | "Lustre::array" -> lustre_array tl
    | "Lustre::concat" -> lustre_concat

    | "Lustre::arrow" -> lustre_arrow
    | "Lustre::current" -> assert false
    
    | "Lustre::array_slice" -> lustre_slice tl si_opt

    | "Lustre::nor" -> assert false (* ougth to be translated into boolred *)
    | "Lustre::diese" -> assert false (* ditto *)
    | _ -> raise Not_found


