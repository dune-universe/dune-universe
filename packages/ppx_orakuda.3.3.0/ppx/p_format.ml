open Ast_405
open Ppxx.Helper
open Lexformat

let parse specials loc s =
  let t, (abss, apps), fmt, rems = Lexformat.from_string_to_classic specials s in
  let string_constant =
    List.for_all (function 
      | String _ | Char _ | Escaped _ -> true
      | Conv _ -> false) t
  in
  let v = 
    if string_constant then `Const (Exp.string ~loc fmt)
    else 
      `Fun (abss, fun fnct ->
	let base = 
          [%expr 
              [%e fnct ] 
              [%e Exp.string ~loc fmt] 
          ] 
        in
	let id n = Printf.sprintf "id%d" n in
	let rec put_apps e = function
	  | [] -> e
	  | `Var n ::xs -> 
	      let id = id n in
	      put_apps 
                [%expr 
                  [%e e]
                  [%e Exp.var id]
                ] 
                xs
	  | `Applied ((Arg_expr str | Arg_var str), _pos) :: xs ->
	      put_apps 
                [%expr
                    [%e e]
                    [%e Exp.parse str ]
                ]
		xs
	  | `Applied (Arg_rex_ref var, _pos) :: xs ->
              let meth = match var with
                | '0' .. '9' -> Printf.sprintf "_%c" var
                | '`' -> "_left"
                | '\'' -> "_right"
                | '&' -> "_0"
                | '+' -> "_last"
                | _ -> assert false
              in
	      put_apps 
	        [%expr
                  [%e e]
                  [%e Exp.send [%expr __rex_group] !@meth ]
                ]
		xs
	in
	let rec put_abss e = function
	  | [] -> e
	  | _n::ns ->
	      put_abss 
                [%expr 
                    Exp.fun_ "" None (Pat.var id) [%e e]
                ]
                ns
	in
	put_abss (put_apps base apps) abss)
  in
  v, rems
