open Ppxx.Helper
open Ppxx.Utils
open Lexformat
open List

exception Error of conversion * string

let format_for_c conv =
  let ctype = match conv.ctype with
    | CF -> Cf (* to get "%f" *)
    | ctype -> ctype
  in
  let conv = { conv with inlined_arg = None; ctype = ctype } in
  compile_conversion conv

(* check we need to call c primitives *)
let c_primitive conv =
  try
    Some (match conv.ctype with
    | Cd | Cu | Cx | CX | Co -> "int"
    | Cf | Ce | CE | Cg | CG -> "float"
    | Cld | Clu | Clx | ClX | Clo -> "int32"
    | Cnd | Cnu | Cnx | CnX | Cno -> "natint"
    | CLd | CLu | CLx | CLX | CLo -> "int64"
    | CF -> "camlfloat"
    | _ -> raise Not_found)
  with
  | Not_found -> None

let with_width_or_precision conv =
  conv.width <> None || conv.precision <> None

let flush_buffer loc buf =
  let contents = Buffer.contents buf in
  Buffer.clear buf;
  if contents = "" then None
  else 
    Some (* <:expr< __cformat_out.Ppx_orakuda.Cformat.string $str:contents$ >> *)
    [%expr 
        __cformat_out.Ppx_orakuda.Cformat.string 
        [%e Exp.string ~loc contents ]
    ] [@metaloc loc]

let xid = function
  | -1 -> "__cformat_out"
  | n -> Printf.sprintf "__cformat_x%d" n 

let compile_conv 
    loc 
    buf (* static, non % part *)
    pos (* arg position *) 
    conv 
    : _ * (int * (Lexformat.inlined_arg * int) option) list
    =
  if conv.ctype = Cpercent then begin
    Buffer.add_char buf '%';
    [], []
  end else 
    let e, abss = 
      let with_adv_pos ?(by=1) e = pos := !pos + by; e in
      let get_pos ?(add=0) () = 
        Exp.var & xid & !pos + add
      in
      match c_primitive conv with
      | Some typ -> 
          begin match format_for_c conv with
	  | [] -> assert false
	  | [ `String fmt ] -> 
              let format = 
                Exp.ident & lid & "Ppx_orakuda.Cformat.format_" ^ typ
              in
    	      with_adv_pos 
                [%expr __cformat_out.Ppx_orakuda.Cformat.string 
		    ( [%e format ]
                        [%e Exp.string fmt ]
                        [%e get_pos () ]
                    )
                ],
	      [!pos, conv.inlined_arg]
	  | tokens -> 
	      let rev_parameters, rev_tokens = 
		fold_left (fun (params, tokens) token ->
		  match token with
		  | `String s -> (params, Exp.string s :: tokens)
		  | `Star -> 
		      let param = !pos in 
		      let var = get_pos () in
		      incr pos;
		      (param :: params, var :: tokens))
		  ([], []) tokens
	      in
	      let parameters = rev rev_parameters in
	      let tokens = rev rev_tokens in
	      let fmt = 
		fold_left (fun acc exp ->
                  [%expr [%e acc] ^ [%e exp]]) (hd tokens) (tl tokens)
	      in
              with_adv_pos
                [%expr 
                    __cformat_out.Ppx_orakuda.Cformat.string
		    ([%e Exp.ident & lid & "Ppx_orakuda.Cformat.format_" ^ typ ]
                        [%e fmt]
                        [%e get_pos ()]
                    )
                ],
	      map (fun param -> param, None) parameters @ [!pos, conv.inlined_arg]
          end
      | None ->
          match conv.ctype with
          | Cpercent 
          | Cd | Cu | Cx | CX | Co
          | Cf | Ce | CE | Cg | CG
          | Cld | Clu | Clx | ClX | Clo
          | Cnd | Cnu | Cnx | CnX | Cno
          | CLd | CLu | CLx | CLX | CLo
          | CF -> assert false
                  
          | (Cs | CS | Cc | CC | CB | Ca | Ct 
    	    | Cformat _ | Cformat_subst _ | Cflush ) 
    	      when with_width_or_precision conv ->
    	      raise (Error (conv, "no width nor precision allowed"))
                    
          | Cs -> 
	      with_adv_pos 
		[%expr 
                    __cformat_out.Ppx_orakuda.Cformat.string 
		    [%e get_pos ()]
                ],
	      [!pos, conv.inlined_arg]
          | CS -> 
	      with_adv_pos 
		[%expr 
                    __cformat_out.Ppx_orakuda.Cformat.caml_string
		    [%e get_pos ()]
                ],
	      [!pos, conv.inlined_arg]
		    
          | Cc -> 
	      with_adv_pos 
		[%expr
                    __cformat_out.Ppx_orakuda.Cformat.char 
                    [%e get_pos ()]
                ],
	      [!pos, conv.inlined_arg]
          | CC -> 
	      with_adv_pos 
		[%expr 
                    __cformat_out.Ppx_orakuda.Cformat.caml_char 
		    [%e get_pos ()]
                ],
              [!pos, conv.inlined_arg]
          | CB -> 
	      with_adv_pos 
		[%expr 
                    __cformat_out.Ppx_orakuda.Cformat.bool 
                    [%e get_pos ()]
                ],
	        [!pos, conv.inlined_arg]
          | Ca -> 
              (* Ca cannot take ${} *)
	      if conv.inlined_arg <> None then 
    	        raise (Error (conv, 
			      "cannot take inlined argument. use %t"));
    	      with_adv_pos ~by:2
    	        [%expr
		    __cformat_out.Ppx_orakuda.Cformat.formatf 
		    [%e get_pos ()]
		    [%e get_pos ~add:1 ()] 
                ],
	      [!pos, None; !pos+1, None]
          | Ct -> 
    	      with_adv_pos
    	        [%expr
                    __cformat_out.Ppx_orakuda.Cformat.formatf 
		    [%e get_pos ()]
                    ()
                ],
	      [!pos, conv.inlined_arg]
          | Cflush -> 
	      (* no ${} *)
	      if conv.inlined_arg <> None then 
    	        raise (Error (conv,
			      "cannot take inlined argument"));
	      [%expr
                __cformat_out.Ppx_orakuda.Cformat.flush ()
              ], 
              []
          | _ -> raise (Error (conv, "not supported")) 
        in
        let es = 
          match flush_buffer loc buf with
          | None -> [e]
          | Some e' -> [e'; e]
        in
        es, abss

let compile_token loc buf pos = function
  | String s -> Buffer.add_string buf s; [], []
  | Char c -> Buffer.add_char buf c; [], []
  | Escaped c -> Buffer.add_char buf c; [], []
  | Conv conv -> compile_conv loc buf pos conv



let compile loc tokens = 
  with_loc loc & fun () ->
    let buf = Buffer.create 128 in
    let pos = ref 0 in
    let es, abss =
      let es_list, abss_list = 
        split (map (compile_token loc buf pos) tokens)
      in
      let es = 
        let es = concat es_list in
        match flush_buffer loc buf with
        | None -> es
        | Some e -> es @ [e] (* CR jfuruse: ugly *)
      in
      let abss = concat abss_list in
      es, abss
    in
    let put_abstract e i = 
      [%expr fun [%p Pat.var' & xid i] -> [%e e ] ] 
    in
    let rec abstract e = function
      | [] -> e
      | (i,_)::is -> put_abstract (abstract e is) i
    in
    (* code *)
    let e = 
      fold_right (fun e acc ->
        [%expr [%e e]; [%e acc]]) es 
      [%expr __cformat_out.Ppx_orakuda.Cformat.finish () ]
    in 
    let abss = (-1, None) :: abss in
    let e =
      abstract
        [%expr 
          ( [%e 
                abstract e 
                (filter (function (_, None) -> true | _ -> false)  abss)
            ]
  	: (_,_) Ppx_orakuda.Cformat.t)
        ]
        (filter (function (_, None) -> false | _ -> true) abss) 
    in
    
    let put_app e = function
      | (_, None) -> e
      | (_, Some (arg, _pos)) ->
  	let str =
  	  match arg with
            | (Arg_expr str | Arg_var str) -> str
            | Arg_rex_ref var ->
  	      match var with
  	      | '0' .. '9' -> Printf.sprintf "_%c" var
  	      | '`' -> "_left"
  	      | '\'' -> "_right"
  	      | '&' -> "_0"
  	      | '+' -> "_last"
  	      | _ -> assert false
  	in
    	(* CR jfuruse: use pos *)
    	[%expr [%e e] [%e Exp.parse str]] (* CR jfuruse: Exp.parse misses loc *)
    in
    let put_apps e = 
      fold_left put_app e abss
    in
    e, put_apps

let parse specials loc attrs s =
  match Lexformat.from_string specials s with
  | _, Some rem -> 
      let pos = String.length s - String.length rem in
      raise (Lexformat.Error (pos, pos + 1,
		              Printf.sprintf "Unescaped special character %C found" rem.[0]))
  | t, None -> 
      let e, inlined_f = compile loc t in
      (* inlined_f (Top.may_put_in_top e) *)
      inlined_f { e with pexp_attributes = attrs }

class cformat = object (self)

  inherit P_top.top as super

  method! expr e = 
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (s, Some "fmt")) ->
        self#expr & parse [] e.pexp_loc e.pexp_attributes s
    | _ -> super#expr e
end
