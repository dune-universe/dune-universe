open Ppxx.Helper
open Ppxx.Utils
open Printf
open Lexrex

(* methods for group access *) 
let group_methods loc num_of_groups named_groups =
  let num_methods n =
    let num n =
      Cf.method_concrete ~loc (at ~loc & sprintf "_%d" n)
        [%expr
            self#_unsafe_group
            [%e Exp.int n]
        ]
    in
    let num_opt n =
      Cf.method_concrete ~loc (at ~loc & sprintf "_%dopt" n)
        [%expr
            self#_unsafe_group_opt 
            [%e Exp.int n ]
        ]
    in
    let rec f n =
      if n < 0 then []
      else num n :: num_opt n :: f (n-1)
    in
    f n
  in
  let named_methods named_groups =
    let named name pos =
      let mname =
	match name.[0] with
	| 'a'..'z' -> name
	| _ -> "_" ^ name
      in
      Cf.method_concrete ~loc (at ~loc mname)
        [%expr
            self#_unsafe_group 
            [%e Exp.int pos ]
        ]
    in
    List.map (fun (name,pos) -> named name pos) named_groups
  in
  num_methods num_of_groups @ named_methods named_groups

  (* syntax encoding for named_groups *)
let rec named_groups loc = function
  | [] -> [%expr [] ]
  | (n,pos)::gs ->
      [%expr
        ([%e Exp.string n], [%e Exp.int pos])   
        :: [%e named_groups loc gs ] 
      ]
;;

let in_implem = ref false
let set_in_implem () = in_implem := true

let build_group_binder loc typ = 
  let group_methods = 
    group_methods loc typ.num_of_groups typ.named_groups 
  in
  let named_groups = named_groups loc typ.named_groups in
  [%expr
      fun ~left ~right ~last groups ->
        [%e Exp.object_ 
            & Cstr.mk 
                (Pat.var' "self") (* (Pat.any ()) *)
                (Cf.inherit_ 
                   (Cl.apply (Cl.constr (lid ~loc & "OrakudaRegexpInternal.group") [])
                      [ Nolabel, named_groups
                      ; Nolabel, Exp.var "groups"
                      ; Labelled "left", Exp.var "left"
                      ; Labelled "right", Exp.var "right"
                      ; Labelled "last", Exp.var "last"
                      ])
                   None
                 :: group_methods )
        ]
  ]
(*
          object (self)
            $group_methods$
         inherit Ppx_orakuda.Regexp.Internal_use_only.group $named_groups$ groups ~left ~right ~last
       end
*)


let build_rex loc (* s *) tokens typ flags =
  let group_binder = build_group_binder loc typ in
  (* let str = ExStr (loc, s) in *)
  let str = Lexrex.string_of_tokens typ tokens in
  let rex = 
    [%expr
      (* This enforces linking with Ppx_orakuda library *)
      OrakudaRegexpInternal.create 
        [%e Exp.string str]
        ~flags:[%e flags ]
        [%e group_binder ]
    ]
  in
  (* Top.may_put_in_top rex *)
  rex
;;

(* CR jfuruse; BUG: This is wrong.
   It cannot parse correctly \\/
*)
let rec find_non_escaped_slash from s =
  let pos = String.rindex_from s from '/' in
  if pos = 0 || s.[pos-1] <> '\\' then pos
  else find_non_escaped_slash (pos - 1) s
;;

let split_by_non_escaped_slash s =
  let from = String.length s - 1 in
  let rec split st from =
    if from < 0 then "" :: st
    else
      try
        let pos = find_non_escaped_slash from s in
        split (String.sub s (pos + 1) (from - pos) :: st) (pos-1)
      with
      | Not_found -> 
          String.sub s 0 (from+1) :: st
  in
  split [] from
;;

let parse_rex_quotation loc s =
  with_loc loc & fun () ->
(* TODO
    let loc = Loc.join (Loc.move `start q.q_shift loc) in
*)
  let _rex, tokens, flags = Lexrex.from_string s
  in
  let parse_flag = function
    | 'i' -> [%expr `CASELESS]
    | 'm' -> [%expr `MULTLINE]
    | 's' -> [%expr `DOTALL]
    | 'x' -> [%expr `EXTENDED]
    | 'U' -> [%expr `LAZY]
    | '8' -> [%expr `UTF8]
    | c -> raise (Stream.Error 
      	       (Printf.sprintf "unknown pcre match flag %C" c))
  in
  let flags = 
    let len = String.length flags in
    let rec iter acc pos =
      if pos = len then acc
      else iter (parse_flag flags.[pos] :: acc) (pos + 1)
    in
    List.fold_left (fun acc sw ->
      [%expr [%e sw] :: [%e acc]]) [%expr [] ] (iter [] 0)
  in
  let typ = Lexrex.type_regexp tokens in
  build_rex loc (*rex*) tokens typ flags
;;

let parse_rex_replace_quotation loc s =
  with_loc loc & fun () ->
(* TODO
    let loc = Loc.join (Loc.move `start q.q_shift loc) in
*)
  let _rex, tokens, replace_ = Lexrex.replace_from_string s in
  let replace, flags_ = P_format.parse ['/'] loc replace_ in
  let flags = match flags_ with
    | None -> ""
    | Some flags_ -> String.sub flags_ 1 (String.length flags_ - 1)
  in
  let replace_global = ref false in
  let parse_flag = function
    | 'i' -> Some [%expr `CASELESS]
    | 'm' -> Some [%expr `MULTLINE]
    | 's' -> Some [%expr `DOTALL]
    | 'x' -> Some [%expr `EXTENDED]
    | 'U' -> Some [%expr `LAZY]
    | '8' -> Some [%expr `UTF8]
    | 'g' -> replace_global := true; None
    | c -> raise (Stream.Error 
      	       (Printf.sprintf "unknown pcre replace flag %C" c))
  in
  let flags =
    let len = String.length flags in
    let rec iter acc pos =
      if pos = len then acc
      else 
        let acc = 
          match parse_flag flags.[pos] with
          | None -> acc
          | Some f -> f :: acc
        in
        iter acc (pos + 1)
    in
    List.fold_left (fun acc sw ->
      [%expr [%e sw] :: [%e acc]]) [%expr []] (iter [] 0)
  in
  let typ = Lexrex.type_regexp tokens in
  let rex = build_rex loc (*rex*) tokens typ flags in
  (* CR jfuruse: we can unify these to substitute_substrings(_first) *)
  match replace with
  | `Const replace ->
      if !replace_global then
        [%expr
            OrakudaRegexpInternal.replace 
              [%e rex]
              ~templ:[%e replace]
        ]
      else
        [%expr
            OrakudaRegexpInternal.replace_first
              [%e rex]
              ~templ:[%e replace]
        ]
  | `Fun ([], f) -> 
      let e = f [%expr Printf.sprintf] in
      if !replace_global then 
        [%expr
          OrakudaRegexpInternal.substitute_substrings 
            (fun __rex_group -> [%e e])
            [%e rex]
        ]
      else
        [%expr
          OrakudaRegexpInternal.substitute_substrings_first
            (fun __rex_group -> [%e e])
            [%e rex]
        ]
  | `Fun (_abss, _) -> 
      raise (Stream.Error 
      	  (Printf.sprintf "non closed template %S" replace_))

class regexp = object (self)

  inherit P_command.command as super

  method! expr e = 
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (s, Some "m")) ->
        self#expr & parse_rex_quotation e.pexp_loc (* e.pexp_attributes *) s
    | Pexp_constant (Pconst_string (s, Some "s")) ->
(* TODO: attrs *) 
        self#expr & parse_rex_replace_quotation e.pexp_loc (* e.pexp_attributes *) s
    | _ -> super#expr e
end
