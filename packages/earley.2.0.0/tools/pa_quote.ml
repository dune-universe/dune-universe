(* AST of types *)

open Earley_core

type btype =
  | Bool | Int | Char | String | Int32 | Int64 | Nativeint
  | Option of btype
  | List of btype
  | Location_t | Location_loc
  | Longident_t
  | Class_infos of btype
  | Include_infos of btype
  | Var of string
  | Loc of btype
  | Name of string
  | Prod of btype list

type typetype =
  | Syn of string * btype
  | Sum of string * (string * btype list) list
  | Rec of string option * string * (string * btype) list

type item =
  | Type of typetype list
  | Open of string

type ast = item list

(* Parser *)
let reserved = [ "bool"; "int32"; "int64"; "int"; "char"; "string"; "nativeint" ]

let parser lid = | s : ''[a-z][_a-z]*'' -> if List.mem s reserved then Earley.give_up (); s
let parser uid = | RE("[A-Z][_a-zA-Z0-9]*")
let parser arg = | '\'' - RE("[a-z]+")

let parser base_type p_auth =
  | t:(base_type false) "option"                  -> Option t
  | t:(base_type false) "list"                    -> List t
  | t:(base_type false) "loc"                     -> Loc t
  | t:(base_type false) "class_infos"             -> Class_infos t
  | t:(base_type false) "include_infos"           -> Include_infos t
  | t:(base_type false) ts:{'*' (base_type false)}+ when p_auth -> Prod (t :: ts)
  | "bool"                                        -> Bool
  | "int"                                         -> Int
  | "char"                                        -> Char
  | "string"                                      -> String
  | "int32"                                       -> Int32
  | "int64"                                       -> Int64
  | "nativeint"                                   -> Nativeint
  | "Location.t"                                  -> Location_t
  | "Location.loc"                                -> Location_loc
  | a:arg                                         -> Var a
  | n:lid                                         -> Name n
  | "Longident.t"                                 -> Longident_t
  | '(' (base_type true) ')'


let base_type = base_type true

let parser cdecl =
  | c:uid t:{"of" base_type}? ->
      begin
        let ts =
          match t with
          | None           -> []
          | Some (Prod ts) -> ts
          | Some t         -> [t]
        in (c, ts)
      end

let parser csdecl =
  | '|'? c:cdecl cs:{'|' cdecl}* -> c::cs

let parser field =
  | lid ':' base_type

let parser fields =
  | f:field fs:{';' field}* ';'? -> f::fs

let parser any_decl =
  | n:lid '=' b:base_type                                      -> Syn (n,b)
  | n:lid '=' cl:csdecl                                        -> Sum (n,cl)
  | a:arg? n:lid '=' _:{arg? base_type '='}? '{' fs:fields '}' -> Rec (a,n,fs)

let parser any_rec_decl =
  | "type" t:any_decl ts:{"and" any_decl}* -> Type (t::ts)
  | "open" n:uid                           -> Open n

let parser any_decls = | any_rec_decl*

let modname =
   match Array.length Sys.argv with
  | 1 -> fun id -> Printf.sprintf "(Lident \"%s\")" id
  | 2 -> let s = Filename.chop_extension (Filename.basename Sys.argv.(1)) in
	 assert (String.length s > 0);
	 let f = s.[0] in
	 let s =
           if f >= 'a' && f <= 'z' then (
	     let f' = Char.chr (Char.code f - Char.code 'a' + Char.code 'A') in
	     String.make 1 f' ^ String.sub s 1 (String.length s - 1))
           else s
         in
	 fun id -> Printf.sprintf "(Ldot(Lident \"%s\", \"%s\"))" s id
  | _ -> failwith "Wrong number of arguments..."

(* Printer *)
open Printf

let rec zip xs ys =
  match xs, ys with
  | []   , []    -> []
  | x::xs, y::ys -> (x,y) :: zip xs ys
  | _    , _     -> assert false

let rec print_btype ch = function
  | Bool            -> output_string ch "quote_bool"
  | Int             -> output_string ch "quote_int"
  | Char            -> output_string ch "quote_char"
  | String          -> output_string ch "quote_string"
  | Int32           -> output_string ch "quote_int32"
  | Int64           -> output_string ch "quote_int64"
  | Nativeint       -> output_string ch "quote_nativeint"
  | Option t        -> fprintf ch "(quote_option %a)" print_btype t
  | List t          -> fprintf ch "(quote_list %a)" print_btype t
  | Location_t      -> output_string ch "quote_location_t"
  | Location_loc    -> assert false (* never used *)
  | Longident_t     -> output_string ch "quote_longident"
  | Class_infos t   -> fprintf ch "(quote_class_infos %a)" print_btype t
  | Include_infos t -> fprintf ch "(quote_include_infos %a)" print_btype t
  | Var a           -> fprintf ch "quote_%s" a
  | Loc t           -> fprintf ch "(quote_loc %a)" print_btype t
  | Name n          -> fprintf ch "quote_%s" n
  | Prod lt         ->
      let len = List.length lt in
      let rec build_list pfx n =
        if n = 0 then [] else (pfx^(string_of_int n)) :: build_list pfx (n-1)
      in
      let xs = List.rev (build_list "x" len) in
      let cxs = "(" ^ (String.concat "," xs) ^ ")" in
      let txs = zip lt xs in
      fprintf ch "(fun e_loc _loc %s -> quote_tuple e_loc _loc [" cxs;
      let f (t, x) = fprintf ch "%a e_loc _loc %s;" print_btype t x in
      List.iter f txs;
      fprintf ch "])"

let print_type ch = function
  | Syn (n,t)    -> fprintf ch "quote_%s e_loc _loc x =  %a e_loc _loc x" n print_btype t
  | Sum (n,cl)   ->
      fprintf ch "quote_%s e_loc _loc x = match x with\n" n;
      let f (c, ts) =
        match ts with
        | []  -> fprintf ch "  | %s -> quote_const e_loc _loc %s []\n" c (modname c)
        | [t] -> fprintf ch "  | %s(x) -> quote_const e_loc _loc %s [%a e_loc _loc x]\n" c (modname c)
                   print_btype t
        | _   ->
            let len = List.length ts in
            let rec build_list pfx n =
              if n = 0 then []
              else (pfx^(string_of_int n)) :: build_list pfx (n-1)
            in
            let xs = List.rev (build_list "x" len) in
            let cxs = "(" ^ (String.concat "," xs) ^ ")" in
            fprintf ch "  | %s%s -> quote_const e_loc _loc %s [" c cxs (modname c);
            let txs = zip ts xs in
            let f (t,x) = Printf.fprintf ch " %a e_loc _loc %s;" print_btype t x in
            List.iter f txs;
            fprintf ch "]\n"
      in
      List.iter f cl
  | Rec (a,n,fl) ->
     let is_location = match fl with
	 (desc,_)::(loc,_)::_ when String.length desc > 5 && String.length loc > 4 &&
	     String.sub desc (String.length desc - 5) 5 = "_desc" &&
			     String.sub loc (String.length loc - 4) 4 = "_loc" ->
	  let name =
	      try String.sub desc 0 (String.length desc - 5) with _ -> assert false
	  in
	  Some (desc,loc,name)
       | [("txt",_); ("loc",_)] ->
	  Some("txt","loc","loc")
       | _ -> None
     in
     let prefix = match is_location with
	 None -> ""
       | Some (desc,loc,name) ->
	  Printf.sprintf "if is_antiquotation r.%s then try (Hashtbl.find anti_table r.%s) Quote_%s with Not_found -> failwith \"antiquotation not in a quotation\" else\n"
	    loc loc name
     in
     let suffix = match is_location with
	 None -> ""
       | Some (desc,loc,name) ->
	  Printf.sprintf "and %s_antiquotation _loc f %s= let _loc = make_antiquotation _loc in Hashtbl.add anti_table _loc f; %s _loc (dummy_%s)\n"
	    name
	    (if name = "loc" then "dummy_txt " else "")
	    (if name = "loc" then "loc_id" else "loc_"^name)
	    (if name = "loc" then "txt" else name)
      in
      (match a with
       | None   -> fprintf ch "quote_%s e_loc _loc r = %s" n prefix
       | Some a ->
           fprintf ch "quote_%s : 'a. (expression -> Location.t -> 'a -> expression) ->" n;
           fprintf ch " expression -> Location.t -> 'a %s -> expression =\n" n;
           fprintf ch "  fun quote_%s e_loc _loc r -> %s\n  " a prefix);
      fprintf ch "  quote_record e_loc _loc [\n";
      let f (l, t) =
        fprintf ch "   (%s, %a e_loc _loc r.%s) ;\n" (modname l) print_btype t l
      in
      List.iter f fl;
      fprintf ch "  ]\n%s" suffix

let print_types ch = function
  | []      -> assert false
  | [x]     -> Printf.fprintf ch "let %a\n" print_type x
  | x :: xs -> Printf.fprintf ch "let rec %a\n" print_type x;
               let f t = Printf.fprintf ch "and %a\n" print_type t in
               List.iter f xs

let rec print ch = function
  | []           -> ()
  | Open _ :: xs -> print ch xs
  | Type l :: xs -> print_types ch l; print ch xs

(* Main program *)
let _ =
  let ast =
    match Sys.argv with
    | [|_|]       -> Earley.parse_channel any_decls Blanks.ocaml_blank stdin
    | [|_;fname|] -> Earley.parse_file any_decls Blanks.ocaml_blank fname
    | _           -> failwith "Wrong number of arguments..."
  in
  print stdout ast
