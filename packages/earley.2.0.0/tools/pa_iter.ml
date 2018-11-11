open Earley_core

(* AST of types *)
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
let parser uid = | ''[A-Z][_a-zA-Z0-9]*''
let parser arg = | '\'' - RE("[a-z]+")

let parser base_type p_auth =
  | "bool"                                        -> Bool
  | "int32"                                       -> Int32
  | "int64"                                       -> Int64
  | "int"                                         -> Int
  | "char"                                        -> Char
  | "string"                                      -> String
  | "nativeint"                                   -> Nativeint
  | "Longident.t"                                 -> Longident_t
  | "Location.t"                                  -> Location_t
  | "Location.loc"                                -> Location_loc
  | t:(base_type false) "option"                  -> Option t
  | t:(base_type false) "list"                    -> List t
  | t:(base_type false) "loc"                     -> Loc t
  | t:(base_type false) "class_infos"             -> Class_infos t
  | t:(base_type false) "include_infos"           -> Include_infos t
  | t:(base_type false) ts:{'*' (base_type false)}+ when p_auth -> Prod (t :: ts)
  | a:arg                                         -> Var a
  | n:lid                                         -> Name n
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

(* Printer *)
let rec print_btype ch = function
  | Bool | Int | Char | String | Int32 | Int64
  | Nativeint       -> Printf.fprintf ch "(fun _ -> ())"
  | Option t        -> Printf.fprintf ch "(iter_option %a)" print_btype t
  | List t          -> Printf.fprintf ch "(iter_list %a)" print_btype t
  | Location_t      -> Printf.fprintf ch "(fun _ -> ())"
  | Location_loc    -> assert false (* never used *)
  | Longident_t     -> Printf.fprintf ch "iter_longident"
  | Class_infos t   -> Printf.fprintf ch "(iter_class_infos %a)" print_btype t
  | Include_infos t -> Printf.fprintf ch "(iter_include_infos %a)" print_btype t
  | Var a           -> Printf.fprintf ch "iter_%s" a
  | Loc t           -> Printf.fprintf ch "(iter_loc %a)" print_btype t
  | Name n          -> Printf.fprintf ch "iter_%s" n
  | Prod lt         ->
      let len = List.length lt in
      let rec build_list pfx n =
        if n = 0 then [] else (pfx^(string_of_int n)) :: build_list pfx (n-1)
      in
      let xs = List.rev (build_list "x" len) in
      let cxs = "(" ^ (String.concat "," xs) ^ ")" in
      let rec zip2 xs ts =
        match xs, ts with
        | []   ,  []    -> []
        | x::xs, t::ts -> (x,t) :: zip2 xs ts
        | _ -> assert false
      in
      let data = zip2 xs lt in
      Printf.fprintf ch "(fun %s -> ()" cxs;
      let f (x, t) =
        Printf.fprintf ch " ; (%a %s)" print_btype t x
      in
      List.iter f data;
      Printf.fprintf ch ")"

let print_type ch = function
  | Syn (n,t)    -> Printf.fprintf ch "iter_%s c1 = %a c1" n print_btype t
  | Sum (n,cl)   ->
      Printf.fprintf ch "iter_%s c1 =\n  match c1 with\n" n;
      let f (c, ts) =
        match ts with
        | []     -> Printf.fprintf ch "  | %s -> ()\n" c
        | [t]    -> Printf.fprintf ch "  | %s(x) -> %a x\n" c      print_btype t
        | ts     ->
            let len = List.length ts in
            let rec build_list pfx n =
              if n = 0 then []
              else (pfx^(string_of_int n)) :: build_list pfx (n-1)
            in
            let xs = List.rev (build_list "x" len) in
            let cxs = "(" ^ (String.concat "," xs) ^ ")" in
            let rec zip2 xs ts =
              match xs, ts with
              | []   , []    -> []
              | x::xs, t::ts -> (x,t) :: zip2 xs ts
              | _ -> assert false
            in
            let data = zip2 xs ts in
            Printf.fprintf ch "  | %s%s -> ()" c cxs;
            let f (x, t) =
              Printf.fprintf ch " ; (%a %s)" print_btype t x
            in
            List.iter f data;
      in
      List.iter f cl
  | Rec (a,n,fl) ->
      (match a with
        | None   -> Printf.fprintf ch "iter_%s = fun r1 -> ()" n
        | Some a -> Printf.fprintf ch "iter_%s : 'a. ('a -> unit) -> 'a %s -> unit = fun iter_%s r1 -> ()"
	                    n n a);
      let f (l,t) =
        Printf.fprintf ch " ; %a r1.%s " print_btype t l
      in
      List.iter f fl

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
