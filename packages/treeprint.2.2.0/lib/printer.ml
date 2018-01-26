open Spotlib.Spot

module Token = struct
  (** Display machine with Format like capability *)

  type t = 
    | String of string
    | Box of int * t  (** "@[<n>...@]" *)
    | VBox of int * t (** "@[<vn>...@]" *)
    | Cut             (** "@," (known also as Good Break) *)
    | Space           (** "@ " *)
    | Flush           (** "@." *)
    | Seq of t list
    | NOP
  
  open Format

  (* self tokenize *)
  let rec dump_self = function
    | String s -> String ("String \"" ^ String.escaped s ^ "\"")
    | Box (n, t) -> 
        Box (2,
             Seq [ String "Box ("
                 ; Cut
                 ; String (string_of_int n ^ ",")
                 ; Space
                 ; dump_self t
                 ; String ")"
                 ])

    | VBox (n, t) -> 
        Box (2,
             Seq [ String "VBox ("
                 ; Cut
                 ; String (string_of_int n ^ ",")
                 ; Space
                 ; dump_self t
                 ; String ")"
                 ])
    | Cut -> String "Cut"
    | Space -> String "Space"
    | Flush -> String "Flush"
    | Seq ts ->
        Box (2,
             Seq ( [ String "Seq ["
                   ; Cut
                   ; Seq List.(intersperse (Seq [String ";"; Space]) (map dump_self ts))
                   ; Cut
                   ; String "]" ]))
    | NOP -> String "NOP"

  let rec format ppf = function
    | String s -> string ppf s
    | Box (n, tk) -> 
        box ppf n; format ppf tk; close_box ppf ()
    | VBox (n ,tk) -> 
        vbox ppf n; format ppf tk; close_box ppf ()
    | Cut -> cut ppf
    | Space -> space ppf 
    | Flush -> flush ppf; newline ppf
    | Seq tks -> List.iter (format ppf) tks
    | NOP -> ()
  
  let dump ppf t = format ppf & dump_self t
    
  open Buffer
  let rec buffer buf = function
    | String s -> add_string buf s
    | Box (_, tk) | VBox (_ ,tk) -> buffer buf tk
    | Seq tks -> List.iter (buffer buf) tks
    | Cut | NOP -> ()
    | Space -> add_char buf ' '
    | Flush -> add_char buf '\n' (* CR jfuruse: probably add too many? *)
  
  let show token = 
    let buf = Buffer.create 100 in 
    buffer buf token; 
    Buffer.contents buf
end

type token = Token.t
open Token

(** Primitive operators *)

type assoc = Left | Right | Noassoc
type level = float
type 'a t = assoc -> level -> 'a (* monadic *)
type 'a t_ = 'a t

include Monad.Make(struct 
  type 'a t = 'a t_
  let bind at f = fun a l -> f (at a l) a l
  let return a = fun _ _ -> a
end)

type ppr = token t

let box : int -> ppr -> ppr = 
  fun offset t a l -> Box (offset, t a l)

let vbox : int -> ppr -> ppr = 
  fun offset t a l -> VBox (offset, t a l)

let do_Seq xs =
  Seq (List.concat_map (function 
    | Seq ys -> ys
    | y -> [y]) xs)

let (++) : ppr -> ppr -> ppr = 
  fun p1 p2 a l -> do_Seq [p1 a l; p2 a l]

let cut : ppr = 
  fun _out_ops _out_lev -> Cut

let space : ppr = 
  fun _out_pos _out_lev -> Space

let flush : ppr =
  fun _out_pos _out_lev -> Flush

let seq : ppr list -> ppr
  = fun ps a l -> do_Seq (List.map (fun p -> p a l) ps)

let string : string -> ppr
  = fun s _out_pos _out_lev -> String s

let nop : ppr = fun _out_pos _out_lev -> NOP

let left    : 'a t -> 'a t          = fun p _a l -> p Left    l
let right   : 'a t -> 'a t          = fun p _a l -> p Right   l
let noassoc : 'a t -> 'a t          = fun p _a l -> p Noassoc l
let level : level -> 'a t -> 'a t = fun l p a _l -> p a l
let reset   : 'a t -> 'a t = fun t -> noassoc (level 0.0 t)

let check_against_current_level : level -> [`Weaker | `Stronger | `Same] t = fun lev _out_pos out_lev ->
  match compare out_lev lev with
  | 1 -> `Weaker
  | -1 -> `Stronger
  | 0 -> `Same
  | _ -> assert false

let need_paren : assoc -> level -> bool t = fun assoc lev out_pos out_lev ->
  match compare out_lev lev with
  | 1 -> true
  | -1 -> false
  | 0 ->
      begin match out_pos, assoc with
      | Left, Left -> false
      | Right, Right -> false
      | _ -> true
      end
  | _ -> assert false

type parens = string * string

let parens = "(", ")"
  
let parenbox : ?parens:parens -> assoc -> level -> ppr -> ppr = fun ?(parens=parens) assoc lev t ->
  need_paren assoc lev >>= function
    | true  -> box 1 & string (fst parens) ++ reset t ++ string (snd parens)
    | false -> t

(** Common utilities *)

let binop : ?parens:parens -> assoc -> level -> op:ppr -> ppr -> ppr -> ppr =
  fun ?parens assoc lev ~op:sep l r ->
    parenbox ?parens assoc lev (level lev (left l ++ sep ++ right r))

let list : ?parens:parens -> level -> ppr -> ppr list -> ppr = 
  fun ?parens lev sep f_elems ->
    parenbox ?parens Noassoc lev (level lev (seq (List.intersperse sep f_elems)))

let prefix : ?parens:parens -> level -> op:ppr -> ppr -> ppr =
  fun ?parens lev ~op:pref t -> 
    let t = parenbox ?parens Right lev (pref ++ level lev (right t)) in
    (* [uminus (uminus 1)] should not be printed out neither "- - 1" or "--1",
       but "- -1" *)    
    check_against_current_level lev >>= function
      | `Same -> space ++ t 
      | `Weaker | `Stronger -> t

let postfix : ?parens:parens -> level -> op:ppr -> ppr -> ppr =
  fun ?parens lev ~op:postf t -> 
    let t = parenbox ?parens Left lev (level lev (left t) ++ postf) in
    (* [uminus (uminus 1)] should not be printed out neither "- - 1" or "--1",
       but "- -1" *)    
    check_against_current_level lev >>= function
      | `Same -> t ++ space
      | `Weaker | `Stronger -> t

let parens left right ppr = string left ++ level (-1.0) ppr ++ string right

module OCaml = struct
  let mbinop assoc lev sep = binop assoc lev ~op:(space ++ string sep ++ space)

(*
%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
*)

  let sequence = list 0.25 (string ";" ++ space)

(*
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
*)

  let if_then_else e1 e2 e3 = list 0.5 space [string "if"; reset e1; string "then"; e2; string "else"; e3]
  let if_then e1 e2 = list 0.5 space [string "if"; reset e1; string "then"; e2]

(*
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
*)

  let ty_as = mbinop Noassoc 0.6 "as"

(*
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
*)

  let tuple = list 0.8 (string "," ++ space (* CR jfuruse: should be break *))

(*
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
*)

  let ( ^-> ) = mbinop Right 0.9 "->"

(*
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
*)

  let (+) = mbinop Left 1.0 "+"
  let (-) = mbinop Left 1.0 "-"
  let ( * ) = mbinop Left 2.0 "*"

  let ty_tuple = list 2.0 (space ++ string "* ")
(*
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
*)

  let mprefix lev op = prefix lev ~op:(string op)
  let uminus = mprefix 5.0 "-"
  
(*
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT
*)

  let app = binop Left 100.0 ~op:space (* CR jfuruse: contiguous spaces must be contracted *)
  
end

(** Drivers *)
let format ?(assoc=Noassoc) ?(level=0.0) ppr ppf v = Token.format ppf (ppr v assoc level)
let buffer ppr buf ?(assoc=Noassoc) ?(level=0.0) v = Token.buffer buf (ppr v assoc level)
let show ppr ?(assoc=Noassoc) ?(level=0.0) v = Token.show (ppr v assoc level)

module MakeDrivers(M : sig 
  type t
  val ppr : t -> ppr
end) = struct
  open M
  let format ppf v = Token.format ppf (ppr v Noassoc 0.0)
  let dump ppf v = Token.dump ppf (ppr v Noassoc 0.0)
  let buffer buf ?(assoc=Noassoc) ?(level=0.0) v = Token.buffer buf (ppr v assoc level)
  let show ?(assoc=Noassoc) ?(level=0.0) v = Token.show (ppr v assoc level)
end

open OCaml

module Test = struct
  let num z = string (string_of_int z)
  let id x = string x
  let int = id "int"
  let alpha = id "'a"

  let test t answer = 
    let str = Token.show (t Noassoc 0.0) in
    Format.eprintf "%s =?= %s@." str answer;
    if str <> answer then failwith "FAILED";
    Format.eprintf "%a@.@." Token.format  (t Noassoc 0.0);
    Format.eprintf "%a@.@." Token.dump  (t Noassoc 0.0)

  let test () =
    test (num 0) "0";
    test (num 0 + num 0) "0 + 0";
    test (num 0 + num 0 * num 0) "0 + 0 * 0";
    test (num 0 * (num 0 + num 0)) "0 * (0 + 0)";
    test ((num 1 + num 2) * num 3) "(1 + 2) * 3";
    test (num 1 + (num 2 + num 3)) "1 + (2 + 3)"; (* It is as same as 1 + 2 + 3 but just semantically *)
    test (num 1 - (num 2 - num 3)) "1 - (2 - 3)";
    test (num 1 - num 2 - num 3) "1 - 2 - 3";
    test (uminus (num 1)) "-1";
    test (uminus (num 1 + num 2 + num 3))   "-(1 + 2 + 3)";
    test (num 1 + uminus (num 1)) "1 + -1";
    test (uminus (uminus (num 1))) "- -1";
    test (int ^-> int ^-> int) "int -> int -> int";
    test ((int ^-> int) ^-> int) "(int -> int) -> int";
    test (ty_as (int ^-> int ^-> int) alpha) "int -> int -> int as 'a";
    test ((ty_as (int ^-> int) alpha) ^-> int) "(int -> int as 'a) -> int";
    test (tuple [num 1; num 2; num 3]) "1, 2, 3";
    test (tuple [num 1; tuple [num 2; num 3]; num 4]) "1, (2, 3), 4";
    test (app (app (id "x") (id "y")) (id "z")) "x y z";
    test (app (id "x") (app (id "y") (id "z"))) "x (y z)";
    test (app (id "x") (num 1 * num 2)) "x (1 * 2)";
    test (sequence [ num 1 + num 2; num 1 + num 2; num 1 + num 2 ]) "1 + 2; 1 + 2; 1 + 2";
    test (if_then_else (num 1 + num 2) (num 1 + num 2) (num 1 + num 2)) "if 1 + 2 then 1 + 2 else 1 + 2";
    test (if_then_else (num 1 + num 2) (num 1 + num 2) (num 1) + num 2) "(if 1 + 2 then 1 + 2 else 1) + 2";
    test (app (if_then_else (num 1 + num 2) (num 1 + num 2) (num 1)) (num 2)) "(if 1 + 2 then 1 + 2 else 1) 2";
    test (app (id "f") (if_then_else (num 1 + num 2) (num 1 + num 2) (num 1 + num 2))) "f (if 1 + 2 then 1 + 2 else 1 + 2)";
    test (if_then_else 
            (sequence [ num 1 + num 2; num 1 + num 2 ])
            (sequence [ num 1 + num 2; num 1 + num 2 ])
            (sequence [ num 1 + num 2; num 1 + num 2 ])) "if 1 + 2; 1 + 2 then (1 + 2; 1 + 2) else (1 + 2; 1 + 2)";
    test (sequence [if_then_else
                 (sequence [ num 1 + num 2; num 1 + num 2 ])
                 (sequence [ num 1 + num 2; num 1 + num 2 ])
                 (sequence [ num 1 + num 2; num 1 + num 2 ]);
               num 1 + num 2 ]) "if 1 + 2; 1 + 2 then (1 + 2; 1 + 2) else (1 + 2; 1 + 2); 1 + 2";
    test (if_then
            (sequence [ num 1 + num 2; num 1 + num 2 ])
            (sequence [ num 1 + num 2; num 1 + num 2 ])) "if 1 + 2; 1 + 2 then (1 + 2; 1 + 2)";
    test (sequence [if_then 
                 (sequence [ num 1 + num 2; num 1 + num 2 ])
                 (sequence [ num 1 + num 2; num 1 + num 2 ]);
               num 1 + num 2 ]) "if 1 + 2; 1 + 2 then (1 + 2; 1 + 2); 1 + 2";
    prerr_endline "done"

end

