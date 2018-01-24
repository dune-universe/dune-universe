{
open Lexing

type ctype =
    | Cd (* [d], [i], [n], [l], [L], or [N]: convert an integer
	    argument to signed decimal. *)
    | Cu (* [u]: convert an integer argument to unsigned decimal. *)
    | Cx (* [x]: convert an integer argument to unsigned hexadecimal, 
	   using lowercase letters. *)
    | CX (* [X]: convert an integer argument to unsigned hexadecimal, 
	    using uppercase letters. *)
    | Co (* [o]: convert an integer argument to unsigned octal. *)
    | Cs (* [s]: insert a string argument. *)
    | CS (* [S]: insert a string argument in Caml syntax (double
	    quotes, escapes). *)
    | Cc (* [c]: insert a character argument. *)
    | CC (* [C]: insert a character argument in Caml syntax (single
	    quotes, escapes). *)
    | Cf (* [f]: convert a floating-point argument to decimal notation,
	    in the style [dddd.ddd]. *)
    | CF (* [F]: convert a floating-point argument to Caml syntax ([dddd.]
	    or [dddd.ddd] or [d.ddd e+-dd]). *)
    | Ce | CE (* [e] or [E]: convert a floating-point argument to decimal
	    notation, in the style [d.ddd e+-dd] (mantissa and
	    exponent). *)
    | Cg | CG (* [g] or [G]: convert a floating-point argument to decimal
	    notation, in style [f] or [e], [E] (whichever is more
	    compact). *)
    | CB (* [B]: convert a boolean argument to the string [true] or
	    [false] *)
    | Cld | Clu | Clx | ClX | Clo 
          (* [ld], [li], [lu], [lx], [lX], [lo]: convert an [int32]
	     argument to the format specified by the second letter
	     (decimal, hexadecimal, etc). *)
    | Cnd | Cnu | Cnx | CnX | Cno
	  (* [nd], [ni], [nu], [nx], [nX], [no]: convert a
	     [nativeint] argument to the format specified by the
	     second letter. *)
    | CLd | CLu | CLx | CLX | CLo
	  (* [Ld], [Li], [Lu], [Lx], [LX], [Lo]: convert an
	     [int64] argument to the format specified by the
	     second letter. *)
    | Ca (* [a]: user-defined printer. Takes two arguments and applies the
	    first one to [outchan] (the current output channel) and to the
	    second argument. The first argument must therefore have type
	    [out_channel -> 'b -> unit] and the second ['b].
	    The output produced by the function is inserted in the output of
	    [fprintf] at the current point. *)
    | Ct (* [t]: same as [%a], but takes only one argument (with type
	    [out_channel -> unit]) and apply it to [outchan]. *)
    | Cformat of t
	(* [\{ fmt %\}]: convert a format string argument. The argument must
	   have the same type as the internal format string [fmt]. *)
    | Cformat_subst of t
	(* [( fmt %)]: format string substitution. Takes a format string
	   argument and substitutes it to the internal format string [fmt]
	   to print following arguments. The argument must have the same
	   type as [fmt]. *)
    | Cflush (* [!]: take no argument and flush the output. *)
    | Cpercent (* [%]: take no argument and output one [%] character. *)

and flag =
    | Fminus
    | Fzero
    | Fplus
    | Fspace
    | Fsharp

and width_precision =
    | WPint of int
    | WPstar

and inlined_arg = 
    | Arg_expr of string
    | Arg_var of string
    | Arg_rex_ref of char
        
and conversion = {
  flags : flag list;
  width : width_precision option;
  precision : width_precision option option; 
    (** Some (Some _) : ".1", ".*"
	Some None : "."
	None : "" *)
  ctype : ctype;
  inlined_arg : (inlined_arg * int (* pos *)) option;
}

and token =
    | String of string (* invariant: no $ % *)
    | Char of char (* invariant: no $ % *)
    | Conv of conversion
    | Escaped of char

and t = token list

let flag_to_char = function
  | Fminus -> '-'
  | Fzero -> '0'
  | Fplus -> '+'
  | Fspace -> ' '
  | Fsharp -> '#'

let width_precision_to_string = function
  | WPint n -> string_of_int n
  | WPstar -> "*"

let option_to_string f = function
  | Some v -> f v
  | None -> ""

(*
   [% \[${arg_inlined}\] \[flags\] \[width\] \[.precision\] type]

  $var => %${var}s
  ${exp} => %${exp}s 
*)

let rec ctype_to_string = function
  | Cd -> "d"
  | Cu -> "u"
  | Cx -> "x"
  | CX -> "X"
  | Co -> "o"
  | Cs -> "s"
  | CS -> "S"
  | Cc -> "c"
  | CC -> "C"
  | Cf -> "f"
  | CF -> "F"
  | Ce -> "e"
  | CE -> "E"
  | Cg -> "g"
  | CG -> "G"
  | CB -> "B"
  | Cld -> "ld"
  | Clu -> "lu"
  | Clx -> "lx"
  | ClX -> "lX"
  | Clo -> "lo"
  | Cnd -> "nd"
  | Cnu -> "nu"
  | Cnx -> "nx"
  | CnX -> "nX"
  | Cno -> "no"
  | CLd -> "Ld"
  | CLu -> "Lu"
  | CLx -> "Lx"
  | CLX -> "LX"
  | CLo -> "Lo"
  | Ca -> "a"
  | Ct -> "t"
  | Cformat t -> Printf.sprintf "{%s%%}" (to_string t)
  | Cformat_subst t -> Printf.sprintf "(%s%%)" (to_string t)
  | Cflush -> "%!"
  | Cpercent -> "%%"

and conversion_to_string conv =
  let buf = Buffer.create 2 in
  Buffer.add_char buf '%';
  List.iter 
    (fun f -> Buffer.add_char buf (flag_to_char f))
    conv.flags;
  (match conv.inlined_arg with
  | None -> ()
  | Some (Arg_expr s, _pos) -> 
      Buffer.add_string buf "${";
      Buffer.add_string buf s;
      Buffer.add_string buf "}"
  | Some (Arg_var s, _pos) -> 
      Buffer.add_string buf "$";
      Buffer.add_string buf s;
      Buffer.add_string buf ""
  | Some (Arg_rex_ref char, _pos) -> 
      Buffer.add_string buf "$";
      Buffer.add_char buf char;
      Buffer.add_string buf "");
  Buffer.add_string buf 
    (option_to_string width_precision_to_string conv.width);
  (match conv.precision with
  | None -> ()
  | Some p ->
      Buffer.add_char buf '.';
      Buffer.add_string buf (option_to_string width_precision_to_string p));
  Buffer.add_string buf (ctype_to_string conv.ctype);
  Buffer.contents buf
    
and compile_conversion conv =
  let tokens = 
    let buf = Buffer.create 2 in
    let token_flags = 
      Buffer.add_char buf '%';
      List.iter 
        (fun f -> Buffer.add_char buf (flag_to_char f))
        conv.flags;
      `String (Buffer.contents buf)
    in
    let token_width =
      match conv.width with
      | None -> `String ""
      | Some WPint n -> `String (string_of_int n)
      | Some WPstar -> `Star
    in
    let tokens_precision =
      match conv.precision with
      | None -> [`String ""]
      | Some None -> [`String "."]
      | Some (Some (WPint n)) -> [`String ("." ^ string_of_int n)]
      | Some (Some WPstar) -> [`String "."; `Star]
    in
    let token_type = `String (ctype_to_string conv.ctype) in
    token_flags :: token_width :: tokens_precision @ [token_type]
  in
  let rec simplif string tokens = function
    | `String s :: xs -> simplif (string ^ s) tokens xs
    | `Star :: xs ->
	if string = "" then simplif "" (`Star :: tokens) xs
	else simplif "" (`Star :: `String string :: tokens) xs
    | [] -> 
	let tokens = 
	  if string = "" then tokens
	  else `String string :: tokens
	in
	List.rev tokens
  in
  simplif "" [] tokens

and token_to_string = function
  | String s -> s
  | Escaped '"' -> "\\\""
  | Escaped '\\' -> "\\\\"
  | Char char | Escaped char -> String.make 1 char
  | Conv conv -> conversion_to_string conv

and to_string tokens =
  (* CR jfuruse: Char tends to continue! Inefficient! Use Buffer! *)
  String.concat "" (List.map token_to_string tokens)

exception Error of int * int * string

let errorf lexbuf fmt =
  Printf.ksprintf (fun s -> raise (Error (lexeme_start lexbuf, lexeme_end lexbuf, s)))
    fmt

let errorf_at start end_ fmt =
  Printf.ksprintf (fun s -> raise (Error (start, end_, s)))
    fmt

let check_conversion lexbuf conv =
  let no_inlined_arg () =
    if conv.inlined_arg <> None then
      errorf lexbuf "This conversion cannot take an inlined argument"
  in
  let no_flag_width_precision () =
    if conv.flags <> [] then 
      errorf lexbuf "This conversion cannot take flags";
    if conv.width <> None then
      errorf lexbuf "This conversion cannot take a width";
    if conv.precision <> None then
      errorf lexbuf "This conversion cannot take a precision";
  in
  (* just no perfect quick checks *)
  begin match conv.ctype with
  | Cflush | Cpercent | Ca ->
      no_inlined_arg ();
      no_flag_width_precision ()
  | Ct ->
      no_flag_width_precision ()
  | _ -> ()
  end;
  conv
;;
}

rule ctype stopat = parse
  | ( "d" | "i" | "n" | "l" | "L" | "N" ) { Cd }
  | "u" { Cu }
  | "x" { Cx }
  | "X" { CX }
  | "o" { Co }
  | "s" { Cs }
  | "S" { CS }
  | "c" { Cc }
  | "C" { CC }
  | "f" { Cf }
  | "F" { CF }
  | "e" { Ce }
  | "E" { CE }
  | "g" { Cg }
  | "G" { CG }
  | "B" { CB }
  | "b" { CB }
  | ( "ld" | "li" ) { Cld }
  | "lu" { Clu }
  | "lx" { Clx }
  | "lX" { ClX }
  | "lo" { Clo }
  | ( "nd" | "ni" ) { Cnd }
  | "nu" { Cnu }
  | "nx" { Cnx }
  | "nX" { CnX }
  | "no" { Cno }
  | ( "Ld" | "Li" ) { CLd }
  | "Lu" { CLu }
  | "Lx" { CLx }
  | "LX" { CLX }
  | "Lo" { CLo }
  | "a" { Ca }
  | "t" { Ct }
  | "{" { 
      match format stopat [] lexbuf with
      | fmt, None -> 
          enclose '}' lexbuf;
          Cformat fmt 
      | _, Some (pos, c) ->
          errorf_at pos (pos+1) "illegal format type %C" c
    }
  | "(" { 
      match format stopat [] lexbuf with
      | fmt, None -> 
          enclose ')' lexbuf;
          Cformat_subst fmt
      | _, Some (pos, c) -> 
          errorf_at pos (pos+1) "illegal format type %C" c
    }
  | "!" { Cflush }
  | "%" { Cpercent }
  | _ as c { errorf lexbuf "illegal format type %C" c }

and enclose str = parse
  | "%" ([ ')' '}' ] as s) {
      if s = str then ()
      else errorf lexbuf "wrong format closing (%%%c expected)" str
    }

and flag = parse
  | "-" { Some Fminus }
  | "0" { Some Fzero }
  | "+" { Some Fplus }
  | " " { Some Fspace }
  | "#" { Some Fsharp }
  | "" { None }
      
and width_precision = parse
  | (['0'-'9']+ as num) { Some (WPint (int_of_string num)) }
  | "*" { Some WPstar }
  | "" { None }

and precision = parse
    | "." { Some (width_precision lexbuf) }
    | "" { None }

and dollared = parse
  | (['a' - 'z'  '_'] ['A' - 'Z' 'a' - 'z' '0' - '9' '_' '\'' ]* as var) 
      { 
	let pos = lexeme_start lexbuf in
	Arg_var var, pos
      }
  | ( ['0'-'9' '`' '\'' '&' '+' ] as var) 
      {
	let pos = lexeme_start lexbuf in
	Arg_rex_ref var, pos
      }
  | "{"
      { let pos = lexeme_start lexbuf + 1 in
        let exp = exp "" lexbuf in
	Arg_expr exp, pos
      }
  | "" {
      errorf lexbuf "illegal $-expression"
    }

and inlined_arg = parse
  | "$" { Some (dollared lexbuf) }
  | "" { None }

and format specials st = parse
  | "%" { 
      let inlined_arg = inlined_arg lexbuf in
      let flags = 
	let rec get_flag () = 
	  match flag lexbuf with
	  | None -> []
	  | Some f -> f :: get_flag ()
	in
	get_flag ()
      in
      let width = width_precision lexbuf in
      let precision = precision lexbuf in
      let ctype = ctype specials lexbuf in
      format specials
	(Conv (check_conversion lexbuf
		  { flags = flags;
		    width = width;
		    precision = precision;
		    ctype = ctype;
		    inlined_arg = inlined_arg }) :: st) lexbuf
    }
	
  | ([^ '%' '$' '\\'] as c) { 
      if List.mem c specials then
        (* Special char. We must stop here *)
        List.rev st, Some (lexeme_start lexbuf (* the char is already loaded! *), c)
      else
        format specials (Char c :: st) lexbuf 
    }

  | "\\" (['\\' 'n' 't' 'r' 'b']  as char) {
      format specials 
        (Escaped (match char with
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'r' -> '\r'
        | 'b' -> '\b'
        | '"' | '\\' -> char
        | _ -> assert false) :: st) lexbuf
    }

  (* This is OCTAL, not DIGITAL, so putting 'o' is mandatory! *)
  | "\\o" (['0'-'9']['0'-'9']['0'-'9'] as s) {
      let c = Char.chr (Scanf.sscanf s "%o" (fun x -> x)) in (* CR jfuruse; error recovery *)
      format specials (Escaped c :: st) lexbuf
    }

  | "\\x" (['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F'] as s) {
      let c = Char.chr (Scanf.sscanf s "%x" (fun x -> x)) in (* CR jfuruse; error recovery *)
      format specials (Escaped c :: st) lexbuf
    }

  | "\\" (_ as char) {
        format specials (Escaped char :: st) lexbuf
      }

  | "$" 
      { 
	let exp_pos = dollared lexbuf in
	format specials
          (Conv (check_conversion lexbuf
		    { flags = [];
		      width = None;
		      precision = None;
		      ctype = Cs;
		      inlined_arg = Some exp_pos }) :: st) lexbuf 
      }
  | eof { List.rev st, None }

and exp st = parse
  | "}" { st }
  | "\\}" { exp (st ^ "}") lexbuf }
  | "\\" { exp (st ^ "\\") lexbuf }
  | ([^ '\\' '}']+ as s) { exp (st ^ s) lexbuf }
  | _ as c { 
      errorf lexbuf "illegal char in ${exp}: %C" c }
  | eof { 
      errorf lexbuf "unterminated ${exp}"
    }

{

let from_string specials s = 
  let lexbuf = Lexing.from_string s in
  let tokens, rest = format specials [] lexbuf in
  let rems = match rest with
    | Some (pos, _) -> Some (String.sub s pos (String.length s - pos))
    | None -> None
  in
  tokens, rems
;;


let rec parameters_conv conv =
  let params_width =
    match conv.width with
    | Some WPstar -> [ `Int ]
    | Some _ | None -> []
  in
  let params_precision =
    match conv.precision with
    | Some (Some WPstar) -> [ `Int ]
    | Some (Some _ | None) | None -> []
  in
  let params_ctype, conv' =
    let params_ctype, ctype' = 
      match conv.ctype with
      | Cformat_subst t -> 
	  let params, t' = parameters t in
	  `Format :: params, Cformat_subst t'
      | _ ->
	  (match conv.ctype with
	  | Cformat_subst _ -> assert false
	  | Cd | Cu | Cx | CX | Co -> [ `Int ]
	  | Cs | CS -> [ `String ]
	  | Cc | CC -> [ `Char ]
	  | Cf | CF | Ce | CE | Cg | CG -> [ `Float ]
	  | CB -> [ `Bool ]
	  | Cld | Clu | Clx | ClX | Clo -> [ `Int32 ]
	  | Cnd | Cnu | Cnx | CnX | Cno -> [ `Natint ]
	  | CLd | CLu | CLx | CLX | CLo -> [ `Int64 ]
	  | Ca -> [ `Formatter; `Argument ]
	  | Ct -> [ `Unit_formatter ]
	  | Cformat _ -> [ `Format ]  (* ? *)
	  | Cflush -> []
	  | Cpercent -> []),
	  conv.ctype
    in
    match conv.inlined_arg with
    | None -> params_ctype, { conv with ctype = ctype' }
    | Some e -> 
	match params_ctype with
	| [] -> assert false
	| x::xs -> 
	    `Applied (x,e) :: xs, 
	    { conv with 
	      ctype = ctype';
	      inlined_arg = None }
  in
  params_width @ params_precision @ params_ctype, 
  conv'

and parameters_token token = match token with
  | String _ | Char _ | Escaped _ -> [], token
  | Conv conv -> 
      let params, conv' = parameters_conv conv in
      params, Conv conv'

and parameters t = 
  let parameters_list, t' = List.split (List.map parameters_token t) in
  List.flatten parameters_list, t'

let parameters_to_application params =
  let cntr = ref 0 in
  let rev_abss, rev_apps =
    let rec reduce (rev_abss, rev_apps) =
      match rev_abss, rev_apps with
      | x::xs, `Var y::ys when x = y -> reduce (xs, ys)
      | _ -> rev_abss, rev_apps
    in
    reduce 
      (List.fold_left (fun (rev_abss, rev_apps) -> function
	| `Applied (_, e_pos) -> rev_abss, `Applied e_pos :: rev_apps
	| _ -> 
	    incr cntr;
	    !cntr :: rev_abss, `Var !cntr :: rev_apps)
	  ([], []) params)
  in
  List.rev rev_abss, List.rev rev_apps
;;

(* CR jfuruse: bad name *)
let from_string_to_classic specials s =
  let t, rems = from_string specials s in
  let parameters, t' = parameters t in
  let application = parameters_to_application parameters in
  t, application, to_string t', rems
;;
}
