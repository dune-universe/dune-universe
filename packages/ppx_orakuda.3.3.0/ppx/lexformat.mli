exception Error of int * int * string
val errorf : Lexing.lexbuf -> ('a, unit, string, 'b) format4 -> 'a
val errorf_at : int -> int -> ('a, unit, string, 'b) format4 -> 'a

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
	    [false] 
	    [b]: convert a boolean argument (for backward compatibility; do not
	    use in new programs). *)
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
    | Arg_rex_ref of char (* for $0 $1 .. $` $' $& $+ *)

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

val conversion_to_string : conversion -> string

val compile_conversion : conversion -> [`String of string | `Star] list

val from_string : char list -> string -> t * string option
(** Tokens and remains if stopped at special chars.
    Lexing stops when it finds a non-escaped special char 
*)

val from_string_to_classic :
  char list
  -> string 
  -> t 
  * (int list * [`Applied of (inlined_arg * int) | `Var of int] list) 
  * string
  * string option (** remains *)

