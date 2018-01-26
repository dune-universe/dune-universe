(** Pretty printing combinator based on [Format]
    with automatic parenthese insertion 
*)

open Spotlib.Spot

type assoc = Left | Right | Noassoc
(** Associativity *)
    
type level = float
(** Operator precedence. Smaller is stronger. *)

(** {1 Pretty printing monad } *)
    
type 'a t = assoc -> level -> 'a
(** Type of the pretty printer combinator:
    under the given assoc position and precendence level, compute some 'a
 *)
    
include Monad.T with type 'a t := 'a t
(** Pretty printer combinator is a state monad *)

type token
(** Pretty printing command *)
  
type ppr = token t
(** Type of pretty printers *)

(** { 1 Basic printer functions } *)
    
val string : string -> ppr
(** [string s] pretty-prints the string [s] *)  

val box : int -> ppr -> ppr
(** [box n ppr] creates a horizontal layout box.
    An internal call of [space] and [cut] may cause a line break
    if the entire contents do not fit with the current printable width.
    If a line break is required the following contents are printed 
    with additional indentation of [n].
*)
    
val vbox : int -> ppr -> ppr
(** [vbox n ppr] creates a vertical layout box.
    If the entire contents do not fit with the current printable width,
    all calls of [space] and [cut] cause line breaks.
    If a line break is required the following contents are printed 
    with additional indentation of [n].
*)

val ( ++ ) : ppr -> ppr -> ppr
(** [ppr1 ++ ppr2] concatenates pretty printings *)
  
val seq : ppr list -> ppr
(** Concatenation of pretty printings. *)
  
val cut : ppr
(** A hint of a line break. *)
  
val space : ppr
(** A hint of a line break, preceded with a white space. *)

val flush : ppr
(** Force flushing. All the opened boxes are closed and a line break is
    inserted. *)

val nop : ppr
(** Do nothing *)
  
(** { 1 Level and environments } *)

val left : 'a t -> 'a t           
(** Put the arg in the "left" environment *)

val right : 'a t -> 'a t          
(** Put the arg in the "right" environment *)

val noassoc : 'a t -> 'a t        
(** Put the arg in the "noassoc" environment *)

val level : level -> 'a t -> 'a t 
(** Put the arg in the specified level *)

val reset : 'a t -> 'a t          
(** Put the arg in "noassoc" env and 0.0 level *)

val check_against_current_level : level -> [ `Same | `Stronger | `Weaker ] t
(** Compare the argument against the current level:
    `Same     : the same as the current level
    `Stronger : the argument is stronger than the current
    `Weak     : the argument is weaker than the current
*)

val need_paren : assoc -> level -> bool t
(** Check the printer object of [assoc] and [level] requires 
    parenthesis wrapping or not in the current environment *)

(** {1 Combinators with automatic parenthese insertion }

    These implements common printing with auto parenthesis wrapping.
    Note: They may not suit with your own taste.
*)

type parens = string * string
(** left and right parenthese. The default is "(" and ")" *)
    
val parenbox : ?parens:parens -> assoc -> level -> ppr -> ppr
(** [parenbox ?parens assoc level ppr] pretty-prints [ppr] as an object of
    associativity [assoc] and precedence [level]. 
    Parentheses '(' and ')' are automatically wrapped when required.
*)
  
val binop    : ?parens:parens -> assoc -> level -> op:ppr -> ppr -> ppr -> ppr
(** [binop ?parens assoc level ~op left right] pretty prints binary operation
    <left> <op> <right> where [op] has [assoc] and [level].
    Parentheses '(' and ')' are automatically wrapped when required.
*)
  
val list     : ?parens:parens -> level -> ppr -> ppr list -> ppr
(** [list ?parens lev sep pprs] is to create a list without a box. [lev] is the level
    of the separator [sep]. 
    Parentheses '(' and ')' are automatically wrapped when required.
*)

val prefix   : ?parens:parens -> level -> op:ppr -> ppr -> ppr
(** [prefix ?parens level ~op p] pretty-prints an prefix operator application expression
    <op> <p> where [op]'s level is [level]. 
    Parentheses '(' and ')' are automatically wrapped when required.
*)
  
val postfix  : ?parens:parens -> level -> op:ppr -> ppr -> ppr
(** [postfix ?parens level ~op p] pretty-prints an postfix operator application expression
    <p> <op> where [op]'s level is [level]. 
    Parentheses '(' and ')' are automatically wrapped when required.
*)
  
val parens : string -> string -> ppr -> ppr
(** [parens left right ppr] surrounds [ppr] with the [left] and [right] strings.
    The level used for pretty-printing the internal [ppr] is reset to -1.0 
    to prevent from having auto parens for it *)

(** {1 OCaml like printers } 

    Printing combinators which use the same operator assciativity and precedence
    as OCaml language.
*)
  
module OCaml : sig
  val sequence     : ppr list -> ppr
  (** for [e1; e2; e3] *)
    
  val if_then_else : ppr -> ppr -> ppr -> ppr
  (** for [if e1 then e2 else e3] *)
    
  val if_then      : ppr -> ppr -> ppr
  (** for [if e1 then e2] *)

  val ty_as        : ppr -> ppr -> ppr
  (** for [t as 'a] *)

  val tuple        : ppr list -> ppr
  (** for [e1, e2, e3] *)
    
  val ( ^-> )      : ppr -> ppr -> ppr
  (** for [t1 -> t2] *)
    
  val ( + )        : ppr -> ppr -> ppr
  (** for [e1 + e2] *)
    
  val ( - )        : ppr -> ppr -> ppr
  (** for [e1 - e2] *)
    
  val ( * )        : ppr -> ppr -> ppr
  (** for [e1 * e2] *)

  val ty_tuple     : ppr list -> ppr
  (** for [t1 * t2 * t3] *)

  val uminus       : ppr -> ppr
  (** for [- e] *)

  val app          : ppr -> ppr -> ppr
  (** for [e1 e2] *)
end

(** {1 Drivers } *)
  
val format : ?assoc:assoc -> ?level:level -> ('a -> ppr) -> Format.t -> 'a -> unit
(** [format ?assoc ?level conv ppf a] pretty-prints [a] using the conversion 
    function [conv] to the formatter [ppf]. 
    The initial associativity and level are given by [assoc] and [level].
*)

val buffer : ('a -> ppr) -> Buffer.t -> ?assoc:assoc -> ?level:level -> 'a -> unit
(** [buffer conv buf ?assoc ?level a] pretty-prints [a] using the conversion 
    function [conv] to the buffer [buf].
    The initial associativity and level are given by [assoc] and [level].
*)

val show : ('a -> ppr) -> ?assoc:assoc -> ?level:level -> 'a -> string
(** [show conv ?assoc ?level a] pretty-prints  [a] using the conversion 
    function [conv] then returns the result as a string.
    The initial associativity and level are given by [assoc] and [level].
*)


(** Functor version of drivers *)
module MakeDrivers(M : sig type t val ppr : t -> ppr end) : sig
  open M
  val format : Format.t -> t -> unit
  (** Pretty-prints a value to a formatter *)
    
  val buffer : Buffer.t -> ?assoc:assoc -> ?level:level -> t -> unit
  (** Pretty-prints a value to a buffer *)

  val show   : ?assoc:assoc -> ?level:level -> t -> string
  (** Pretty-prints a value to a string *)

  val dump   : Format.t -> t -> unit
  (** Output printing commands for debugging *)
end

module Test :sig 
  val test : unit -> unit
  (** Testing *)
end
