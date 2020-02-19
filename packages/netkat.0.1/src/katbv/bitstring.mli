open Base

(** {2 Type} *)

(** the type of a bit string. AF: the table [H] represents a bitvector [v] where
    [v_i]=1 iff [(i, ())] is in [H]. Note that bit strings are interpreted left 
    to right as highest bit to lowest bit. So the string 110 is the binary
    representation of 6 and (100)_0 = 0 whereas (100)_2 = 1 *)
type t = (int, unit) Hashtbl.t [@@deriving sexp]
include Comparator.S with type t := t


(** {2 Constructors} *)

(** the zero bit string *)
val zero : t

(** [of_bool_list lst bv] is the bit string [v] where [v_i]=1 iff [lst_i]=true *)
val of_bool_list : bool list -> t

(** [of_int_list lst bv] the bit string [v] where [v_i]=1 iff [i] is in [lst] *)
val of_int_list: int list -> t

(** [of_binary s] is the bit string [v] where [v_i]=1 iff [s_i]="1" *)
val of_binary : string -> t

(** [of_ternary s] is the pair (z, n) where [z_i]=1 iff [s_i]="0" and 
    [n_i]=1 iff [s_i]="1" *)
val of_ternary : string -> t * t

(** {2 Basic Operations} *)

(** [max v] is [Some i] where [i] is the maximum of \{j:[v_j]=1\} *)
val max : t -> int option

(** [min v] is [Some i] where [i] is the minimum of \{j:[v_j]=1\} *)
val min : t -> int option

(** [v**v'] is bitwise multiplication of [v] and [v'] *)
val ( ** ) : t -> t -> t

(** [v++v'] is bitwise OR of [v] and [v'] *)
val ( ++ ) : t -> t -> t

(** [v -- v'] is [v''] where [v''_i]=0 if [v'_i]=1 and otherwise [v''_i]=[v_i] *)
val ( -- ) : t -> t -> t

(** [xor v v'] is the bitwise xor of [v] and [v'] *)
val xor : t -> t -> t

(** [is_zero v] is true iff [v]=0 *)
val is_zero : t -> bool

(** [equal v v'] is true iff [v]=[v'] *)
val equal : t -> t -> bool

(** [nth v i] is true iff [v_i]=1  *)
val nth : t -> int -> bool

(** {2 KAT+B! functions} *)

(** [lower_bound s a] is KAT+B! Boolean expression that is satisfied by [s:=v]
    iff v>=a where [s] is the variable name *)
val lower_bound : string -> t -> Katbb_lib.Ast.bexp

(** [upper_bound s b] is KAT+B! Boolean expression that is satisfied by [s:=v]
    iff [v]<=[b] where [s] is the variable name *)
val upper_bound : string -> t ->  Katbb_lib.Ast.bexp

(** [build_term_list s z n] is KAT+B! Boolean expression that is satisfied by [s:=v]
    iff [v_i=0] for all i such that [z_i=1] and [v_i=1] for all i such that 
    [n_i=1] *)
val build_term_list : string -> t -> t -> 
  (bool -> int -> 'a) -> 'a list

(** {2 Rendering} *)

val to_string : t -> string