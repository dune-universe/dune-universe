(* Time-stamp: <modified the 18/10/2017 (at 11:31) by Erwan Jahier> *)

(** Sub module of EvalClock that defines clock-checking utilities.

   We have Two kind of substitutions. 

    - subst1 is used to deal with the clock name binding, to relate
    node parameters and node arguments (to check clock equality up to
    alpha conversion).

    - subst2 is used to deal with polymorphic clock variables. Indeed,
    constant clock is intrinsically polymorphic. Hence, when clocking
    a contant (e.g., "42"), we return the clock_info Ci_var i, where i
    is a fresh integer. Afterwards, when encoutering an expression
    such as "42+x", where we know the clock of x (clk_x), we produce
    the substitution (i,clk_x).

    XXX Shouldn't I merge the 2 kind of substitutions ?  That would
    mean I need to invent a fake var_info_eff, but it would make the
    whole more homogeous.

    XXX Make subst abstract?
*)

type subst1 = (Lv6Id.t * Lv6Id.t) list
type subst2

type subst = subst1 * subst2
(* = (Lv6Id.t * Lv6Id.t) list  * (int * Lic.clock) list *)

val empty_subst : subst
val subst_to_string : subst -> string


val apply_subst:subst -> Lic.clock -> Lic.clock

(* only apply the part of the subst that deals with clock var *)
val apply_subst2:subst -> Lic.clock -> Lic.clock
val add_subst2 : int -> Lic.clock -> subst2 -> subst2
val add_link2 : int -> int -> subst2 -> subst2

val apply_subst_val_exp : subst -> Lic.val_exp -> Lic.val_exp

(** [f lxm s clk_arg clk_par] Raises a Compile_error is the 2
    Lic.clock [clk_arg] and [clk_par] are not compatible. The
    accumulated substitution s is augmented with eventual clock
    variable instanciation.

    For instance, when clock checking the expression 'toto(a,b)', if
    toto has the clock profile "alpha x alpha -> alpha", and the args
    'a' and 'b' are on clock 'clk', the expression 'toto(a,b)' should
    be on clk too. We can check that later using the association
    "alpha->clk" is added in the returned substitution.

    XXX UnifyClock.f is not a good name actually. CheckClock.f would
    better reflect the reality

*)
val f : subst -> Lxm.t -> Lic.clock -> Lic.clock -> subst
val list : Lxm.t list -> Lic.clock list -> subst -> subst

val new_clock_var : subst -> subst * Lic.clock

(** Transform a const into a val_exp 
    [const_to_val_eff lxm flag s const]
    - flag tells if abstract const should be expanded 
    - s is used to provide a clock to val_exp (actually, this is why
    this function is in this module, but it's a weird reason).
    
    
*)
val const_to_val_eff: Lxm.t -> bool -> subst -> Lic.const -> subst * Lic.val_exp  
