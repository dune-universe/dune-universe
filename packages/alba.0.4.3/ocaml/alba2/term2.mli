open Container
open Alba2_common


type fix_index = int
type decr_index = int
type oo_application = bool

type t =
  | Sort of Sorts.t
  | Variable of int
  | Application of t * t * Application_type.t
  | Lambda of abstraction
  | All of abstraction
  | Inspect of t * t * case array
  | Fix of fix_index * fixpoint

and typ = t

and abstraction =  string option * typ * t

and case

and fixpoint = (Feature_name.t option * typ * decr_index * t) array
(** Array of fixpoint components. Each component has a name, a type, a
   decreasing argument and a term. The term has [n] bound variables where [n]
   is the size of the fixpoint (usually 1). The bound variable [j] represents
   the component [n - j - 1]. *)

type fname = Feature_name.t option
type name_type = string option * typ
type fname_type = Feature_name.t option * typ
type gamma = fname_type array
type arguments = name_type array
type argument_list = name_type list

val proposition: t
val any: t
val box: t

val product: t -> t -> t option
val product1: t -> t -> t
val get_sort: t -> Sorts.t option

val variable0: t
val variable1: t
val variable2: t
val variable3: t
val variable4: t
val variable5: t
val apply1: t -> t -> t
val apply2: t -> t -> t -> t
val apply3: t -> t -> t -> t -> t
val apply4: t -> t -> t -> t -> t -> t
val apply_target: t -> t -> t
val binary: t -> t -> t -> t

val equal: t -> t -> bool
val equal1: t option -> t -> bool
val equal_arguments: arguments -> arguments -> bool

val shift: int -> t -> t
val up_from: int -> int -> t -> t
val up: int -> t -> t

val has_variables: (int->bool) -> t -> bool

val arrow: t -> t -> t



val substitute: t -> t -> t
(** [substitute a b] substitutes the variable 0 of the term [a] by the term
   [b]. All other variables of [a] are shifted down by 1.

   The term [a] has one bound variable (the variable 0). This bound variable
   is substituted. I.e. the call [substitute a b] performs the beta
   reduction [(lambda x.a) b -> a[x:=b]]. *)


val reduce_fixpoint: int -> fixpoint -> t
(** [reduce i fp] reduces the fixpoint [i] of [fp].

   The fixpoint [fp] consists of [n] components. The component [j] of [fp] has
   the term [t(j)]. The term [t(j)] has [n] bound variables. The variable [j]
   represents the [n - j - 1] component of the fixpoint.

   Fixpoints which are not mutually recursive have only one component.

   [reduce i fp] is the term [t(i)] where the bound variables are
   substitute. The bound variable [j] is substituted by [Fix (n - j - 1, fp)].
*)


val split_application: t -> t list -> t * t list
val apply_args: t -> t list -> t
val apply_n_args: t -> int -> t list -> t
val apply_arg_array: t -> t array -> t
val apply_standard: int -> int -> t -> t


val lambda: argument_list -> t -> t
val split_lambda0: int -> t -> int -> argument_list -> t * argument_list
val split_lambda: t -> arguments * t
val push_lambda:  arguments -> t -> t

val make_case: arguments -> t -> t -> case
val case_constructor: case -> t
val case_definition: case -> t
val case_pair: case -> t * t
val split_case: case -> argument_list * t * t

val split_product0: int -> typ -> int -> argument_list -> typ * argument_list
val split_product: typ -> arguments * typ
val push_product: arguments -> typ -> typ

val beta_reduce: t -> t list -> t * t list


val to_level: int -> t -> t
val to_index: int -> t -> t
