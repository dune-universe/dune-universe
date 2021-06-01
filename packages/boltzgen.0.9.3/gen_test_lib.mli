(** Boltzgen runtime library entry point*)

val compute_boltzman : Type.func -> float -> float * float
(** [compute_boltzman fd z] Evaluate the bolzman generating function for function signature [fd]
    using boltzman parameter [z] *)

val set_max_size : int -> unit

val rand_fun : string -> int -> 'a -> 'b
(** [rand_fun size type seed arg] is a pure generic function, it generate a value of type [type] 
    [seed] and [arg] are parameters for the value [size] control the size of the output*)

val random_state : unit -> Random.State.t

val mname_of_string : string -> string

val print_typedef : Format.formatter -> Type.sum_type list -> unit
(** Print a type definition*)

val print_sig : Format.formatter -> Type.sum_type list * Type.func -> unit
(** Print a module signature matching type and function signature *)

(** Internal test generation function*)

val generic_loop :
  ('a ->
  out_err:bool ->
  int ->
  (Format.formatter -> unit -> unit) ->
  string ->
  (Format.formatter -> unit -> unit) ->
  unit) ->
  ('a -> out_err:bool -> ?throw:bool -> ?canonize:string -> string -> unit) ->
  ('a -> 'b -> unit) ->
  ?out_err:bool ->
  ?tsrange:int * int ->
  ?throw:bool ->
  ?canonize:string ->
  ?boltz_evaluated:Type.sum_type list * Type.func * float ->
  'a ->
  float ->
  int ->
  string ->
  'b ->
  unit

val gen_test :
  ?out_err:bool ->
  ?ftotest:string ->
  ?tsrange:int * int ->
  ?boltz_evaluated:Type.sum_type list * Type.func * float ->
  Format.formatter ->
  float ->
  int ->
  string ->
  unit

val gen_to_string : ?throw:bool -> ?canonize:string -> Type.func -> string

val gen_value :
  ?tsrange:int * int ->
  ?boltz_evaluated:Type.sum_type list * Type.func * float ->
  Format.formatter ->
  float ->
  int ->
  string ->
  unit

val gen_test_direct :
  ?out_err:bool ->
  ?throw:bool ->
  ?canonize:string ->
  ?boltz_evaluated:Type.sum_type list * Type.func * float ->
  Format.formatter ->
  float ->
  int ->
  string ->
  unit

val gen_test_diff :
  ?out_err:bool ->
  ?tsrange:int * int ->
  ?throw:bool ->
  ?canonize:string ->
  ?boltz_evaluated:Type.sum_type list * Type.func * float ->
  string ->
  string ->
  Format.formatter ->
  float ->
  int ->
  string ->
  unit

(** Runtime function *)

val nb_test : int ref

val nb_fail : int ref

val assert_equal :
  ?throw:bool ->
  ?err:bool ->
  ('a -> string) ->
  ('b -> string) ->
  string ->
  (unit -> 'a) ->
  (unit -> 'b) ->
  unit

val assert_equal_arg :
  ?throw:bool ->
  ?err:bool ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('c -> 'a) ->
  ('c -> 'b) ->
  'c ->
  unit

val assert_equal_string :
  ?err:bool -> string -> (unit -> string) -> (unit -> string) -> unit

val gen_test_t : ?out_err:bool -> float -> int -> string -> unit
(** Simple test generation function*)

val gen_test_d :
  ?throw:bool -> ?canonize:string -> float -> int -> string -> unit

val gen : ?out_err:bool -> float -> int -> string -> unit

val gen_dir : float -> int -> string -> unit

val gen_qbank :
  ?really_test:bool -> ?override_login:string -> string -> int -> unit
