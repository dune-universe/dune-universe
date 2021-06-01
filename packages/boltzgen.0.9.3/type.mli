val verbose : int ref

(** A type for ocaml type*)
type compo_type =
  | Name of string * compo_type list
  | Abstract of string
  | Fun of compo_type list * compo_type
  | Prod of compo_type list

type func = { name : string; intypes : compo_type list; outtype : compo_type }
(** A type for named function *)

val flatten_prod : compo_type list -> compo_type list

val ano_func : func -> compo_type

val deano_func : string -> compo_type -> func

type sum_type_def = (string * compo_type option * float option) list

type sum_type = (string * string list) * sum_type_def

(* Poor man dynamic typing *)
type hidden_type

val hide : 'a -> compo_type -> hidden_type

val reveal : hidden_type -> compo_type -> 'a

type gen_function = float -> float * float

type recdefprint = (compo_type * string) list

type poly = ((compo_type list * int) * float) list

type poly_assoc = (compo_type * poly) list

type named_type = {
  identifier : string;
  boltz_identifier : string;
  is_simple : bool;
  arguments : int;
  get_equ :
    (compo_type -> poly_assoc -> poly_assoc) ->
    poly_assoc ->
    compo_type ->
    poly_assoc;
  gen_fun :
    Random.State.t ->
    int ->
    (Random.State.t -> int -> compo_type -> float -> hidden_type * int) ->
    (compo_type -> gen_function) ->
    compo_type ->
    float ->
    hidden_type * int;
  print :
    (compo_type -> Format.formatter -> hidden_type -> unit) ->
    compo_type ->
    Format.formatter ->
    hidden_type ->
    unit;
  string_of_named :
    recdefprint -> (recdefprint -> compo_type -> string) -> compo_type -> string;
  boltzman_fun : (compo_type -> gen_function) -> compo_type -> gen_function;
}

val find_type : string -> named_type

val rewrite : compo_type -> compo_type

val add_type_to_lib : ?rename:string -> named_type -> unit

(*val string_of_compo : ?use_boltz_id:bool -> compo_type -> string*)

val pp_compo : ?use_boltz_id:bool -> Format.formatter -> compo_type -> unit

(*val print_func : out_channel -> func -> unit*)

val pp_func : ?use_boltz_id:bool -> Format.formatter -> func -> unit

val pp_sum : Format.formatter -> sum_type -> unit

val print_prod_aux : ('a -> string) -> int -> 'a list -> string * string

val instantiate : string -> compo_type -> compo_type -> compo_type
