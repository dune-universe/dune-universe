val verbose : int ref

type compo_type =
  | Name of string * compo_type list
  | Abstract of string
  | Fun of compo_type list * compo_type
  | Prod of compo_type list

type func = { name : string; intypes : compo_type list; outtype : compo_type }

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

type named_type = {
  identifier : string;
  arguments : int;
  gen_fun :
    Random.State.t ->
    int ->
    (Random.State.t -> int -> compo_type -> float -> hidden_type * int) ->
    (compo_type -> gen_function) ->
    compo_type ->
    float ->
    hidden_type * int;
  to_string :
    (compo_type -> hidden_type -> string) -> compo_type -> hidden_type -> string;
  string_of_named :
    recdefprint -> (recdefprint -> compo_type -> string) -> compo_type -> string;
  boltzman_fun : (compo_type -> gen_function) -> compo_type -> gen_function;
}

val find_type : string -> named_type

val add_type_to_lib : ?rename:string -> named_type -> unit

val string_of_compo : compo_type -> string

val print_func : out_channel -> func -> unit

val string_of_sum : sum_type -> string

val print_prod_aux : ('a -> string) -> int -> 'a list -> string * string

val instantiate : string -> compo_type -> compo_type -> compo_type
