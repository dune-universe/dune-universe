type qcm = A | B | C | D

val string_of_qcm : qcm -> string

val qcm_of_string : string -> qcm

val qcm_of_int : int -> qcm

type qlist = int list

val qlist_of_string : string -> qlist

val canonize_qlist : qlist -> qlist

val iter_hash : int -> int -> int

val hash_string : string -> int

type question_value = {
  text : string;
  proposed_answer : string array;
  rtype : Type.compo_type;
  answer : string;
  test_effort : int;
  test_max : int;
}

type question_type = { text : string; answer : string }

type question = Value of question_value | Type of question_type

type question_bank = {
  questions : question array;
  digest : Digest.t;
  shuffle : string option;
}

val mkqcm : string -> string list -> qcm -> question

val mkfun :
  ?test_effort:int ->
  ?test_max:int ->
  string ->
  string ->
  string ->
  string list ->
  question

val mkdeftype : string -> string -> question

val shuffle : string -> question_bank -> question_bank

val save_qbank : string -> question_bank -> unit

val load_qbank : string -> question_bank
