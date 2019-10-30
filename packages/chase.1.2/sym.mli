(** Symbols *)

(** A symbol is a pair consisting of an integer and a string.
   Symbols are the same when their integers agree.  The string is
   used for printing.  The printer handles the case of two different
   symbols with the same string by adding a suffix when
   printing. *)

type sym

(** Make a flesh symbol. *)
val mk_sym : string -> sym

(** Integer of a symbol *)
val int_of_sym : sym -> int

(** String of a symbol *)
val string_of_sym : sym -> string

(** Equality for symbols *)
val same : sym -> sym -> bool

(** Comparisons for symbols *)
val cmp : sym -> sym -> int

(** Create a fresh indentifier that shares the string of the input *)
val clone : sym -> sym

(** Lookup for an association list with symbols as keys. *)
val assoc_opt : sym -> (sym * 'a) list -> 'a option

(** Printer for symbols *)

(** Remove suffixes from a string. *)
val root_name : string -> string

(** A printing context *)
type ctx

(** make a context *)
val mk_ctx : unit -> ctx

(** Generate printed name for a symbol.  Adds a suffix when
   necessary *)
val pname : ctx -> sym -> string

(** Sets of symbols as ordered lists *)

val mem : sym -> sym list -> bool
val add : sym -> sym list -> sym list
val remove : sym -> sym list -> sym list
val diff : sym list -> sym list -> sym list

(** Hash table with SYMs as keys *)

module SymHashtbl : Hashtbl.S with type key = sym

(** Tries of symbols *)

(** A trie node *)
type trie =
  | T of sym * trie list

val insert : trie list -> sym list -> trie list

val to_list : trie list -> sym list list

(** Return the number of leafs (records) in a trie. *)
val count : trie list -> int

val fold_trie : ('a -> sym -> 'a) -> 'a -> trie list -> 'a

(** Tables of tries *)

val find_trie : (trie list) SymHashtbl.t -> sym -> trie list
val insert_trie : (trie list) SymHashtbl.t -> sym -> sym list -> unit

(** A printer for dubugging *)

val print_sym : Format.formatter -> sym -> unit
