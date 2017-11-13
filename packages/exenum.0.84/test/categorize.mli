open Exenum_internals.Convenience

(* 'Categories' (as in Youtube, not immediately related to categories as in Haskell)
 *
 * Used ONLY for the surjectivity test.
 * Not used by the core library.
 *
 * We provide a categorizing function for values, that is categorize : 'a -> category
 * and we know the cardinality of each category, that is category_size : category -> int
 * Then, the surjectivity test enumerate the values for a given range, and list the categories which are fully enumerated.
 * The larger the exenum range is, the more categories we expect to be fully enumerated.
 *
 * Note that categories are distinct from parts (although they are partially similar).
 * *)

(* A category whose identifiers are of type 'b. *)
type 'b category

val size : 'b category -> Z.t
val name : 'b category -> string

(* Returns a unique identifier for this category. *)
val id   : 'b category -> 'b

type ('a,'b) catfun = ('a -> 'b category)

val pair   : ('a1, 'b1) catfun -> ('a2, 'b2) catfun -> ('a1 * 'a2, 'b1 * 'b2) catfun
val triple : ('a1, 'b1) catfun -> ('a2, 'b2) catfun -> ('a3, 'b3) catfun -> ('a1 * 'a2 * 'a3, 'b1 * 'b2 * 'b3) catfun
val list : ('a, 'b) catfun -> ('a list, 'b list) catfun

val cat_bool : (bool, unit) catfun
val cat_int  : (int, int) catfun
val cat_rstring : char list -> (string, int) catfun

