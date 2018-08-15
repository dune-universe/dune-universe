
(** 
    {2 Build efficient enumerations for datatypes}

    The exenum library offers constructors to build enumerations for datatypes, that is, functions
    from (arbitrarily large) integers to values. Such enumerations are very useful for unit testing.

    The library is efficient: the n-th element of an enumeration
    is returned without having computed the (n-1) previous elements.
    Complexity is in log(n), except for some pathological datatypes.

    Homepage: {{: https://github.com/lebotlan/ocaml-exenum} https://github.com/lebotlan/ocaml-exenum }

    {i Inspired by Feat: Functional Enumeration of Algebraic Types, by Duregard, Jansson, Wang, Chalmers University.}

    {i Contact: D. Le Botlan (github.lebotlan\@dfgh.met where you replace .met by .net.) }

    {3 Example}

    As an example, consider the following familiar datatype:
    {[ type term = Var of string | App of term * term | Lambda of string * term ]}
    Using exenum, one may easily generate zillions of different lambda-terms.
    For this example, we limit ourselves to four variable names : x, y, u, and v.
    Then, one may compute for instance term number 2000000000000, which happens to be
    {v ((((x v) (fun u -> y)) ((fun u -> y) (fun y -> y))) (((x v) (fun u -> v)) (fun u -> y))) v}

    Building an enumeration from a datatype is straightforward. For instance, the enumeration corresponding to type [term] is
    built as follows:
    {[
(* We restrict ourselves to four variable names. *)
let e_vars = from_list ~name:"variables" ["x" ; "y" ; "u" ; "v"]

(* Type term is recursive, hence we need a lazy enumeration first. *)
let rec e_term = lazy
  begin
    (* In order to use the enumeration recursively, we need to "pay" a recursive fee. *)
    let r_term = pay e_term in
   
    (* Now, this is the direct translation of the datatype definition. *)
    union 
      [ map e_vars (fun x -> Var x) ;
        map (pair r_term r_term) (fun (t1, t2) -> App (t1, t2)) ;
        map (pair e_vars r_term) (fun (x, t) -> Lambda (x, t))  ] 
  end

(* Here is the enumeration for lambda-terms. *)
let e_term = Lazy.force e_term
    ]}

See examples in {{: https://github.com/lebotlan/ocaml-exenum/tree/master/examples} https://github.com/lebotlan/ocaml-exenum/tree/master/examples}

*)

(** The type of exhaustive enumerations of values of type 'a. 
    Enumerations can be finite of infinite. *)
type 'a enum
type 'a t = 'a enum

(** {3 Basics} *)

(** Builds a finite enumeration from a finite set of values. 
    The name is used for nicer debugging. *)
val from_list : ?name:string -> 'a list -> 'a t

(** Enumeration of a single value (derived from {!from_list}). *)
val single : ?name:string -> 'a -> 'a t

(** [cardinal enum] Returns the cardinality of [enum]. None means infinity. *)
val cardinal : 'a t -> Z.t option

(** [get enum n] Returns the nth value of type 'a, starting at 0.
    @raise Failure if [n] is greater or equal than the cardinality of [enum]. *)
val get : 'a t -> Z.t -> 'a

(** {3 Finite enumerations for ground types} *)

(** One element: (). *)
val e_unit   : unit t

(** Two elements: true, false *)
val e_bool   : bool t

(** Contains 256 elements: from '\000' to '\255'. *)
val e_char   : char t
val e_pchar  : char t (** Printable characters (from 32 to 125). *)

(** Enumeration of a big-integer interval. *)
val e_biginterval : Z.t -> Z.t -> Z.t t

(** Enumeration of an integer interval. *)
val e_interval : int -> int -> int t

(** [sub ~max enum] Returns a finite enumeration with at most [max] elements. *)
val sub : max:Z.t -> 'a t -> 'a t


(** {3 Infinite enumerations for ground types} *)

(** For these enumerations of integers, do not expect the n-th value to be equal to the integer n. Integers are shuffled. *)

(** Strictly positive numbers: \[1, +infty\[*)
val e_bigpos : Z.t t 

(** Natural numbers: \[0, +infty\[ *)
val e_bignat : Z.t t

(** All numbers: \] -infty, +infty \[ 
    This enumeration starts from 0 and alternates between positive and negative values. *)
val e_bigint : Z.t t


(** Natural integers: \[0, max_int\] as an infinite enumeration (hence, non-injective). *)
val e_nat : int t

(** Strictly positive integers: \[1, max_int\] as an infinite enumeration (hence, non-injective). *)
val e_pos : int t

(** All integers: \[min_int, max_int\].
    This enumeration starts from 0 and alternates between positive and negative values. 
    This enumeration is infinite, hence non-injective. *)
val e_int : int t


(** Strings, built only with printable characters. *)
val e_string : string t 

(** Strings, built only with the given characters. *)
val e_rstring : char list -> string t 


(** {3 Composition} *)

(** [union enums] builds an enumeration from a union of enumerations. 
    If [enums] are disjoint enumerations, the resulting enumeration is injective. *)
val union : ('a t) list -> 'a t

(** Builds an enumeration from a cartesian product of enumerations. *)
val product : ('a t) list -> ('a list) t

(** Convenience function to build an enumeration of pairs from two enumerations (derived from product and projection functions). *)
val pair   : ('a t) -> ('b t) -> ('a * 'b) t
val triple : ('a t) -> ('b t) -> ('c t) -> ('a * 'b * 'c) t

(** This is the same as [pair] *)   
val tuple2  : ('a t) -> ('b t) -> ('a * 'b) t

(** This is the same as [triple] *)
val tuple3  : ('a t) -> ('b t) -> ('c t) -> ('a * 'b * 'c) t

val tuple4  : ('a t) -> ('b t) -> ('c t) -> ('d t) -> ('a * 'b * 'c * 'd) t
val tuple5  : ('a t) -> ('b t) -> ('c t) -> ('d t) -> ('e t) -> ('a * 'b * 'c * 'd * 'e) t
val tuple6  : ('a t) -> ('b t) -> ('c t) -> ('d t) -> ('e t) -> ('f t) -> ('a * 'b * 'c * 'd * 'e * 'f) t

(** Enumerations of lists of arbitrary size, starting from the empty list. *)
val e_list : 'a t -> ('a list) t

(** Enumerations of non-empty lists (see e_list). *)
val e_ne_list: 'a t -> ('a list) t

(** Enumerations of arrays. *)
val e_array : 'a t -> ('a array) t

val e_option : 'a t -> ('a option) t


(** {3 Recursion, map} *)

(** An enumeration is a possibly-infinite set of finite parts.
    Recursive, infinite enumerations must be built by increasing the cost of each part.
    The enumeration given in argument {b must} be infinite (which is usually the case when building a recursive enumeration). 
    See examples to understand how to use [pay]. *)
val pay : ('a t Lazy.t) -> 'a t

(** Builds an enumeration by mapping an existing enumeration. *)
val map : 'a t -> ('a -> 'b) -> 'b t


(** {3 Helper functions} *)

(** [show enum to_string index len] outputs values of the given enumeration, using the [to_string] conversion function, 
    from index index to index (index + len - 1). *)
val show : 'a t -> ('a -> string) -> int -> int -> unit

(** [bigshow enum to_string index len] is similar to [show], except that index is a Z.t. *)
val bigshow : 'a t -> ('a -> string) -> Z.t -> int -> unit

(** [tester enum ~len f] applies f in sequence to different values of the enumeration. 
    First, [len] values are taken starting at 0 (or starting from [from], if specified).
    Then, the current index is multiplied by 2 (that is, we start at 2*len) and again [len] values are considered.
    This is repeated forever (or while the current index is lower than the upper bound [upto]).

    If [verbose_period] is strictly positive, a message giving the current index is printed on stdout every [verbose_period] tests.
    If tos is given, the test value is printed too.

    Typical usage is: [tester enum ~len:10000 f], where f is a testing function.

    @raise Assert_failure if [len <= 0]
 *)
val tester : 'a t -> ?from:Z.t -> ?upto:Z.t -> ?verbose_period:int -> ?tos:('a -> string) -> len:int -> ('a -> unit) -> unit


(**/**)
(* You certainly don't need to use this. *)
val get_exen: 'a enum -> 'a Exenum_internals.Exen.t


(* TODO: automatically build the print function ? *)
    
