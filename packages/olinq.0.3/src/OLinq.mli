
(* This file is free software, part of OLinq. See file "license" for more details. *)

(** {1 LINQ-like operations on collections}

    The purpose is to provide powerful combinators to express iteration,
    transformation and combination of collections of items.

    Functions and operations are assumed to be referentially transparent, i.e.
    they should not rely on external side effects, they should not rely on
    the order of execution.

    {[

      OLinq.(
        of_list [1;2;3]
        |> flat_map (fun x -> (x -- (x+10)))
        |> count ()
        |> sort ()
        |> run_list
      );;

      - : (int * int) list = [(1, 1); (2, 2); (3, 3); (4, 3); (5, 3); (6, 3);
                              (7, 3); (8, 3); (9, 3); (10, 3); (11, 3); (12, 2); (13, 1)]

        OLinq.(
          IO.read_file "/tmp/foo"
          |> IO.lines
          |> sort ()
          |> IO.to_file_lines "/tmp/bar"
        );;
      - :  `Ok ()
    ]}


    {[
      OLinq.(
        1 -- 20
        |> group_by (fun x -> x mod 3)
        |> run_list
      ) ;;
      - : (int * int list) list =
      [(2, [20; 17; 14; 11; 8; 5; 2]);
       (0, [18; 15; 12; 9; 6; 3; 0]);
       (1, [19; 16; 13; 10; 7; 4; 1])]
    ]}

*)

type 'a iter = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a hash = 'a -> int
type 'a or_error = [`Ok of 'a | `Error of string ]
type 'a printer = Format.formatter -> 'a -> unit

(* some helpers *)
(*$inject
  let lsort l = List.sort Pervasives.compare l
  let llsort l = l |> List.map lsort |> lsort

*)

module Iterable : sig
  type 'a t

  val of_list : 'a list -> 'a t
  val of_vec : 'a OLinq_vec.t -> 'a t

  val to_list : 'a t -> 'a list
  val to_vec : 'a t -> 'a OLinq_vec.t
end

(** {2 Polymorphic Maps} *)

type ('a, 'b) map = ('a, 'b) OLinq_map.t

(** {2 Main Type} *)

type ('a, +'card) t constraint 'card = [<`One | `AtMostOne | `Any]
(** Type of a query that returns zero, one or more values of type ['a].
    The parameter ['card] indicates how many elements are in the
    collection,
    with [`Any] indicating the number is unknown, [`AtMostOne] that there
    are 0 or 1 elements and [`One] exactly one.

    To simplify, this is very similar to a type ['a t] that would
    behave like a collection of ['a]. The ghost parameter ['card]
    is only useful to restrict the kind of operations one
    can perform on these collections. For example , a
    value of type [('a, [`One]) t] contains exactly one
    element so we can access it safely.

    Conceptually, the cardinalities are ordered from most precise (`One)
    to least precise (`Any):  [`One < `AtMostOne < `Any].

*)

type 'a t_any = ('a, [`Any]) t
type 'a t_one = ('a, [`One]) t
type 'a t_at_most_one = ('a, [`AtMostOne]) t

(** {2 Initial values} *)

val empty : ('a, [>`AtMostOne]) t
(** Empty collection *)

val return : 'a -> ('a, [>`One]) t
(** Return one value *)

val of_list : 'a list -> ('a, [`Any]) t
(** Query that just returns the elements of the list *)

val of_array : 'a array -> ('a, [`Any]) t
val of_array_i : 'a array -> (int * 'a, [`Any]) t

val range : int -> int -> (int, [`Any]) t
(** [range i j] goes from [i] up to [j] included *)

val (--) : int -> int -> (int, [`Any]) t
(** Synonym to {!range} *)

val of_hashtbl : ('a,'b) Hashtbl.t -> ('a * 'b, [`Any]) t

val of_iter : 'a iter -> ('a, [`Any]) t
(** Query that returns the elements of the given sequence. *)

val of_vec : 'a OLinq_vec.t -> ('a, [`Any]) t

val of_queue : 'a Queue.t -> ('a, [`Any]) t

val of_stack : 'a Stack.t -> ('a, [`Any]) t

val of_string : string -> (char, [`Any]) t
(** Traverse the characters of the string *)

val of_map : ('a, 'b) map -> ('a * 'b, [`Any]) t
(** [of_map m] yields each binding of [m] *)

val of_multimap : ('a, 'b list) map -> ('a * 'b, [`Any]) t
(** [of_multimap m] yields each single binding of [m] *)

(** {6 Execution} *)

val run : ?limit:int -> ('a, _) t -> 'a Iterable.t
(** Execute the query, possibly returning an error if things go wrong
    @param limit max number of values to return *)

val run_list : ?limit:int -> ('a, _) t -> 'a list

val run_array : ?limit:int -> ('a, _) t -> 'a array

val run_vec : ?limit:int -> ('a, _) t -> 'a OLinq_vec.t

val run1 : ('a, [`One]) t -> 'a
(** Run the query and return the only value *)

val run_head : ('a, _) t -> 'a option
(** Return first result *)

val run1_exn : ('a, _) t -> 'a
(** unsafe shortcut for {!run_head}.
    @raise Not_found if the query contains 0 element *)

(** {6 Basics} *)

val map : ('a -> 'b) -> ('a, 'card) t -> ('b, 'card) t
(** Map each value *)

val (>|=) : ('a, 'card) t -> ('a -> 'b) -> ('b, 'card) t
(** Infix synonym of {!map} *)

val filter : ('a -> bool) -> ('a, _) t -> ('a, [`Any]) t
(** Filter out values that do not satisfy predicate. We lose precision
    on the cardinality because of type system constraints. *)

val size : (_,_) t -> (int, [>`One]) t
(** [size t] returns one value, the number of items returned by [t] *)

val choose : ('a, _) t -> ('a, [>`AtMostOne]) t
(** Choose one element (if any, otherwise empty) in the collection.
    This is like a "cut" in prolog. *)

val filter_map : ('a -> 'b option) -> ('a, _) t -> ('b, [`Any]) t
(** Filter and map elements at once *)

val flat_map_seq : ('a -> 'b Seq.t) -> ('a, _) t -> ('b, [`Any]) t
(** Same as {!flat_map} but using seq *)

val flat_map_iter : ('a -> 'b iter) -> ('a, _) t -> ('b, [`Any]) t
(** Same as {!flat_map} but using iterators *)

val flat_map_l : ('a -> 'b list) -> ('a, _) t -> ('b, [`Any]) t
(** map each element to a collection and flatten the result *)

val flatten_list : ('a list, _) t -> ('a, [`Any]) t

val flatten_seq : ('a Seq.t,_) t -> ('a, [`Any]) t

val flatten_iter : ('a iter,_) t -> ('a, [`Any]) t

val flatten_map : (('a, 'b) map, _) t -> ('a * 'b, [`Any]) t

val flatten_multimap : (('a, 'b list) map, _) t -> ('a * 'b, [`Any]) t

val take : int -> ('a, _) t -> ('a, [`Any]) t
(** Take at most [n] elements *)

(*$Q & ~small:(fun (i,j,lim) -> abs (i-j) + lim)
  Q.(triple small_int small_int small_int) (fun (i,j,lim) -> \
    ((i -- j) |> take lim |> size |> run1) <= lim)
*)

val take1 : ('a, _) t -> ('a, [>`AtMostOne]) t
(** Specialized version of {!take} that keeps only the first element *)

(*$Q & ~small:(fun (i,j) -> abs (i-j))
  Q.(pair small_int small_int) (fun (i,j) -> \
    ((i -- j) |> take1 |> size |> run1) <= 1)
*)

val take_while : ('a -> bool) -> ('a, _) t -> ('a, [`Any]) t
(** Take elements while they satisfy a predicate *)

val sort : ?cmp:'a ord -> unit -> ('a, [`Any]) t -> ('a, [`Any]) t
(** Sort items by the given comparison function. Only meaningful when
    there are potentially many elements *)

(*$Q
  Q.(list small_int) (fun l -> \
    let rec sorted = function [] | [_] -> true | x::y::l -> x<=y && sorted (y::l) in \
    of_list l |> sort () |> run_list |> sorted)
*)

val sort_by : ?cmp:'b ord -> ('a -> 'b) -> ('a, [`Any]) t -> ('a, [`Any]) t
(** [sort_by proj c] sorts the collection [c] by projecting elements using
    [proj], then using [cmp] to order them *)

(*$Q
  Q.(list (pair small_int float)) (fun l -> \
    let rec sorted = function [] | [_] -> true | x::y::l -> snd x<=snd y && sorted (y::l) in \
    of_list l |> sort_by snd |> run_list |> sorted)
*)

val distinct : ?cmp:'a ord -> unit -> ('a, [`Any]) t -> ('a, [`Any]) t
(** Remove duplicate elements from the input collection.
    All elements in the result are distinct. *)

(** {6 Aggregation} *)

val group_by : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash ->
  ('a -> 'b) -> ('a, [`Any]) t -> ('b * 'a list, [`Any]) t
(** [group_by f] takes a collection [c] as input, and returns a collection
    of pairs [k, l] where every element [x] of [l] satifies [f x = k].
    In other words, elements of the collection that have the same
    image by [f] are grouped in the same list. *)

(*$= & ~printer:Q.Print.(list (pair int @@ list int))
  [0, [2; 4; 10; 100]; 1, [3; 7; 11; 19]] \
  (group_by (fun x->x mod 2) (of_list [2;3;4;7;10;11;19;100]) \
   |> run_list |> List.map (fun (x,y) -> x, lsort y) |> lsort)
*)

val group_by_reflect : ?cmp:'b ord -> ?eq:'b equal -> ?hash:'b hash ->
  ('a -> 'b) -> ('a, [`Any]) t -> (('b,'a list) map, [>`One]) t
(** [group_by_reflect f] takes a collection [c] as input, and returns
    a multimap [m] such that for each [x] in [c], [x] occurs in [m] under the
    key [f x].
    In other words, [f] is used
    to obtain a key from [x], and [x] is added to the multimap using this key. *)

val count :
  ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
  unit -> ('a, [`Any]) t -> ('a * int, [`Any]) t
(** [count c] counts how many times each element of the collection
    occur, and returns pairs of [x, count(x)] *)

(*$= & ~printer:Q.Print.(list (pair char int))
  ['a', 3; 'b', 2; 'c', 1] \
  (count () (of_list ['a'; 'b'; 'b'; 'a'; 'c'; 'a']) |> run_list |> lsort)
*)

(*$= & ~printer:Q.Print.(list @@ pair int int)
  [0,3_000_001; 1, 3_000_000; 2, 3_000_000] \
  (0 -- 9_000_000 |> map (fun i->i mod 3) |> count () |> run_list |> lsort)
*)

val count_reflect :
  ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
  unit -> ('a, [`Any]) t -> (('a, int) map, [>`One]) t
(** [count_reflect c] returns a map from elements of [c] to the number
    of time those elements occur. *)

val fold : ('b -> 'a -> 'b) -> 'b -> ('a, _) t -> ('b, [>`One]) t
(** Fold over the collection *)

val is_empty : ('a, [<`AtMostOne | `Any]) t -> (bool, [>`One]) t

val sum : (int, [<`AtMostOne | `Any]) t -> (int, [>`One]) t

(*$Q
  Q.(list small_int) (fun l -> \
    (of_list l |> sum |> run1) = (List.fold_left (+) 0 l))
*)

val contains : ?eq:'a equal -> 'a -> ('a, _) t -> (bool, [>`One]) t
(** [contains x q] returns [true] if [x] is among the elements returned
    by [q]. Careful, this runs [q] and might be slow! *)

val average : (int, _) t -> (int, [>`One]) t
val max : (int, _) t -> (int, [>`One]) t
val min : (int, _) t -> (int, [>`One]) t

val for_all : ('a -> bool) -> ('a, _) t -> (bool, [>`One]) t
val exists : ('a -> bool) -> ('a, _) t -> (bool, [>`One]) t
val find : ('a -> bool) -> ('a, _) t -> ('a option, [>`One]) t
val find_map : ('a -> 'b option) -> ('a, _) t -> ('b option, [>`One]) t

(** {6 Binary Operators} *)

val join : ?cmp:'key ord -> ?eq:'key equal -> ?hash:'key hash ->
  ('a -> 'key) -> ('b -> 'key) ->
  merge:('key -> 'a -> 'b -> 'c option) ->
  ('a, _) t -> ('b, _) t -> ('c, [`Any]) t
(** [join key1 key2 ~merge] is a binary operation
    that takes two collections [a] and [b], projects their
    elements resp. with [key1] and [key2], and combine
    values [(x,y)] from [(a,b)] with the same [key]
    using [merge]. If [merge] returns [None], the combination
    of values is discarded. *)

val outer_join : ?cmp:'key ord -> ?eq:'key equal -> ?hash:'key hash ->
  ('a -> 'key) -> ('b -> 'key) ->
  merge:('key -> 'a list -> 'b list -> 'c option) ->
  ('a, _) t -> ('b, _) t -> ('c, [`Any]) t
(** [outer_join key1 key2 ~merge] is a binary operation
    that takes two collections [a] and [b], projects their
    elements resp. with [key1] and [key2], and, for each key [k]
    occurring in at least one of them:
    - compute the list [l1] of elements of [a] that map to [k]
    - compute the list [l2] of elements of [b] that map to [k]
    - call [merge k l1 l2]. If [merge] returns [None], the combination
      of values is discarded, otherwise it returns [Some c]
      and [c] is inserted in the result.
    @since 0.2 *)

val group_join : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
  ('b -> 'a) -> ('a,_) t -> ('b,_) t ->
  ('a * 'b list, [`Any]) t
(** [group_join key2] associates to every element [x] of
    the first collection, all the elements [y] of the second
    collection such that [eq x (key y)]. Elements of the first
    collections without corresponding values in the second one
    are mapped to [[]] *)

val group_join_reflect : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
  ('b -> 'a) -> ('a,_) t -> ('b,_) t ->
  (('a, 'b list) map, [>`One]) t
(** Same as {!group_join}, but reflects the groups as a multimap *)

(*$R
  let res =
    group_join fst
      (of_list [1;2;3])
      (of_list [1, "1"; 1, "one"; 2, "two"; 4, "four"])
    |> map (fun (x,y) -> x, lsort (List.map snd y))
    |> run_list
    |> lsort
  in
  assert_equal
    ~printer:Q.Print.(list @@ pair int @@ list string)
    [1, ["1"; "one"]; 2, ["two"]; 3, []; 4, []]
    res
*)

val product : ('a, _) t -> ('b,_) t -> ('a * 'b, [`Any]) t
(** Cartesian product *)

(*$=
  [1, false; 1, true; 2, false; 2, true; 3, false; 3, true] \
  (product (1 -- 3) (of_list [true; false]) |> run_list |> lsort)
*)

val append : ('a,_) t -> ('a,_) t -> ('a, [`Any]) t
(** Append two collections together *)

(*$=
  [1;2;3;4;5;6;7;8;9;10] (append (1--3) (4--10) |> run_list)
*)

val inter :
  ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
  ('a,_) t -> ('a,_) t -> ('a,[`Any]) t
(** Intersection of two collections. Each element will occur at most once
    in the result *)

val union :
  ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
  ('a,_) t -> ('a,_) t -> ('a,[`Any]) t
(** Union of two collections. Each element will occur at most once
    in the result *)

val diff :
  ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
  ('a,_) t -> ('a,_) t -> ('a,[`Any]) t
(** Set difference *)

(*$=
  [1;2;8;9;10] (diff (1--10) (3--7) |> run_list)
*)

val subset :
  ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash ->
  ('a,_) t -> ('a,_) t -> (bool,[`One]) t
(** [subset () a b] returns [true] if all elements of [a] belong to [b] *)

(*$T
  subset (2 -- 4) (1 -- 4) |> run1
  not (subset (1 -- 4) (2 -- 10) |> run1)
*)

(** {6 Tuple and Options} *)

(** Specialized projection operators *)

val map_fst : ('a -> 'b) -> ('a * 'c, 'card) t -> ('b * 'c, 'card) t

val map_snd : ('a -> 'b) -> ('c * 'a, 'card) t -> ('c * 'b, 'card) t

val flatten_opt : ('a option, _) t -> ('a, [`Any]) t
(** Flatten the collection by removing [None] and mapping [Some x] to [x]. *)

(** {6 Applicative} *)

val pure : 'a -> ('a, _) t
(** Synonym to {!return} *)

val app : ('a -> 'b, 'card) t -> ('a, 'card) t -> ('b, 'card) t
(** Apply each function to each value. The cardinality should be the lowest
    upper bound of both input cardinalities (any,_) -> any, (one,one) -> one, etc. *)

val (<*>) : ('a -> 'b, 'card) t -> ('a, 'card) t -> ('b, 'card) t
(** Infix synonym to {!app} *)

(** {6 Monad}

    Careful, those operators do not allow any optimization before running the
    query, they might therefore be pretty slow. *)

val flat_map : ('a -> ('b, _) t) -> ('a,_) t -> ('b, [`Any]) t
(** Use the result of a query to build another query and immediately run it. *)

val (>>=) : ('a, _) t -> ('a -> ('b, _) t) -> ('b, [`Any]) t
(** Infix version of {!flat_map} *)

(** {6 Misc} *)

val lazy_ : ('a lazy_t, 'card) t -> ('a, 'card) t

exception UnwrapNone

val opt_unwrap_exn : ('a option, 'card) t -> ('a, 'card) t
(** @raise UnwrapNone if some option is None *)

(*$T
  [1;2;3] = (opt_unwrap_exn (of_list [Some 1; Some 2; Some 3]) |> run_list)
  (try ignore (opt_unwrap_exn (of_list [Some 1; None; Some 2]) |> run_list); false \
   with UnwrapNone -> true)
*)

(** {6 Infix} *)

module Infix : sig
  val (--) : int -> int -> (int, [`Any]) t
  val (>|=) : ('a, 'card) t -> ('a -> 'b) -> ('b, 'card) t
  val (<*>) : ('a -> 'b, 'card) t -> ('a, 'card) t -> ('b, 'card) t
  val (>>=) : ('a, _) t -> ('a -> ('b, _) t) -> ('b, [`Any]) t
end

(** {6 Adapters} *)

val reflect_vec : ('a, _) t -> ('a OLinq_vec.t, [>`One]) t
(** [reflect_seq q] evaluates all values in [q] and returns a sequence
    of all those values. Also blocks optimizations *)

val reflect_list : ('a, _) t -> ('a list, [>`One]) t
(** [reflect_list q] evaluates all values in [q] and returns a list
    of all those values. Also blocks optimizations *)

(*$=
  [1;2;3;4] (of_list [1;2;3;4] |> reflect_list |> run1)
*)

val reflect_hashtbl : (('a * 'b), _) t -> (('a, 'b) Hashtbl.t, [>`One]) t
(** Build a hashtable from the collection *)

val reflect_queue : ('a,_) t -> ('a Queue.t, [>`One]) t

val reflect_stack : ('a,_) t -> ('a Stack.t, [>`One]) t

module AdaptSet(S : Set.S) : sig
  val of_set : S.t -> (S.elt, [`Any]) t
  val reflect : (S.elt,_) t -> (S.t, [>`One]) t
  val run : (S.elt, _) t -> S.t
end

module AdaptMap(M : Map.S) : sig
  val of_map : 'a M.t -> (M.key * 'a, [`Any]) t
  val reflect : (M.key * 'a, _) t -> ('a M.t, [`One]) t
  val run : (M.key * 'a, _) t -> 'a M.t
end

module IO : sig
  val read_chan : in_channel -> (string, [>`One]) t
  (** Read the content of the whole channel in (blocking), returning the
      corresponding string. The channel will be read at most once
      during execution, and its content cached; however the channel
      might never get read because evaluation is lazy. *)

  val read_file : string -> (string, [>`One]) t
  (** Read a whole file (given by name) and return its content as a string *)

  val lines : (string, _) t -> (string, [`Any]) t
  (** Convert a string into a collection of lines *)

  val lines_l : (string, 'card) t -> (string list, 'card) t
  (** Convert each string into a list of lines *)

  val concat : string -> (string,_) t -> (string, [>`One]) t
  (** [concat sep q] joins all the strings in [q] together,
      similar to [String.concat sep (run_list q)] basically. *)

  val unlines : (string, _) t -> (string, [>`One]) t
  (** Join lines together *)

  val out : out_channel -> (string, _) t -> unit
  val out_lines : out_channel -> (string, _) t -> unit
  (** Evaluate the query and print it line by line on the output *)

  (** {8 Run methods} *)

  val to_file : string -> (string, _) t -> unit or_error
  val to_file_exn : string -> (string, _) t -> unit

  val to_file_lines : string -> (string, _) t -> unit or_error
  val to_file_lines_exn : string -> (string, _) t -> unit
end

val print : ?sep:string -> 'a printer -> ('a,_) t printer
(** Evaluate the sequence of elements and print them
    @since 0.2 *)

