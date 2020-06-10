(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(** The submodule [Gen] offers facilities for generating values of many common
    types. It draws data from the file whose name is supplied on the command
    line. (If no file name is supplied, it draws random data from OCaml's
    [Random] module.) *)
module Gen : sig

  (** A generator is a function of unit to a value of some desired type ['a].
     It is permitted for a generator to fail by calling [reject] or [guard].
     This failure is not fatal; it causes the engine to backtrack (to an
     unspecified point) and retry. *)
  type 'a gen =
    unit -> 'a

  (** [reject] generates nothing. It always fails. *)
  val reject: 'a gen

  (** [guard b] fails if [b] is false. *)
  val guard: bool -> unit

  (** [byte] generates a byte. A byte is viewed as an unsigned integer. *)
  val byte: int gen

  (** [bits] generates a signed integer. *)
  val bits: int gen

  (** [bool] generates a Boolean. *)
  val bool: bool gen

  (** [int n] generates an integer in the semi-open interval [\[0..n)].
      If this interval is empty, the generator fails. *)
  val int: int -> int gen

  (** [interval i j] generates an integer in the semi-open interval [\[i, j)].
      If this interval is empty, the generator fails. *)
  val interval: int -> int -> int gen

  (** [interval_ i j] generates an integer in the closed interval [\[i, j\]].
      If this interval is empty, the generator fails. *)
  val interval_: int -> int -> int gen

  (** [lt j] is synonymous with [int j] and with [interval 0 j]. *)
  val lt: int -> int gen

  (** [le j] is synonymous with [interval_ 0 j]. *)
  val le: int -> int gen

  (** [sequential()] produces a fresh stateful sequential generator of
      integers. This generator is deterministic. Every time this generator is
      invoked, it produces a new integer, counting from 0 and up. *)
  val sequential: unit -> int gen

  (** [choose xs] picks an element in the list [xs].
      If this list is empty, the generator fails. *)
  val choose: 'a list -> 'a gen

  (** An option generator. If [element] is an element generator, then
      [option element] is a generator of optional elements. *)
  val option: 'a gen -> 'a option gen

  (** A list generator. If [element] is an element generator and if [n] is a
      length generator (where a length is a nonnegative integer), then [list n
      element] is a list generator. *)
  val list: int gen -> 'a gen -> 'a list gen

  (** An array generator. If [element] is an element generator and if [n] is a
      length generator (where a length is a nonnegative integer), then [array n
      element] is a array generator. *)
  val array: int gen -> 'a gen -> 'a array gen

end (* Gen *)

(* -------------------------------------------------------------------------- *)

(** We use the [PPrint] library to pretty-print OCaml code. In particular,
    the module [PPrint.OCaml] can be of interest. *)

type document =
  PPrint.document

(** The submodule [Print] offers a few facilities for printing OCaml
    values and expressions. It is meant to serve as a complement for
    the modules [PPrint] and [PPrint.OCaml], which are part of the
    library [pprint]. *)
module Print : sig

  (** A printer maps a value to a [PPrint] document. *)
  type 'a printer =
    'a -> document

  (** Whereas [PPrint.OCaml.int i] displays the integer [i] naked, this [int]
      combinator encloses [i] within a pair of parentheses if it is negative.
      This ensures that [int i] can safely be used as an argument in an
      application. *)
  val int: int printer

  (** Whereas [PPrint.OCaml.option] prints [Some(2)], this [option] combinator
      prints [(Some 2)]. This ensures that [option element o] can safely be
      used as an argument in an application. *)
  val option: 'a printer -> 'a option printer

  (** [pair] is a pair printer. *)
  val pair: 'a printer -> 'b printer -> ('a * 'b) printer

  (** [list] is an alias for [PPrint.OCaml.flowing_list]. *)
  val list: 'a printer -> 'a list printer

  (** [array] is an alias for [PPrint.OCaml.flowing_array]. *)
  val array: 'a printer -> 'a array printer

  (** Whereas [PPrint.parens] simply encloses a document in parentheses, this
      [parens] combinator encloses the document within parentheses and
      specifies that if the whole thing does not fit a single line, then it
      must be split over three lines and the content must be indented by two
      spaces. *)
  val parens: document -> document

  (** [apply doc docs] constructs an OCaml application of [doc] to the list of
      arguments [docs]. The arguments are separated with spaces, and if the
      whole application does not fit on a line, then a flowing style is
      adopted, inserting a line break where necessary. *)
  val apply: document -> document list -> document

  (** [assert_ doc] constructs an OCaml assertion, that is, an application of
      the variable [assert] to the document [doc] within parentheses. *)
  val assert_: document -> document

  (** [candidate_finds doc] prints the document [doc] inside a comment of the
      form [(* candidate finds _ *)]. See e.g. the demo [demos/working/bag]
      for an example of its use. *)
  val candidate_finds: document -> document

end

(* -------------------------------------------------------------------------- *)

(** A value of type ['a code] is a value of type ['a] that is able to produce
   (on demand) a piece of OCaml code for itself. This code is represented as a
   string, which must form a valid OCaml expression (and must be surrounded
   with parentheses, if necessary). *)
type 'a code

(** The function [code] is a constructor for the type ['a code]. *)
val code : document Lazy.t -> 'a -> 'a code

(* -------------------------------------------------------------------------- *)

(* The type ['a postcondition] describes a reference function that verifies
   a result of type ['a] produced by the candidate implementation. *)

type 'a postcondition =
  'a -> diagnostic

(* The postcondition must determine whether the candidate result is valid or
   invalid. In the latter case, it must produce a piece of OCaml code, which
   is parameterized with one variable [observed], and which explains why this
   input is unacceptable. *)

and diagnostic =
  | Valid
  | Invalid of (document -> document)

(* -------------------------------------------------------------------------- *)

(** A specification is a description of the expected behavior of an operation.
    For instance, the specification of the [pop] operation on a stack could be:
    "provided it is applied to a nonempty stack, [pop] returns an element".
    Equipping every operation with a specification is necessary so that the
    system knows how many arguments the operation expects, what properties
    these arguments should satisfy, what results are produced, what to do with
    these results, and so on. *)

(** A specification of type [('r, 'c) spec] describes an operation (or, more
    generally, a value) whose type on the reference side is ['r] and whose type
    on the candidate side is ['c]. *)
type (_, _) spec

(* -------------------------------------------------------------------------- *)

(** The following operations offer various means of constructing
   specifications. *)

(** [declare_concrete_type "t"] declares a new concrete type named "t". This
    name is used in certain error messages. Which type ['t] is actually
    meant is inferred by the OCaml type-checker.

    A concrete type is the same in the reference implementation and in the
    candidate implementation, which explains why the return type of this
    function is [('t, 't) spec].

    The newly-declared type is {b not} equipped with a generator; this can
    be done separately by using [&&&] or [&&@]. It is important to equip a
    concrete type with a generator (only) if this type is used to describe an
    input in a specification.

    The newly-declared type may be equipped with an equality test,
    represented by the optional argument [equal]. If this argument is
    omitted, OCaml's polymorphic equality is used. It is important to equip a
    concrete type with an equality only if this type is used to describe an
    output in a specification. The newly-declared type may be equipped with a
    printer, represented by the optional argument [print]. This argument is
    required in almost all situations. As an exception, it can be omitted if
    this type describes an input and if this type is later equipped with a
    generator via [&&@]. *)
val declare_concrete_type :
  ?print: ('t -> document) ->
  ?equal: ('t -> 't -> bool) code ->
  (* name: *) string ->
  ('t, 't) spec

(** [declare_abstract_type "t"] declares a new abstract type named "t". This
    name is used in certain error messages. Which types ['r] and ['c] are
    actually meant, in the reference implementation and in the candidate
    implementation, is inferred by the OCaml type-checker.

    An abstract type may be implemented in different ways in the reference
    implementation and in the candidate implementation, which explains why
    the return type of this function is [('r, 'c) spec]. For instance, a
    sequence may be represented as a linked list in the reference
    implementation and as a resizeable array in the candidate implementation.

    The newly-declared type may be equipped with a [check] function. This
    function is invoked by the system after every operation, and is applied
    to every data structure of type "t" that is currently at hand. The
    [check] function has access both to the reference data structure (the
    "model") and to the candidate data structure. It is expected to have no
    effect if all is well and to fail by raising [Assertion_failure _] if
    something is wrong. It is up to the user to decide how much checking
    should be performed. Checking nothing at all is permitted. Checking that
    the candidate data structure seems well-formed is a common strategy.
    Checking that the candidate data structure is well-formed and is in
    agreement with its model is the most comprehensive check that can be
    performed.

    The optional parameter [var] is a base name that is used for variables
    of type "t". If it is omitted, the first letter of the type's name is
    used. *)
val declare_abstract_type :
  ?check: ('r -> ('c -> unit) code) ->
  ?var: string ->
  (* name: *) string ->
  ('r, 'c) spec

(** The operator [&&&] equips a concrete type with a generator. In an
    application [t &&& g], [t] should be a concrete type produced by
    [declare_concrete_type], and [g] should be a generator for this type.
    Equipping a concrete type with a generator is required when this type
    is used to describe an input. *)
val (&&&)  : ('t, 't) spec -> (unit -> 't) -> ('t, 't) spec

(** The operator [&&@] equips a concrete type with a generator and (at the
    same time) with a printer for the generated values. In an application [t
    &&@ g], the function [g] must have type [unit -> 't code], which means
    that must be able to produce not only a value of type ['t], but also, on
    demand, OCaml code for this value.

    This feature can be used to declare an OCaml function type as a concrete
    type (thus working around the fact that a value of a function type cannot
    be printed). *)
val (&&@) : ('t, 't) spec -> (unit -> 't code) -> ('t, 't) spec

(** [unit] represents the concrete type [unit]. It has been predeclared
    using [declare_concrete_type]. *)
val unit : (unit, unit) spec

(** [bool] represents the concrete type [bool]. It has been predeclared
    using [declare_concrete_type]. *)
val bool : (bool, bool) spec

(** [int] represents the concrete type [int]. It has been predeclared using
    [declare_concrete_type]. Unlike [unit] and [bool], the specification
    [int] is {b not} equipped with a generator, because it usually does not
    make sense to generate an integer in the huge interval [\[min_int,
    max_int\]]. Thus, if this type is used to describe an input, it must be
    equipped with a generator using [&&&] or [&&@]. To do so, one can also
    use one of the functions that follow; they are just [int] equipped with a
    suitable generator. *)
val int : (int, int) spec

(** [interval i j] is [int &&& Gen.interval i j]. *)
val interval  : int -> int -> (int, int) spec

(** [interval_ i j] is [int &&& Gen.interval_ i j]. *)
val interval_ : int -> int -> (int, int) spec

(** [lt j] is [int &&& Gen.lt j]. *)
val lt : int -> (int, int) spec

(** [le j] is [int &&& Gen.le j]. *)
val le : int -> (int, int) spec

(** [sequential()] is [int &&& Gen.sequential()]. *)
val sequential: unit -> (int, int) spec

(** [***] is the pair constructor. It is used to describe a pair (which
   typically appears as a function argument or result). *)
val ( *** ) :
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec ->
  ('r1 * 'r2, 'c1 * 'c2) spec

(** [^^>] is the arrow constructor. It is used to describe a function of one
    argument. By using it several times, one can also describe curried
    functions of several arguments, as is usual in OCaml. *)
val (^^>) :
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec ->
  ('r1 -> 'r2, 'c1 -> 'c2) spec

(** [^&>] is the dependent arrow constructor. It is used to describe a
    function of one argument and give a name to this argument, which can be
    referred to in the codomain (the part of the specification that follows
    the operator [^&>]). A typical example is the specification of a [get]
    function for a sequence: [seq ^&> fun s -> lt (length s) ^^> int]. Its
    first argument, a sequence, receives the name [s]. This allows specifying
    that the second argument, an integer, must lie in the semi-open interval
    [\[0, length s)]. Here, the data structure [s] is built by the reference
    implementation, so the [length] function must be the one provided by
    the reference implementation. *)
val (^&>) :
  ('r1, 'c1) spec ->
  ('r1 -> ('r2, 'c2) spec) ->
  ('r1 -> 'r2, 'c1 -> 'c2) spec

(** [%] is the subset constructor. It is used to restrict the set of
    arguments that can be passed to an operation; in other words, it is used
    to express a precondition. For example, to express the fact that the
    operation [pop] must be applied to a nonempty stack, one would use
    [nonempty % stack], where the function [nonempty] tests whether a
    (reference) stack is nonempty, and [stack] is the abstract type of
    stacks. This constructor currently cannot be applied to the result
    of an operation (where it would express a postcondition). *)
val (%) : ('r -> bool) -> ('r, 'c) spec -> ('r, 'c) spec

(** [map] specifies that a transformation must be applied to a value. The user
    must provide OCaml code for the transformation (in the form of a string) as
    well as the transformation itself, which in the reference implementation
    has type ['r1 -> 'r2] and in the candidate implementation has type ['c1 ->
    'c2]. The transformation can be applied to an output of an operation or to
    the operation itself, in which case the types ['r1], ['r2], ['c1], and
    ['c2] are function types. The transformation currently cannot be applied
    to an input. *)
val map :
  string -> ('r1 -> 'r2) -> ('c1 -> 'c2) ->
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec ->
  ('r1, 'c1) spec

(** The constructor [nondet] is used to annotate the result of an operation
    whose {i specification} is nondeterministic. This indicates that several
    results are permitted; therefore, it does not make sense to ask the
    reference implementation for "the" expected result and to verify (via an
    equality test) that the candidate implementation produces the same
    result. Instead, in this case, the system runs the candidate
    implementation first, and asks the reference implementation to verify (by
    whatever appropriate means) that this result is permitted by the
    specification. For this reason, in this case, the reference
    implementation has type ['a postcondition] instead of ['a]. At this
    time, only a concrete type can be annotated with [nondet]. *)
val nondet :
  ('a, 'a) spec ->
  ('a postcondition, 'a) spec

(* -------------------------------------------------------------------------- *)

(** [declare name spec reference candidate] declares the existence of an
    operation. Its parameters are the operation's name (used for printing), the
    operation's type and specification (represented by a runtime value of type
    [('r, 'c) spec]), and the implementations of this operation in the reference
    and in the candidate (represented by values of type ['r] and ['c]). *)
val declare : string -> ('r, 'c) spec -> 'r -> 'c -> unit

(** [main fuel] is the main entry point. The parameter [fuel] is the maximum
    length of a run (expressed as a number of operations). A small value, such
    as 5, 10, or 20, is typically used. The function [prologue], if present, is
    run once at the beginning of each run; it is allowed to call data
    generation functions in the module [Gen] and can therefore set up the
    global parameters of the run, if desired. *)
val main : ?prologue:(unit -> unit) -> int -> unit
