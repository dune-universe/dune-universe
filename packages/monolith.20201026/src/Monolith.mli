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

(**Monolith offers facilities for testing an OCaml library by comparing its
   implementation (known as the candidate implementation) against a reference
   implementation.

   For general information on Monolith and its workflow,
   please consult
   {{:https://gitlab.inria.fr/fpottier/monolith/-/blob/master/README.md}README.md}
   and the paper
   {{:http://cambium.inria.fr/~fpottier/publis/pottier-monolith-2021.pdf}Strong Automated Testing of OCaml Libraries},
   which describes the use and the design of Monolith
   in somewhat greater depth than this documentation.

   This documentation is split up into the following parts:
    - Facilities for {!section:gen}.
    - Facilities for {!section:print}.
    - Combinators for constructing {!section:spec}.
    - Functions for setting up and starting the Monolith {!section:engine}.
    - Miscellaneous runtime {!section:support} functions.

  *)

(* -------------------------------------------------------------------------- *)

(** {1:gen Generating Data} *)

(**The submodule [Gen] offers facilities for generating values of many common
   types. *)
module Gen : sig

  (**A value of type ['a gen] is a generator of values of type ['a].

     A generator is a function of type [unit -> 'a].

     A generator has the ability to draw random data from a source that is
     specified (on the command line) when the Monolith engine is started. An
     end user who wishes to implement generator does not have direct access to
     this source of random data, but can access it indirectly by calling other
     generators such as [bool], [byte], [bits], and so on.

     A generator can fail if (somehow) it detects that it has made incorrect
     choices and entered a dead end. It does so by invoking the functions
     [reject] or [guard]. This not a fatal failure. This is a silent failure
     that causes the engine to backtrack (to an unspecified point) and retry.

     Invoking a generator is permitted only while the engine is running, that
     is, while a call to {!val:main} is ongoing. *)
  type 'a gen =
    unit -> 'a

  (**[reject] generates nothing. It always fails. *)
  val reject: 'a gen

  (**[guard b] fails if [b] is false. *)
  val guard: bool -> unit

  (**[byte] generates a byte. A byte is viewed as an unsigned integer. It is
     therefore a value in the semi-open interval [\[0, 256)]. *)
  val byte: int gen

  (**[bits] generates a signed integer. *)
  val bits: int gen

  (**[bool] generates a Boolean value. *)
  val bool: bool gen

  (**[char] generates a character. *)
  val char: char gen

  (**[int n] generates an integer in the semi-open interval [\[0, n)].
     If this interval is empty, the generator fails. *)
  val int: int -> int gen

  (**[semi_open_interval i j] generates an integer in the semi-open
     interval [\[i, j)]. If this interval is empty, the generator
     fails. *)
  val semi_open_interval: int -> int -> int gen

  (**[closed_interval i j] generates an integer in the closed interval
     [\[i, j\]]. If this interval is empty, the generator fails. *)
  val closed_interval: int -> int -> int gen

  (**[lt j] is synonymous with [int j] and with [semi_open_interval 0 j]. *)
  val lt: int -> int gen

  (**[le j] is synonymous with [closed_interval 0 j]. *)
  val le: int -> int gen

  (**[sequential()] produces a fresh stateful sequential generator of
     integers. This generator is deterministic. Every time this generator is
     invoked, it produces a new integer, counting from 0 and up. *)
  val sequential: unit -> int gen

  (**[choose xs] picks an element in the list [xs].
     If this list is empty, the generator fails. *)
  val choose: 'a list -> 'a gen

  (**An option generator. *)
  val option: 'a gen -> 'a option gen

  (**A result generator. *)
  val result: 'a gen -> 'b gen -> ('a, 'b) result gen

  (**A list generator. If [n] is a length generator (where a length is a
     nonnegative integer) and if [element] is an element generator then [list n
     element] is a list generator. *)
  val list: int gen -> 'a gen -> 'a list gen

  (**An array generator. If [n] is a length generator (where a length is a
     nonnegative integer) and if [element] is an element generator then [array
     n element] is a array generator. *)
  val array: int gen -> 'a gen -> 'a array gen

  (**A string generator. If [n] is a length generator (where a length is a
     nonnegative integer) and if [char] is a character generator then [string
     n char] is a string generator. *)
  val string: int gen -> char gen -> string gen

end (* Gen *)

(**A value of type ['a gen] is a generator of values of type ['a]. *)
type 'a gen = 'a Gen.gen

(* -------------------------------------------------------------------------- *)

(**{1:print Displaying Data and Code} *)

(**We use the {{:https://github.com/fpottier/pprint/}PPrint} library to
   pretty-print OCaml code. *)
type document =
  PPrint.document

(**The submodule [Print] offers facilities for printing OCaml values and
   expressions. *)
module Print : sig

  (**A printer transforms a value to into a document. A printer is said to be
     safe if it produces a document that is unambiguously delimited (e.g.,
     delimited with parentheses). It is unsafe otherwise. *)
  type 'a printer =
    'a -> document

  (**[int] is an integer literal printer. It is safe. *)
  val int: int printer

  (**[bool] is a Boolean literal printer. It is safe. *)
  val bool: bool printer

  (**[char] is a character literal printer. It is safe. *)
  val char: char printer

  (**[string] is a string literal printer. It is safe. *)
  val string: string printer

  (**[option] is an option printer. It is safe. *)
  val option: 'a printer -> 'a option printer

  (**[result] is a result printer. It is safe. *)
  val result: 'a printer -> 'b printer -> ('a, 'b) result printer

  (**[pair] is a pair printer. It is safe. *)
  val pair: 'a printer -> 'b printer -> ('a * 'b) printer

  (**[list] is a list printer. It is safe. *)
  val list: 'a printer -> 'a list printer

  (**[array] is an array printer. It is safe. *)
  val array: 'a printer -> 'a array printer

  (**[parens] encloses its argument within a pair of parentheses. It is safe.
     If the document thus obtained does not fit a single line, then it is
     split over three lines and its content is indented by two spaces. *)
  val parens: document -> document

  (**[apply doc docs] constructs an OCaml application of [doc] to the list of
     arguments [docs]. The arguments are separated with spaces, and if the
     whole application does not fit on a line, then a flowing style is
     adopted, inserting a line break where necessary. This combinator is
     unsafe: the application is not parenthesized. *)
  val apply: document -> document list -> document

  (**[assert_ doc] constructs an OCaml assertion, that is, an application of
     the variable [assert] to the document [doc], surrounded with parentheses.
     This combinator is unsafe: the assertion is not parenthesized. *)
  val assert_: document -> document

  (**[comment doc] prints the document [doc] inside a comment (preceded with a
     breakable space). It is safe. *)
  val comment: document -> document

  (**[candidate_finds doc] prints the document [doc] inside a comment of the
     form [(* candidate finds _ *)]. See e.g. the demo
     {{:https://gitlab.inria.fr/fpottier/monolith/-/blob/master/demos/working/bag/Reference.ml}[demos/working/bag]}
     for an example of its use. It is safe. *)
  val candidate_finds: document -> document

end

(**A value of type ['a gen] is a printer of values of type ['a]. *)
type 'a printer = 'a Print.printer

(* -------------------------------------------------------------------------- *)

(**A value of type [appearance] is a printable representation an OCaml
   expression. One can think of the type [appearance] almost as synonym for
   the type [document]. In particular, the function {!val:document} is an
   injection of [document] into [appearance]. The type [appearance] offers a
   few bells and whistles that the type [document] does not have; these
   include the ability to print applications in a customized manner. *)
type appearance

(**[constant] is an injection of [string] into [appearance]. The appearance
   [constant c] is printed as the string [c] (without quotes). *)
val constant: string -> appearance

(**[document] is an injection of [document] into [appearance]. The appearance
   [document doc] is printed as the document [doc]. *)
val document: document -> appearance

(**[infix op] is an appearance for an OCaml infix operator named [op], where
   [op] is the name of the operator, without parentheses. This appearance is
   set up so that an application of it to exactly two actual arguments is
   printed infix. An application of it to more or fewer than two actual
   arguments is printed using normal application syntax. *)
val infix: string -> appearance

(**A value of type ['a code] is a pair of a value of type ['a] and a printable
   representation of this value. Several specification combinators, including
   {!val:constructible}, {!val:deconstructible}, {!val:declare_abstract_type},
   {!val:map_into}, and {!val:map_outof} expect an argument of type ['a code].
   *)
type 'a code =
  'a * appearance

(* -------------------------------------------------------------------------- *)

(**{1:spec Specifications} *)

(**A specification is a description of an OCaml value. This value might be an
   argument for an operation, the result of an operation, or an operation
   itself.

   When a value is meant to be used as an argument, Monolith must be able to
   construct such a value. In that case, a {i constructible} specification
   must be given. For instance, [lt 10 *** lt 10], which means {i a pair of
   integers less than 10}, is a constructible specification.

   When a value is a result, Monolith must be able to deconstruct such a
   value. In that case, a {i deconstructible} specification must be given. For
   instance, [option (int *** int)], which means {i an optional pair of two
   integers}, is a deconstructible specification.

   When a value is an operation (a function), Monolith must be able to
   construct one or more arguments for this operation, to apply this
   operation, and to deconstruct the result. Thus, the arguments of a function
   type must be constructible, while its result must be deconstructible.

   For instance, the specification of the [pop] operation on a stack could be
   [nonempty % stack ^> element], which means {i [pop] expects a nonempty
   stack and returns an element}. Or it could be [stack ^!> element], which
   means {i [pop] expects a stack and either returns an element or raises an
   exception}.

   Equipping every operation with a specification is required for Monolith to
   know how many arguments the operation expects, what properties these
   arguments should satisfy, what result is produced, and what to do with this
   result.

   Some specifications are constructible, but not deconstructible. For
   example, the combinator {!val:constructible} yields specifications that are
   constructible, but not deconstructible.

   Some specifications are deconstructible, but not constructible. A typical
   example is {!val:int}. More generally, the combinator
   {!val:deconstructible} yields specifications that are deconstructible, but
   not constructible.

   Some specifications are both constructible and deconstructible. For
   example, the combinator {!val:ifpol} yields specifications that are both
   constructible and deconstructible.

   Some specifications are neither constructible nor deconstructible. As a
   typical example, the specification of an operation, built by the combinator
   {!val:(^>)}, is neither constructible nor deconstructible. This implies
   that Monolith has no direct support for higher-order functions. *)

(**A specification of type [('r, 'c) spec] describes a value whose type on the
   reference side is ['r] and whose type on the candidate side is ['c]. *)
type (_, _) spec

(* -------------------------------------------------------------------------- *)

(**The following combinators offer a wide variety of means of constructing
   specifications. *)

(**[constructible generate] describes a basic constructible type, that is, a
   type ['t] that is equipped with a generator.

   The function [generate] must have type [unit -> 't code], which means that
   it must produce a pair of a value and a printable representation of this
   value. (See also the combinator {!val:constructible_}, which has slightly
   different requirements.)

   It is worth noting that [constructible] {i can} be used, if desired, to
   construct a value whose type is a function type.

   When a value must be constructed, the function [generate] is applied once,
   and the value thus obtained is used both on the reference side and on the
   candidate side. This explains why the return type of this combinator is
   [('t, 't) spec].

   This specification is constructible. *)
val constructible:
  (unit -> 't code) ->
  ('t, 't) spec

(* -------------------------------------------------------------------------- *)

(**[easily_constructible generate print] describes a basic constructible type,
   that is, a type ['t] that is equipped with a generator.

   It is a special case of {!val:constructible}. It is less powerful, but is
   easier to use.

   The function [generate] must have type ['t gen]. The function [print] must
   have type ['t printer]. These functions are combined to obtain a generator
   of type [unit -> 't code], which is used in a call to {!val:constructible}.

   This specification is constructible. *)
val easily_constructible:
  't gen ->
  't printer ->
  ('t, 't) spec

(* -------------------------------------------------------------------------- *)

(**[deconstructible ~equal print] describes a basic deconstructible type, that
   is, a type ['t] that is equipped with an equality test and with a printer.

   The equality test [equal] is used to compare the values produced by the
   reference implementation and by the candidate implementation. (The
   reference value is the first argument; the candidate value is the second
   argument.)

   The argument [equal] is optional. If it is omitted, then OCaml's generic
   equality function [(=)] is used.

   Because the reference value and the candidate value are expected to have
   the same type, the return type of this combinator is [('t, 't) spec].

   This specification is deconstructible. *)
val deconstructible:
  ?equal:(('t -> 't -> bool) code) ->
  't printer ->
  ('t, 't) spec

(* -------------------------------------------------------------------------- *)

(**[declare_abstract_type()] declares a new abstract type, whose values on the
   reference side have type ['r] and whose values on the candidate side have
   type ['c].

   An abstract type is usually implemented in different ways in the reference
   implementation and in the candidate implementation. For instance, a
   sequence may be represented as a linked list in the reference
   implementation and as a resizeable array in the candidate implementation.

   The optional parameter [check] is a well-formedness check. If present, this
   function is applied by Monolith, after every operation, to every data
   structure of this abstract type that is currently at hand. This allows
   checking after every operation that every data structure remains
   well-formed.

   The [check] function is applied to two arguments, namely, the reference
   data structure of type ['r] and the candidate data structure of type ['c].

   If all is well, then [check] should return [()]. If something is wrong,
   then [check] should raise an exception, such as [Assertion_failure _].

   It is up to the user to decide how thorough (and how costly) the
   well-formedness check should be. Checking that the candidate data structure
   seems well-formed, while ignoring the reference data structure, is a
   possibility. Checking that the candidate data structure is well-formed and
   is in agreement with the reference data structure is the most comprehensive
   check that can be performed.

   The optional parameter [var] is a base name that is used for variables of
   this abstract type.

   This specification is constructible and deconstructible.

   Because [declare_abstract_type] declares an abstract type as a side effect,
   it cannot be used under a dependent arrow [^>>]. It is recommended to use
   it at the top level only. *)
val declare_abstract_type:
  ?check: ('r -> ('c -> unit) code) ->
  ?var: string ->
  unit ->
  ('r, 'c) spec

(* -------------------------------------------------------------------------- *)

(* Base types. *)

(**[unit] represents the base type [unit].

   This specification is constructible and deconstructible. *)
val unit: (unit, unit) spec

(**[bool] represents the base type [bool].

   This specification is constructible and deconstructible. *)
val bool: (bool, bool) spec

(**[int] represents the basic deconstructible type [int].

   This specification is deconstructible. It is {i not} constructible, because
   it usually does not make sense to generate an integer in the huge interval
   [\[min_int, max_int\]]. *)
val int: (int, int) spec

(**[int_within range] is the basic type [int], equipped with the generator
   [range].

   This specification is constructible and deconstructible. *)
val int_within: int gen -> (int, int) spec

(**[semi_open_interval i j] is [int_within (Gen.semi_open_interval i j)]. *)
val semi_open_interval: int -> int -> (int, int) spec

(**[closed_interval i j] is [int_within (Gen.closed_interval i j)]. *)
val closed_interval: int -> int -> (int, int) spec

(**[lt j] is [int_within (Gen.lt j)]. *)
val lt: int -> (int, int) spec

(**[le j] is [int_within (Gen.le j)]. *)
val le: int -> (int, int) spec

(**[sequential()] is [int_within (Gen.sequential())]. *)
val sequential: unit -> (int, int) spec

(**[exn] represents the base type [exn].

   This specification is deconstructible, but not constructible. *)
val exn: (exn, exn) spec

(**[override_exn_eq f] overrides the notion of equality that is associated
   with the type [exn]. This notion of equality is used to compare an
   exception raised by the reference implementation with an exception raised
   by the candidate implementation. By default, it is OCaml's generic equality
   [(=)], which means that both implementations must raise exactly the same
   exceptions. [override_exn_eq] allows relaxing this requirement. The
   function [f] receives the current notion of equality as an argument and is
   expected to return a new notion of equality, which is installed in place of
   the previous one. *)
val override_exn_eq: ((exn -> exn -> bool) -> (exn -> exn -> bool)) -> unit

(**[ignored] describes a result that should be ignored.

   This specification is deconstructible, but not constructible. *)
val ignored: ('r, 'c) spec

(* -------------------------------------------------------------------------- *)

(* Combinators for products and sums. *)

(**[***] is the pair type constructor.

   When applied to constructible specifications,
   it produces a constructible specification.
   When applied to deconstructible specifications,
   it produces a deconstructible specification. *)
val ( *** ):
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec ->
  ('r1 * 'r2, 'c1 * 'c2) spec

(**[option] is the option type constructor.

   When applied to a constructible specification,
   it produces a constructible specification.
   When applied to a deconstructible specification,
   it produces a deconstructible specification. *)
val option:
  ('r, 'c) spec ->
  ('r option, 'c option) spec

(**[result] is the result type constructor.

   When applied to constructible specifications,
   it produces a constructible specification.
   When applied to deconstructible specifications,
   it produces a deconstructible specification. *)
val result:
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec ->
  (('r1, 'r2) result, ('c1, 'c2) result) spec

(**[list ~length] is the list type constructor.

   When applied to a constructible specification, it produces a constructible
   specification. When applied to a deconstructible specification, it produces
   a deconstructible specification.

   When this specification is used in construction mode, the generator
   [length] controls the length of the lists that are generated. The [length]
   parameter is optional; if is omitted, [Gen.lt 16] is used. When this
   specification is used in deconstruction mode, the [length] parameter is
   irrelevant. *)
val list:
  ?length: int gen ->
  ('r, 'c) spec ->
  ('r list, 'c list) spec

(* -------------------------------------------------------------------------- *)

(* Nondeterminism. *)

(**Using the combinator {!val:nondet} allows the reference implementation to
   have access to the candidate result of type ['c] produced by the candidate
   implementation. It must then produce a diagnostic of type ['r diagnostic].

   The diagnostic [Valid r] means that the candidate result is acceptable and
   that the corresponding reference result is [r].

   The diagnostic [Invalid cause] means that the candidate result is wrong.
   The function [cause] is an explanation why the candidate result is
   unacceptable. This function is applied to the name of a variable, such as
   [observed], which stands for the candidate result. It is expected to
   produce a piece of OCaml code that pinpoints the problem. This code could
   be an OCaml assertion that the observed result does not satisfy, such as
   [assert (observed > 0)]. It could also be just a comment, such as [(*
   candidate finds -2, whereas a positive number was expected *)]. Or, it
   could be a combination of both. The module {!module:Print} can help
   construct such OCaml code. *)
type 'r diagnostic =
  | Valid of 'r
  | Invalid of (document -> document)

(**In the common case where ['r] and ['c] are the same type, the following
   type abbreviation is useful. Then, using the combinator {!val:nondet}
   changes the type of the reference implementation from ['r] to ['r nondet].
   *)
type 'r nondet =
  'r -> 'r diagnostic

(**[nondet spec] describes the result of an operation whose specification is
   nondeterministic. It indicates that several results are permitted.
   Therefore, one cannot expect the reference implementation to produce "the"
   expected result. Instead, Monolith must run the candidate implementation
   first, and allow the reference implementation to have access to the result
   [c] produced by the candidate. The reference implementation is then
   expected to return a result of type ['r diagnostic]. If the reference
   implementation returns [Valid r], then the deconstruction process
   continues: that is, the reference result [r] and the candidate result [c]
   are deconstructed in the manner specified by [spec].

   When ['r] and ['c] are in fact the same type, the type of [nondet] can be
   written under the form [('t, 't) spec -> ('t nondet, 't) spec].

   [nondet] must be applied to a deconstructible specification,
   and produces a deconstructible specification. *)
val nondet:
  ('r, 'c) spec ->
  ('c -> 'r diagnostic, 'c) spec

(* A comment that does not appear in the generated documentation.

   One should avoid applying [nondet] to a concrete spec, as in [nondet int].
   Indeed, this specification requires Monolith to compare the integer result
   produced by the reference implementation and the integer result produced by
   the candidate implementation. This is pointless, because the reference
   implementation has already checked that the candidate result is correct,
   and has (normally) returned the same result. It is recommended to use
   [nondet ignored] instead, so as to suppress the redundant equality check.
   *)

(* -------------------------------------------------------------------------- *)

(* Arrows. *)

(**[^>] is the ordinary arrow combinator. It describes a function of one
   argument. By using it several times, one can also describe curried
   functions of several arguments, which are common in OCaml. This combinator
   imposes the absence of exceptions: the reference and candidate
   implementations are expected to not raise any exception.

   In an application [domain ^> codomain], the specification [domain] must be
   constructible, while [codomain] must be either a function specification or
   deconstructible. The specification [domain ^> codomain] is neither
   constructible nor deconstructible. *)
val (^>):
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec ->
  ('r1 -> 'r2, 'c1 -> 'c2) spec

(**[^?>] is the nondeterministic arrow combinator. [spec1 ^?> spec2] is a
   short-hand for [spec1 ^> nondet spec2].

   The (de)constructibility constraints are the same as with an ordinary arrow
   [(^>)]. *)
val (^?>):
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec ->
  ('r1 -> 'c2 -> 'r2 diagnostic, 'c1 -> 'c2) spec

(**[^!>] is the exceptional arrow combinator. It describes a function that may
   raise an exception, and it requests that this exception be caught and
   observed. This implies that the reference and candidate implementations are
   expected to raise an exception under the same circumstances, and are
   expected to raise comparable exceptions, according to the notion of
   equality that exists at type [exn]. This notion of equality can be
   customized by using {!val:override_exn_eq}.

   The (de)constructibility constraints are the same as with an ordinary arrow
   [(^>)]. *)
val (^!>):
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec ->
  ('r1 -> 'r2, 'c1 -> 'c2) spec

(**[^!?>] is an arrow combinator that combines the exception effect and the
   nondeterminism effect. It describes a function that may raise an exception.
   Furthermore, it allows nondeterminism: the candidate implementation is
   allowed to decide whether it wishes to return normally or to raise an
   exception; it is also allowed to decide what exception it wishes to raise.
   To deal with this flexibility, the behavior of the candidate implementation
   is reified as a value of type [('c2, exn) result], which the reference
   implementation receives as an argument. The reference implementation is
   then expected to either accept or reject this candidate behavior, which it
   does by returning a diagnostic. If it decides to accept this behavior, then
   it must return its own behavior as a value of type [('r2, exn) result]. The
   reference implementation must never raise an exception.

   The (de)constructibility constraints are the same as with an ordinary arrow
   [(^>)]. *)
val (^!?>):
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec ->
  ('r1 -> ('c2, exn) result -> ('r2, exn) result diagnostic, 'c1 -> 'c2) spec

(**[^>>] is the dependent arrow constructor. It describes a function of one
   argument and allows naming (the reference side of) this argument, so that
   this argument can be referred to in the codomain.

   For example, the specification of a [get] function in a sequence might be
   [seq ^>> fun s -> lt (length s) ^> element]. The first argument, a
   sequence, receives the name [s]. This allows specifying that the second
   argument, an integer, must lie in the semi-open interval [\[0, length s)].
   Here, the variable [s] denotes the data structure built by the reference
   implementation, so [length] must be the reference-side function that maps a
   sequence to its length.

   The (de)constructibility constraints are the same as with an ordinary arrow
   [(^>)]. *)
val (^>>):
  ('r1, 'c1) spec ->
  ('r1 -> ('r2, 'c2) spec) ->
  ('r1 -> 'r2, 'c1 -> 'c2) spec

(* -------------------------------------------------------------------------- *)

(* Subset. *)

(**[%] is the subset constructor. It restricts the set of arguments that can
   be passed to an operation; in other words, it expresses a precondition. For
   example, to express the fact that the operation [pop] must be applied to a
   nonempty stack, one can use [nonempty % stack], where the reference-side
   function [nonempty] tests whether a stack is nonempty, and [stack] is the
   abstract type of stacks.

   [%] must be applied to a constructible specification, and produces a
   constructible specification. *)
val (%): ('r -> bool) -> ('r, 'c) spec -> ('r, 'c) spec

(* -------------------------------------------------------------------------- *)

(* Transformations. *)

(**[map_outof] specifies that a transformation must be applied to a value. The
   user must provide the reference side of the transformation, the candidate
   side of the transformation, and a specification of the input type of the
   transformation. It is typically used to transform an argument before
   passing it to an operation.

   [map_outof] must be applied to a constructible specification, and produces
   a constructible specification. *)
val map_outof:
  ('r1 -> 'r2) ->
  ('c1 -> 'c2) code ->
  ('r1, 'c1) spec ->
  ('r2, 'c2) spec

(**[map_into] specifies that a transformation must be applied to a value. The
   user must provide the reference side of the transformation, the candidate
   side of the transformation, and a specification of the output type of the
   transformation. It is typically used to transform the result of an
   operation, or an operation itself.

   [map_into] must be applied to a deconstructible specification, and produces
   a deconstructible specification. *)
val map_into:
  ('r1 -> 'r2) ->
  ('c1 -> 'c2) code ->
  ('r2, 'c2) spec ->
  ('r1, 'c1) spec

(* -------------------------------------------------------------------------- *)

(* Rearranging arguments. *)

(**[flip] exchanges the first two arguments of a curried function. *)
val flip:
  ('r1 -> 'r2 -> 'r3, 'c1 -> 'c2 -> 'c3) spec ->
  ('r2 -> 'r1 -> 'r3, 'c2 -> 'c1 -> 'c3) spec

(**[rot2] moves the second argument of a curried function to the first
   position. It is synonymous with [flip]. *)
val rot2:
  ('r1 -> 'r2 -> 'r3, 'c1 -> 'c2 -> 'c3) spec ->
  ('r2 -> 'r1 -> 'r3, 'c2 -> 'c1 -> 'c3) spec

(**[rot3] moves the third argument of a curried function to the first
   position. It is synonymous with [flip]. *)
val rot3:
  ('r3 -> 'r1 -> 'r2 -> 'r4, 'c3 -> 'c1 -> 'c2 -> 'c4) spec ->
  ('r1 -> 'r2 -> 'r3 -> 'r4, 'c1 -> 'c2 -> 'c3 -> 'c4) spec

(**[curry] transforms a function that expects a pair into a function that
   expects two separate arguments. *)
val curry:
  ('r1 -> 'r2 -> 'r3, 'c1 -> 'c2 -> 'c3) spec ->
  ('r1 * 'r2 -> 'r3, 'c1 * 'c2 -> 'c3) spec

(**[uncurry] transforms a function that expects two separate arguments into a
   function that expects a pair. *)
val uncurry:
  ('r1 * 'r2 -> 'r3, 'c1 * 'c2 -> 'c3) spec ->
  ('r1 -> 'r2 -> 'r3, 'c1 -> 'c2 -> 'c3) spec

(* -------------------------------------------------------------------------- *)

(**The conditional specification [ifpol neg pos] is interpreted as the
   specification [neg] when it is used in construction mode (that is, when it
   describes the input of an operation) and as the specification [pos] when it
   is used in deconstruction mode (that is, when it describes the output of
   an operation).

   [ifpol] is a low-level combinator that is typically used to define
   higher-level abstractions.

   In an application [ifpol neg pos], the specification [neg] must be
   constructible, the specification [pos] must be deconstructible, and the
   result is both constructible and deconstructible. *)
val ifpol: ('r, 'c) spec -> ('r, 'c) spec -> ('r, 'c) spec

(* -------------------------------------------------------------------------- *)

(**[fix] builds a recursive specification. *)
val fix: (('r, 'c) spec -> ('r, 'c) spec) -> ('r, 'c) spec

(* -------------------------------------------------------------------------- *)

(**The function call [declare_semi_abstract_type spec] declares a new
   abstract type [t] and equips it with one operation, namely a
   one-way conversion from [t] to [spec], whose implementation is the
   identity function.

   This is typically used to disguise a function type as an abstract
   type. For instance, if an operation has type [a -> b -> c] and if
   one wishes Monolith to perform a partial application, then one
   should declare [b ^> c] as a semi-abstract type [t] and use the
   specification [a ^> t].

   [declare_semi_abstract_type] must be applied to a deconstructible
   specification, and produces a deconstructible specification.

   Because [declare_semi_abstract_type] declares an abstract type as a
   side effect, it cannot be used under a dependent arrow [^>>]. It is
   recommended to use it at the top level only. *)
val declare_semi_abstract_type: ('r, 'c) spec -> ('r, 'c) spec

(* -------------------------------------------------------------------------- *)

(* Sequences. *)

(**[declare_seq ~length] is a persistent sequence type constructor.

   When applied to a constructible specification, it produces a constructible
   specification. When applied to a deconstructible specification, it produces
   a deconstructible specification.

   When this specification is used in construction mode, the generator
   [length] controls the length of the sequences that are generated. The
   [length] parameter is optional; if is omitted, [Gen.lt 16] is used. When
   this specification is used in deconstruction mode, the [length] parameter
   is irrelevant.

   When this specification is used in deconstruction mode, it is treated as an
   abstract type, equipped with an operation that demands the first element of
   the sequence. It is possible for a sequence to be demanded several times.

   A sequence is not allowed to raise an exception when it is demanded: such
   an event is considered an error.

   Because [declare_seq] declares an abstract type as a side effect, it cannot
   be used under a dependent arrow [^>>]. It is recommended to use it at the
   top level only. *)
val declare_seq:
  ?length: int gen ->
  ('r, 'c) spec ->
  ('r Seq.t, 'c Seq.t) spec

(**[declare_affine_seq ~length] is an affine sequence type constructor.

   When applied to a constructible specification, it produces a constructible
   specification. When applied to a deconstructible specification, it produces
   a deconstructible specification.

   When this specification is used in construction mode, the generator
   [length] controls the length of the sequences that are generated. The
   [length] parameter is optional; if is omitted, [Gen.lt 16] is used. When
   this specification is used in deconstruction mode, the [length] parameter
   is irrelevant.

   When this specification is used in deconstruction mode, it is treated as an
   abstract type, equipped with an operation that demands the first element of
   the sequence. An affine sequence is demanded at most once.

   A sequence is not allowed to raise an exception when it is demanded: such
   an event is considered an error.

   Because [declare_affine_seq] declares an abstract type as a side effect, it
   cannot be used under a dependent arrow [^>>]. It is recommended to use it
   at the top level only. *)
val declare_affine_seq:
  ?length: int gen ->
  ('r, 'c) spec ->
  ('r Seq.t, 'c Seq.t) spec

(* -------------------------------------------------------------------------- *)

(**{1:engine Engine} *)

(**Monolith's engine offers a very small number of operations. A typical
   usage scenario involves first calling {!val:declare} once for each
   operation of the library under test, then invoking {!val:main} so as to
   parse the command line and start the engine. *)

(**[declare name spec reference candidate] declares the existence of an
   operation. Its parameters are the name of this operation, its
   specification, and its implementations on the reference side and on the
   candidate side.

   [declare] can be called either before [main] is invoked or from the
   function [prologue] that is passed as an argument to [main]. It cannot be
   used under a dependent arrow [^>>]. *)
val declare: string -> ('r, 'c) spec -> 'r -> 'c -> unit

(**[main fuel] sets up and starts the engine.

   [main] parses the command line and sets up the engine to draw data from the
   file whose name is supplied on the command line. If no file name is
   supplied, data is drawn from [/dev/urandom].

   The parameter [fuel] is the maximum length of a run (expressed as a number
   of operations). A small value, such as 5, 10, or 20, is typically used.

   An optional [prologue] can be supplied. If present, the function [prologue]
   is invoked once at the beginning of each run. It may invoke data generation
   functions in the module [Gen], declare operations, and produce output using
   [dprintf]. Its purpose is to determine the global parameters of the run, if
   desired. For instance, if the library under test is a bounded stack and
   takes the form of a functor that expects a bound [n], then the prologue can
   choose a value of [n], apply the functor, and declare the operations thus
   obtained. The demo
   {{:https://gitlab.inria.fr/fpottier/monolith/-/blob/master/demos/working/stack_prologue/Main.ml}[demos/working/stack_prologue]}
   illustrates this. *)
val main: ?prologue:(unit -> unit) -> int -> unit

(**[dprintf] is analogous to [printf]. Its output is actually printed to the
   standard output channel only if a scenario that leads to a problem is
   discovered. In that case, it is printed at the beginning of the scenario.

   This can be exploited, for instance, to print a number of global settings
   that have been made in the prologue.

   [dprintf] can be called either before [main] is invoked or from the
   function [prologue] that is passed as an argument to [main]. It cannot be
   used under a dependent arrow [^>>]. *)
val dprintf: ('a, Buffer.t, unit) format -> 'a

(**The exception [PleaseBackOff] can be raised by the reference implementation
   of an operation to indicate that this operation (or this particular choice
   of arguments for this operation) should not be exercised. The reason could
   be that it is not permitted, or that it has not yet been implemented. This
   exception causes Monolith to silently back off and try another operation.

   The reference implementation must not perform any side effect before
   raising this exception. *)
exception PleaseBackOff

(**The exception [Unimplemented] can be raised by the candidate implementation
   of an operation to indicate that this operation (or this particular choice
   of arguments for this operation) should not be exercised. This exception
   causes Monolith to silently abandon the current scenario and investigate
   other scenarios. *)
exception Unimplemented

(* -------------------------------------------------------------------------- *)

(**{1:support Support} *)

(**The submodule [Support] offers a number of useful functions. The name of
   these functions can appear in an error scenario printed by Monolith. *)

(* In order to find out which of these functions may be mentioned in an
   error scenario, grep for applications of the functions [support] and
   [Ops.declare]. *)

module Support : sig

  module Fun : sig

    (**The identity function. *)
    val id: 'a -> 'a

    (**Moving the second argument to the first position. *)
    val rot2: ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)

    (**Moving the third argument to the first position. *)
    val rot3: ('a -> 'b -> 'c -> 'd) -> ('c -> 'a -> 'b -> 'd)

  end

  module List : sig

    (**Testing two lists for equality. *)
    val equal: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
      (* currently never mentioned in an error scenario *)

    (**Converting a list to a sequence. *)
    val to_seq: 'a list -> 'a Seq.t

  end

  module Exn : sig

    (**Catching all exceptions. *)
    val handle: ('a -> 'b) -> 'a -> ('b, exn) result

  end

  module Seq : sig

    (**This exception is raised by [oneshot] and by several other functions
       that internally depend on [oneshot]. *)
    exception ForcedTwice

    (**[oneshot] transforms a function into a one-shot function.
       A one-shot function raises [ForcedTwice] if it is invoked twice. *)
    val oneshot: ('a -> 'b) -> 'a -> 'b
      (* currently never mentioned in an error scenario *)

    (**[affine] transforms a sequence into an affine sequence.
       An affine sequence raises [ForcedTwice] if it is forced twice. *)
    val affine: 'a Seq.t -> 'a Seq.t
      (* currently never mentioned in an error scenario *)

    (**[to_option] forces a sequence and converts its head to an option. *)
    val to_option: 'a Seq.t -> ('a * 'a Seq.t) option

    (**[list_to_affine_seq] transforms a list into an affine sequence.
       An affine sequence raises [ForcedTwice] if forced twice. *)
    val list_to_affine_seq: 'a list -> 'a Seq.t

  end

end

(*  LocalWords:  PPrint nondet deconstructible deconstructible resizeable exn *)
(*  LocalWords:  Gen.sequential exn seq map_outof uncurry ifpol oneshot *)
(*  LocalWords:  dprintf section:gen parenthesized customized *)
(*  LocalWords:  well-formedness *)
