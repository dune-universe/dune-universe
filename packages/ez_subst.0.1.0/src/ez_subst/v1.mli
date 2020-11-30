(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 OCamlPro & Origin Labs                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

module EZ_SUBST : sig

  (** Easy Substitutions in Strings

      [ez_subst] provides simple functions to perform substitutions of
     expressions in strings. By default, expressions are recognized as
     [${expr}] ([brace] substitution), [$(expr)] ([paren]
     substitution), [$\[expr\]] ([bracket] substitution) and
     [$azAZ_09] ([var] substitution), but it can be further customized
     by:

      - changing the separator [sep] (default is ['$'])

      - using a symmetric notation [sym] (default is [false], whereas
     [true] means ['${ident}$'].)

      Recursion is allowed in substitutions, i.e. "$(x$(y))" will
     perform first the substitution "$(y)", returning "z" for example,
     and then "$(xz)".

      Escaping is done using '\\', i.e. any character preceeded by a
     backslash is printed as itself, and not interpreted as a
     beginning or ending separator for expression. Escaping can be
     controled using the [escape] argument, a reference that can be
     turned to [true] or [false] even during the substitution.

      If [fail] is [true] (default), [string] and [buffer] will raise
     an exception [UnclosedExpression] if they cannot match the end of
     an expression, [string_from_list] will fail with
     [UnknownExpression] if a substitution is not found. If [fail] is
     false, they will try to substitute the broken expression or just
     replace it by itself (without the parens).

      Examples:

{v open Ez_subst.V1 (* versionned interface *)

      let s = EZ_SUBST.string ~brace:(fun ctxt n -> string_of_int
     (ctxt + int_of_string n)) ~ctxt:3 "${4} ${5}"

      let s = EZ_SUBST.string ~sep:'!'  ~paren:(fun () s ->
     String.uppercase s) ~ctxt:() "!(abc) !(def)"

      let s = EZ_SUBST.string ~sym:true ~sep:'%' ~brace:(fun ctxt_ s
     -> ctxt ^ " " ^ s) ~ctxt:"Hello" "%{John}% %{Sandy}%"

      let s = EZ_SUBST.string_from_list ~default:"unknown" [ "name",
     "Doe"; "surname", "John" ] "${name} $(surname) is missing" v} *)

  (** The type for functions performing the translation from [ident]
     to its replacement. ['context] is some information, that is from
     the initial call to the substitution. *)
  type 'context t = 'context -> string -> string

  (** The only exception that may be raised by substitutions: it indicates that
      the end of the expression could not be found, unless [~fail:false]
      is specified. *)
  exception UnclosedExpression of string

  (** [string f context s] performs substitutions on [s] following
     [f], passing the context [context] to [f] for every expression,
     returning the result as a string. *)
  val string :
    ?sep:char ->
    ?sym:bool ->
    ?fail:bool ->
    ?escape:bool ref ->
    ?brace:'context t ->
    ?paren:'context t ->
    ?bracket:'context t ->
    ?var:'context t ->
    ctxt: 'context ->
    string ->
    string

  (** [buffer f b context s] performs substitutions on [s] following
     [f], passing the context [context] to [f] for every expression,
     returning the result by appending it to the buffer [b].

      Note that modifications are not atomic, so if an exception is raised
      during the substitution, the buffer might have been modified.
  *)
  val buffer :
    ?sep:char ->
    ?sym:bool ->
    ?fail:bool ->
    ?escape:bool ref ->
    ?brace:'context t ->
    ?paren:'context t ->
    ?bracket:'context t ->
    ?var:'context t ->
    ctxt: 'context ->
    Buffer.t ->
    string ->
    unit

  (** This exception can be raised by [string_from_list] if an
     expression is not found in the list, no [default] has been
     specified and [fail] is [true].  *)
  exception UnknownExpression of string

  (** Substitute expression using a list of associations. A [default]
     can be speciifed if an expression is not found in the list.
     Brace, paren, bracket and var substitutions are all activated by
     default, and can be deactivated by booleans. *)
  val string_from_list :
    ?sep:char ->
    ?sym:bool ->
    ?fail:bool ->
    ?brace:bool ->
    ?paren:bool ->
    ?bracket:bool ->
    ?var:bool ->
    ?default:string ->
    ( string * string ) list ->
    string ->
    string

end
