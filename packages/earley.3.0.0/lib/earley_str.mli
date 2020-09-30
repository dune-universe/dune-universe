(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 CNRS, Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains a parser combinator library for the OCaml lang-
  uage. It is intended to be used in conjunction with pa_ocaml (an OCaml
  parser and syntax extention mechanism) to provide  a  fully-integrated
  way of building parsers using an extention of OCaml's syntax.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute the software under the terms of the CeCILL-
  B license as circulated by CEA, CNRS and INRIA at the following URL.

      http://www.cecill.info

  As a counterpart to the access to the source code and  rights to copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty  and the software's author, the holder of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

(** Earley support for [Str] regular expressions. *)

open Earley_core.Input
open Earley_core.Earley

(** [blank_regexp re] produces a blank function  from  the  regexp  [re]
    (following the [Str] syntax). There is an important limitation rega-
    rding regular expressions containing the newline  character  ['\n'],
    due to the fact that the [Str] module only matches on  strings  (and
    not on an abstract notion of buffer). Such regular  expressions  can
    only be used if they are idempotent. *)
val blank_regexp : string -> buffer -> int -> buffer * int

(** [regexp ?name re g] is a grammar that parses the input according  to
    the regular expression [re], and returns a value built  by  applying
    the function [g] to a function of type [int -> string] that  returns
    the substring matched by the [n]-th match group of the  regexp  [re]
    (as in the [Str] module). The optional [name] argument  is  used  to
    refer to the regular expression in error messages. *)
val regexp : ?name:string -> string -> ((int -> string) -> 'a) -> 'a grammar
