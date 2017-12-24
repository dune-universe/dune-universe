(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

(** This module provides some useful modules or functions *)

(** [StringSet] is a module for sets of [strings] *)
module StringSet : Set.S with type elt = String.t
  
(** [StringMap] is module for maps from [strings] to type ['a] *)
module StringMap : Map.S with type key = String.t

(** [IntMap] is a module for maps from [int] to type ['a] *)
module IntMap : Map.S with type key = int

(** [IntSet] is a module for sets of [int] *)
module IntSet : Set.S with type elt = int
  
(** [string_of_list sep to_string [a_1;...;a_n]] returns a string made
    of the strings [to_string a_1] ... [to_string a_n] concatenated with
    the separator [sep] between each of the elements (if the list is of
    length greater than 2) *)
val string_of_list : string -> ('a -> string) -> ('a list) -> string

(** [intersperse sep [a_1;...;a_n]] returns a list where elements of
    the input list are interspersed with [sep] as in [a_1; sep; a_2;
    sep; ...; sep; a_n]. *)
val intersperse : 'a -> 'a list -> 'a list

(** [cycle n xs] returns the first [n] elements of the infinite list formed
    by cyclically repeating the elements of [xs]. Returns the empty list if
    [xs] is empty. *)
val cycle : int -> 'a list -> 'a list

(** [fold_left1 plus elems] sums up the elements in [elems] using [plus]. A
    generalization of List.fold_left from monoids to semigroups, where we
    don't have any neutral element. Assumes [elems] is non-empty. *)
val fold_left1 : ('a -> 'a -> 'a) -> 'a list -> 'a

val term_set_size : unit -> unit
val sterm_set_size : unit -> unit
val fterm_set_size : Format.formatter -> unit

val no_pp : unit -> unit

val fformat : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
val format : ('a, Format.formatter, unit) format -> 'a
val sformat : ('a, Format.formatter, unit) format -> 'a

val format_of_list : Format.formatter -> string -> ('a -> string) -> ('a list) -> unit

val blue : string -> string
val red : string -> string
val green : string -> string

(** [string_of_list_rev sep to_string [a_1;...;a_n]] returns a string
    made of the strings [to_string a_n] ... [to_string a_1]
    concatenated with the separator [sep] between each of the elements
    (if the list is of length greater than 2) *)
val string_of_list_rev : string -> ('a -> string) -> ('a list) -> string

(** [No_file (file_name,msg)] is raised when the file [file_name] is
    not found in any of the directories given to the {!Utils.find_file}
    function with the message [msg] *)
exception No_file of  (string * string)


(** [find_file f dirs] tries to find a file with the name [f] in the
    directories listed in [dirs]. If it finds it in [dir], it returns
    the full name [Filename.concat dir f]. To check in the current
    directory, add [""] to the list. It tries in the directories of
    [dirs] in this order and stops when it finds such a file. If it
    can't find any such file, raise the exception [No_file(f,msg)]
    where [msg] contains a string describing where the file [f] was
    looked for.*)
val find_file : string -> string list -> string

val (>>) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val log_iteration : (string -> unit) -> string -> unit
