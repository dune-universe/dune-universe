(* This file is part of ocaml-crontab.
 *
 * Copyright (C) 2019 Yann Régis-Gianas
 *
 * ocaml-crontab is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Cron is a task scheduler standardized by POSIX. *)

type t
(** The type for cron table. *)

type entry
(** The type of cron table entry. *)

val entries : t -> entry list
(** [entries crontab] observes [crontab] entries. *)

val make : entry list -> t
(** [make entries] builds a contrab from a list of entries. *)

type field =
  | All
  (** [All] is equivalent to "*". *)
  | List of element list
  (** [Enumerate [1; 3; 9]] is equivalent to "1,3,9" *)
and element = private
  | Single of int
  (** [Single k] is the singleton value "k". *)
  | Range of int * int
  (** [Range (a, b)] is the range "a-b". *)

type validator = int -> unit
(** Not all integers are valid.
    A validator makes sure only valid values are used. *)

val valid_minute            : validator
(** A minute is valid if in [0..59]. *)

val valid_hour              : validator
(** An hour is valid if in [0..23]. *)

val valid_day_of_the_month  : validator
(** A day of the month is valid if in [1.31]. *)

val valid_day_of_the_week   : validator
(** A day of the week is valid if in [0..6]. *)

val valid_month_of_the_year : validator
(** A month of the year is valid if in [1..12]. *)

val single : validator -> int -> element
(** Build a single valid element. *)

val range  : validator -> int -> int -> element
(** Build a range of valid elements. *)

exception InvalidElement of string * int

val make_entry:
  ?minute:field ->
  ?hour:field ->
  ?day_of_the_month:field ->
  ?month_of_the_year:field ->
  ?day_of_the_week:field ->
  string ->
  entry
(** [make_entry command] creates a crontab entry executing [command].
    @param minute must be in [0..59].
    @param hour must be in [0..23].
    @param day_of_the_month must be in [1.31].
    @param month_of_the_year must be in [1..12].
    @param day_of_the_week must be in [0..6] with 0 = Sunday.
    @raise InvalidEntryField
    if some input does not conform to the previous constraint.
    For all field, the default value is [All].
*)

val minute : entry -> field
(** [minute entry] returns an integer in [0..59].
    [None] means [Any]. *)

val hour : entry -> field
(** [hour entry] returns an integer in [0..23].
    [None] means [Any]. *)

val day_of_the_month : entry -> field
(** [day_of_the_month entry] returns an integer in [1..31].
    [None] means [Any].  *)

val month_of_the_year : entry -> field
(** [month_of_the_year entry] returns an integer in [0..11].
    [None] means [Any]. *)

val day_of_the_week : entry -> field
(** [day_of_the_week entry] returns an integer in [0..6].
    [None] means [Any]. *)

exception ParseError of int * string

val entry_of_string: string -> entry
(** [entry_of_string line] turns a crontab line into an entry.
    @raise ParseError if [line] does not conform to POSIX. *)

val string_of_entry: entry -> string
(** [string_of_entry entry] produces a textual representation of
    [entry] conforming to the POSIX standard.
    @raise ParseError if [line] does not conform to POSIX. *)

val crontab_of_string : string -> t
(** [crontab_of_string input] reads [input] as a crontab is possible.
   @raise ParseError in case of syntax error. *)

val string_of_crontab : t -> string
(** [string_of_crontab crontable] produces a textual representation of
   [crontable] conforming to the POSIX standard. *)

exception CrontabError of Unix.process_status

val crontab_install : ?user:string -> t -> unit
(** [crontab_install crontable] installs the [crontable].
    @param user is the current user by default.
    @raise CrontabError if something went wrong. *)

val crontab_get : ?user:string -> unit -> t
(** [crontab_list ()] returns the currently installed cron table.
    @param user is the current user by default. *)

val crontab_insert_entry : ?user:string -> entry -> unit
(** [crontab_insert_entry entry] installs a new [entry] in the
    current crontab, if it is not already there. *)

val crontab_remove_entry : ?user:string -> entry -> unit
(** [crontab_remove_entry entry] removes an [entry] in the current
   crontab.
   @raise Not_found if it does not exist. *)

val crontab_remove : ?user:string -> unit -> unit
(** [crontab_remove ()] removes the currently installed cron table.
    @param user is the current user by default. *)

val version : string
(** This library version. *)
