(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

module List : sig
  include module type of struct include List end
  val find_map_opt : ('a -> 'b option) -> 'a list -> 'b option
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
end

module Debug : sig
  val on : bool ref
  val format : ('a, Format.formatter, unit) format -> 'a
end

module Lazy : sig
  include module type of struct include Lazy end
  module Open : sig 
    val ( !! ) : 'a Lazy.t -> 'a 
    val eager : 'a -> 'a Lazy.t 
  end
  val peek : 'a t -> 'a option
  val apply : ('a -> 'b) -> 'a t -> 'b Lazy.t
  val is_val : 'a t -> bool
end
include module type of struct include Lazy.Open end

module String : sig
  include module type of struct include String end
  val split : (char -> bool) -> string -> string list

  val sub' : string -> int -> int -> string
  (** Same as [String.sub] but even if the string shorter for [len] 
      the function succeeds and returns a shorter substring. 
  *)

  val split_at : string -> int -> string * string
  (** Get prefix and postfix. May raise an error when out of bound. *)

  val find : string -> int -> (char -> bool) -> int option

  val replace_chars : char -> char -> string -> string
end

module Filename : sig
  include module type of struct include Filename end
  val split_extension : string -> string * string

    val concats : string list -> string

  module Open : sig
    val (^/) : string -> string -> string
  end
end
include module type of struct include Filename.Open end

module Format : sig
  include module type of struct include Format end
  val list :
    (unit, formatter, unit) format (* seprator *)
    -> (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  val option :
    (formatter -> 'a -> unit) ->
    formatter -> 'a option -> unit
  val lazy_ :
    (formatter -> 'a -> unit) ->
    formatter -> 'a Lazy.t -> unit
end

module Option : sig
  val map : f:('a -> 'b) -> 'a option -> 'b option
  val bind : 'a option -> ('a -> 'b option) -> 'b option
  val iter : f:('a -> unit) -> 'a option -> unit
  val default : 'a option -> (unit -> 'a) -> 'a 
end

exception Finally of exn * exn

val protect : f:('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b
val protect' : string -> ('a -> 'b) -> ('a -> 'b)
val catch : f:('a -> 'b) -> 'a -> [> `Error of exn | `Ok of 'b ]
val failwithf : ('a, unit, string, 'b) format4 -> 'a
val invalid_argf : ('a, unit, string, 'b) format4 -> 'a
val with_ref : 'a ref -> 'a -> (unit -> 'b) -> 'b

module Unix : sig
  include module type of struct include Unix end
  val kind : string -> file_kind option
  val is_dir : string -> bool
  val gen_timed : (unit -> 't) -> ('t -> 't -> 't) -> ('a -> 'b) -> 'a -> 'b * 't
  val timed : ('a -> 'b) -> 'a -> 'b * float
  val dev_inode : string -> (int * int) option
  val same_file : string -> string -> bool
  module Process_times : sig
    type t = process_times
    val (-) : t -> t -> t
    val timed : ('a -> 'b) -> 'a -> 'b * t
  end
end

module Find : sig
  open Unix
  type path = 
      { dir : string;
	base : string;
	path : string; (* dir / name *)
	stat : [ `Ok of stats | `Error of exn ];
	depth : int;
      }

  val prune : unit -> unit
  val find : f:(path -> unit) -> string list -> unit
  val kind : path -> file_kind option
  val is_dir : path -> bool
end

module Hashtbl : sig
  include module type of struct include Hashtbl end
  val of_list : int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
  val memoize : ('a, 'b) Hashtbl.t -> ('a -> 'b) -> 'a -> 'b
  val find_default : 'b -> ('a, 'b) Hashtbl.t -> 'a -> 'b
  val multi_add : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
end

module Hashset : sig
  (* poorman's hashset by hashtbl *)
  
  type 'a t
  val create : ?random:bool -> int -> 'a t
  val add : 'a t -> 'a -> unit
  val remove : 'a t -> 'a -> unit
  val mem : 'a t -> 'a -> bool

  val find : 'a t -> 'a -> 'a
  (** good for hash consing *)

  val find_opt : 'a t -> 'a -> 'a option
  (** good for hash consing *)

  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val elements : 'a t -> int
  val clear : 'a t -> unit
  val of_list : int -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
end

external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"
