external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** I prefer [(&)] to [(@@)] *)

val ( !!% ) : ('a, Format.formatter, unit) format -> 'a
(** Synonym of [Format.eprintf] *)
  
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** [flip f x y = f y x] *)

val flip2 : ('a -> 'b -> 'c -> 'd) -> 'b -> 'c -> 'a -> 'd
(** [flip2 f x y z = f y z x] *)
  
module Format : sig
  include module type of struct include Format end

  type t = formatter
      
  val sprintf : ('a, t, unit, string) format4 -> 'a
  (** [sprintf] with a better type *)
    
  val ksprintf : (string -> 'a) -> ('b, t, unit, 'a) format4 -> 'b
  (** [ksprintf] with a better type *)

  val wrapf :
    (t -> unit)
    -> (t -> 'b)
    -> t
    -> ('c, t, unit, 'b) format4 -> 'c
  (** [wrapf left right ppf fmt] executes [fprintf ppf fmt] but
      [left ppf] and [right ppf] are called before and after it respectively. *)
end
  
module Option : sig
  val map : ('a -> 'b) -> 'a option -> 'b option

  val format :
    (Format.t -> 'a -> unit) ->
    Format.t -> 'a option -> unit

  val to_list : 'a option -> 'a list
end
  
module List : sig
  include module type of struct include List end

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
  val assoc_opt : 'a -> ('a * 'b) list -> 'b option

  val partition_map : 
    ('a -> [< `Left of 'b | `Right of 'c ]) -> 'a list -> 'b list * 'c list

  val format :
    (unit, Format.t, unit) format ->
    (Format.t -> 'a -> unit) -> Format.t -> 'a list -> unit

  val from_to : int -> int -> int list
end
  
module String : sig
  include module type of struct include String end

  val is_prefix : t -> t -> bool
end
  
module Hashtbl : sig
  include module type of struct include Hashtbl end

  val to_list : ('a, 'b) t -> ('a * 'b) list
end

module Filename : sig
  include module type of struct include Filename end

  val split_extension : string -> string * string
  (** [split_extension "foo.bar" = "foo", ".bar"]
      [split_extension "foobar" = "foobar", ""]
  *)
end
  
val protect : (unit -> 'a) -> ('a, exn) result
(** Catch the exceptions *)
  
val unprotect : ('a, exn) result -> 'a
(** Reraise the caught exception *)

val warnf : ('a, Format.t, unit, unit) format4 -> 'a
(** Wraps formatting between ["@[Warning:@ "] and ["@]@."] *)
