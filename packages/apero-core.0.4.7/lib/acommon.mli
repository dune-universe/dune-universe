val identity : 'a -> 'a 
val drop : int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val some : 'b -> ('a -> 'b) -> 'a option -> 'b
val result : ('a -> 'b) -> ('c -> 'b) -> ('a, 'c) result -> 'b
val apply : ('a -> 'b) -> 'a -> 'b
val apply_n : 'a -> ('a -> 'b) -> int  -> 'b list
val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)

module Infix : sig 
  val (<.>) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
  val (%) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
  val (%>) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
  val (<*>) : ('a -> 'b) -> 'a -> 'b
end

module Astring : sig
  (** [Apero.AString] module includes and extends the [Astring.String] module -
      see its API here: {{:http://erratique.ch/software/astring/doc/Astring.String.html}[Astring.String]}
     *)

  include (module type of Astring.String)

  val contains : char -> string -> bool
  (** [String.contains c s] tests if the character [c] appears in the string [s] *)

  val contains_sub : sub:string -> string -> bool
  (** [String.contains_string sub s] tests if the substring [sub] appears in the string [s] *)

  val replace : char -> char -> string -> string
  (** [replace c c' s] replaces all occurrences of [c] in [s] with [c'] and returns the result as a new string  *)

  val after : int -> string -> string
  (** [after i s] returns the substring of [s] starting at index [i] and to the end of [s].
      If [i] is out of bound an empty string is returned *)

  val before : int -> string -> string
  (** [before i s] returns the substring of [s] starting at index 0 and ending at index [i] (the character at index [i] is not included).
      If [i] is out of bound [s] is returned *)
end

module type Monad = sig 
  type 'a m
  val return : 'a -> 'a m  
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val map : 'a m -> ('a -> 'b) -> 'b m
  val lift : ('a -> 'b) -> ('a m -> 'b m)
  val iter : 'a m -> ('a -> unit) -> unit
  (* val flatten : ('a m) m -> 'a m   *)
  module  Infix : sig
    val (<$>) : ('a -> 'b) -> ('a m -> 'b m)
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  end 
end

module Option : sig
  val bind : 'a option -> ('a -> 'b option) -> 'b option 
  val map : 'a option -> ('a -> 'b) -> 'b option 
  val return : 'a -> 'a option
  val zero : unit -> 'a option
  val is_some : 'a option -> bool
  val get : 'a option ->  'a
  val get_or_else : 'a option ->  (unit -> 'a) ->  'a
  val get_or_default : 'a option -> 'a ->  'a
  val or_else : 'a option -> (unit -> 'a option) -> 'a option
  val flatten : ('a option) list -> ('a list) option
  val iter : 'a option -> ('a -> unit) -> unit
  val lift : ('a -> 'b) -> ('a option -> 'b option)
  val lift2 : ('a -> 'b -> 'c) -> ('a option -> 'b option -> 'c option)  
  module Infix : sig 
    val (>>=) : 'a option -> ('a -> 'b option) -> 'b option 
    val (>|=) : 'a option -> ('a -> 'b) -> 'b option 
    val (>>) : 'a option -> 'b option -> 'b option 
    val (<$>) : ('a -> 'b) -> ('a option -> 'b option)
    val (<$$>) : ('a -> 'b -> 'c) -> ('a option -> 'b option -> 'c option)
    val (>==) : 'a option -> ('a -> 'b) -> 'b option
    val (>?=) : 'a option -> 'a -> 'a
  end
end

module Result : sig 
  type (+'a, +'e) t = ('a, 'e) result

  val bind : ('a, 'e) t -> ('a -> ('c, 'e) t) -> ('c,'e) t
  val bind2 : (('a * 'b), 'e) t -> ('a -> 'b -> ('c, 'e) t) -> ('c,'e) t
  val map : ('a, 'e) t -> ('a -> 'c) -> ('c,'e) t
  val bind_error : ('a, 'e) t -> ('e -> ('a, 'i) t) -> ('a,'i) t

  val fold_m : ('a -> 'b -> ('b, 'e) t) -> 'a list -> 'b -> ('b, 'e) t

  val return : 'a -> ('a, 'e) t
  val ok : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val is_ok : ('a, 'e) t -> bool
  val is_error : ('a, 'e) t -> bool
  val get : ('a, 'e) t -> 'a
  val try_get :  run:('a -> 'c) -> fail_with: ('e -> 'c) -> on:('a, 'e) t -> 'c
  val get_or_else : ('a, 'e) t -> ('e -> 'a) -> 'a
  val or_else : ('a, 'e) t  -> ('e -> ('a, 'e) t) -> ('a, 'e) t
  val flatten : (('a, 'e) t) list -> ('a list, 'e) t
  val iter : ('a, 'e) t -> ('a -> unit) -> unit
  val to_option : ('a, 'e) t -> 'a option
  val lift : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
  val cons : ('a, 'e) t -> ('a list, 'e) t -> ('a list, 'e) t 

  module Infix : sig   
    val (>>=) : ('a, 'e) t -> ('a -> ('c, 'e) t) -> ('c,'e) t 
    val (>>) : ('a, 'e) t -> ('c, 'e) t -> ('c,'e) t 
    val (>>==): (('a * 'b), 'e) t -> ('a -> 'b -> ('c, 'e) t) -> ('c,'e) t
    val (>>>) : ('a, 'e) t -> ('a -> 'c) -> ('c,'e) t
    val (>>=!) : ('a, 'e) t -> ('e -> ('a, 'i) t) -> ('a,'i) t 
    val (<$>) :  ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
  end
end

module LwtM : sig  
  include (module type of Lwt)
  val fold_m : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b list  -> 'a Lwt.t    
  val lift : ('a -> 'b) -> ('a Lwt.t -> 'b Lwt.t)
  val flatten : ('a Lwt.t) list -> ('a list) Lwt.t 
  val of_result : ('e -> exn) -> ('a, 'e) Result.t -> 'a Lwt.t
  val sequence : 'a Lwt.t -> 'b Lwt.t Lazy.t -> 'b Lwt.t
  val read_mvar : 'a Lwt_mvar.t -> 'a Lwt.t
  (* Get the value stored in the mvar and put its back providing 
     read  access to the underlying functional values. 
     NOTE: This should only be used with functional types, that
          have no side-effects *)
  module InfixM : sig 
    val (<$>) : ('a -> 'b) -> ('a Lwt.t -> 'b Lwt.t)
    val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) ->  'b Lwt.t
    val (>|=) : 'a Lwt.t -> ('a -> 'b) ->  'b Lwt.t
    val (>>) : 'a Lwt.t -> 'b Lwt.t Lazy.t -> 'b Lwt.t
    val (<&>) : unit Lwt.t -> unit Lwt.t -> unit Lwt.t 
    val (<?>) : unit Lwt.t -> unit Lwt.t -> unit Lwt.t
    val (%>>=) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
  end
end
