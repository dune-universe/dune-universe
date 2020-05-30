module Stdlib : sig
  (** {6 Printf style errors } *)
  
  val failwithf    : ('a, unit, string, 'b) format4 -> 'a
  
  val invalid_argf : ('a, unit, string, 'b) format4 -> 'a
  
  val raisef : (string -> exn) -> ('a, unit, string, 'b) format4 -> 'a
  
  (** {6 Exception handling } *)
  
  exception Finally of exn * exn
  
  val protect     : ('a -> 'b) -> 'a -> finally: ('a -> unit) -> 'b
  (** It raises an exception [Finally (org, final)] when [finally] cannot recover the error. *)
   
  val protect_with : ('a -> 'b) -> 'a -> finally: ('a -> 'c) -> 'b * 'c
  (** It raises an exception [Finally (org, final)] when [finally] cannot recover the error. *)
   
  val catch       : ('a -> 'b) -> 'a -> ('b, [> `Exn of exn]) result
  val try_ignore  : ('a -> unit) -> 'a -> unit
  val try_or      : ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b
  val try_bool    : ('a -> unit) -> 'a -> bool
  (** [true] at success *)
  
  val protect_      : (unit -> 'b) -> finally: (unit -> unit) -> 'b
  val protect_with_ : (unit -> 'b) -> finally: (unit -> 'c) -> 'b * 'c
  val catch_        : (unit -> 'b) -> ('b, [> `Exn of exn]) result
  val try_ignore_   : (unit -> unit) -> unit
  val try_or_       : (unit -> 'b) -> (unit -> 'b) -> 'b
  val try_bool_     : (unit -> unit) -> bool
  
  val tee : ('a -> 'b) -> 'a -> handler:(exn -> unit) -> 'b
  (** [tee f v ~handler]. If [f v] raises an exception [e], 
      [handler e] is executed, then [e] is reraised.
  *)
  
  type 'a return = { return : 'jump . 'a -> 'jump }
  val with_return : ('a return -> 'a) -> 'a
end

include (module type of struct include Stdlib end)
      
(* Printexc + alpha *)

val to_string : exn -> string
val format : Format.formatter -> exn -> unit
val print_backtrace : out_channel -> unit
val get_backtrace : unit -> string
val register_printer : (exn -> string option) -> unit
