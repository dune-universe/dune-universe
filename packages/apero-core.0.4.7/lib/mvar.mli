
module type MVar = sig 
  type 'a t
  val create : 'a -> 'a t
  val create_empty : unit -> 'a t
  val put : 'a t -> 'a -> unit Lwt.t
  val take : 'a t -> 'a Lwt.t
  val take_available : 'a t -> 'a option
  val is_empty : 'a t -> bool
  
  val read : 'a t -> 'a Lwt.t  
  (* [read] provides a reference to the value stored on the mvar
     that is safe for read-only operation. This allows to have
     read/write concurrency while leveraging functional data 
     structures
  *)
  val guarded : 'a t -> ('a -> ('b Lwt.t * 'a) Lwt.t) -> 'b Lwt.t
  (* [guarded] execute a function that changes the state and returns
     a result promise while ensuring the the mvar is acquired before 
     executing the function and released after even if exceptions
     are raised while running the function *)
  
  val guarded_and_then : 'a t -> ('a -> ('b Lwt.t * 'a) Lwt.t) -> ('a -> 'b Lwt.t -> 'c Lwt.t) -> 'c Lwt.t
  (* [guarded_and_then m f g] runs  f as "guarded m f" but progagates the result and the new state
    to execute g. Notice that g cannot change the state thus is a read-only operation *)

  val return : 'b -> 'a -> ('b Lwt.t * 'a) Lwt.t

  val return_lwt : 'b Lwt.t -> 'a -> ('b Lwt.t * 'a) Lwt.t
  
end

module MVar_lwt : sig 
(* This module provides an implementation based on 
Lwt_mvar implementation *)
  include MVar 
end  