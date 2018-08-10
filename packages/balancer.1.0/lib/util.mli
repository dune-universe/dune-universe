(** Utilities for concurrency and thread safety *)



module type INT = sig
  type t 

  val succ: t -> t
  val pred: t -> t
  val zero: t
              
end


                    
(** An atomic counter*)
module type COUNTER = sig
  type elt
  type t = elt ref

  val zero: unit -> t
  val create: elt -> t
                     
         
  val incr: t -> elt
  val decr: t -> elt

  val get: t -> elt
  val set: t -> elt -> elt 
end

(** Builds Atomic Counters from Integers*)                        
module Counter (I: INT): COUNTER

module Counter32: COUNTER with type elt = int32 and type t = int32 ref 
module Counter64: COUNTER with type elt = int64 and type t = int64 ref


                                            

(** Thread safe mutable that provides the gaurentees of a Multi Reader single Writer RW mutex, it handles locking and unlocking on it's own so you don't have to touch the mutex *)         
module SyncVar: sig
  type 'a t = { lock: Lwt_mutex.t; mutable value: 'a}


  (** Aquires a read lock and returns data *)
  val read: 'a t -> 'a Lwt.t



  (** Reads the inner value without a read lock for use in update and sync effect *)
  val value: 'a t -> 'a
                       
                       
  val create: 'a -> 'a t

  (** Acquires write lock and sets value*)
  val become: 'a t -> 'a -> unit Lwt.t

  (** Acquires write lock and applies f to it's contents*)
  val update: 'a t -> ('a -> 'a) -> 'a Lwt.t
                     
  val sync: 'a t -> (unit -> 'b Lwt.t) -> 'b Lwt.t
                                           
                                           
                                           

end
                   

(** Run a function that is ran after the promise is completed, whether it's successful or not*)
val ensure: 'a Lwt.t -> (unit -> unit) -> 'a Lwt.t 
