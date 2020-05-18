
module type Buffer = sig 
  type t

  val create : int -> t

  val reset : t -> unit

  val len : t -> int

  val sub : t -> int -> int -> t
end

module Make( B : Buffer ) : sig
  type t
  type buffer = B.t

  (** [alloc pool size] either creates a buffer of length [size] 
      or gets an existing buffer which is larger than [size] *)
  val alloc : t -> int -> B.t

  (** [release pool b] returns [b] to the buffer pool *)
  val release : t -> buffer -> unit

  (** [make ()] creates a new buffer pool. *)
  val make : unit -> t
end 
