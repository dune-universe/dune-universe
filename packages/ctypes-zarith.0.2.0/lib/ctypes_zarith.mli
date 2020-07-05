module MPZ : sig

  type t

  type ptr = t Ctypes.abstract Ctypes.ptr

  val t : t Ctypes.abstract Ctypes.typ

  val clear : ptr -> unit

  val init : ptr -> unit

  val set : Z.t -> ptr -> unit

  val make : unit -> ptr
  (** like {!Ctypes.make}, but with finalise and type already specified.
  mpz is initialized. *)

  val of_z : Z.t -> ptr

  val to_z : ptr -> Z.t

  val zarith : Z.t Ctypes.typ

  val t_ptr : ptr Ctypes.typ

end

module MPQ : sig

  type t

  type ptr = t Ctypes.abstract Ctypes.ptr

  val t : t Ctypes.abstract Ctypes.typ

  val clear : ptr -> unit

  val init : ptr -> unit

  val set  : Q.t -> ptr -> unit

  val make : unit -> ptr
  (** like {!Ctypes.make}, but with finalise and type already specified.
      mpq is initialized. *)

  val of_q : Q.t -> ptr

  val to_q : ptr -> Q.t

  val zarith : Q.t Ctypes.typ

  val t_ptr : ptr Ctypes.typ

end
