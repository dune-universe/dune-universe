module MPZ : sig

  type t

  val make : unit -> t Ctypes.abstract Ctypes.ptr
  (** like {!Ctypes.make}, but with finalise and type already specified *)

  val of_z : Z.t -> t Ctypes.abstract Ctypes.ptr

  val to_z : t Ctypes.abstract Ctypes.ptr -> Z.t

  val zarith : Z.t Ctypes.typ

  val t_ptr : t Ctypes.abstract Ctypes.ptr Ctypes.typ

end

module MPQ : sig

  type t

  val make : unit -> t Ctypes.abstract Ctypes.ptr
  (** like {!Ctypes.make}, but with finalise and type already specified *)

  val of_q : Q.t -> t Ctypes.abstract Ctypes.ptr

  val to_q : t Ctypes.abstract Ctypes.ptr -> Q.t

  val zarith : Q.t Ctypes.typ

  val t_ptr : t Ctypes.abstract Ctypes.ptr Ctypes.typ

end
