type +'a t

type hash = int
type fields = (hash * Obj.t) array

module Internal : sig
  (** They are unsafe. Manual use must be avoided. *)
    
  val get : 'a t -> hash -> 'o
  val set : 'a t -> hash -> 'o -> unit
  val copy_with : fields -> 'a t option -> 'a t
  val create : fields -> 'a t
end
  

