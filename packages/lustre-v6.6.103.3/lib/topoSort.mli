(** Time-stamp: <modified the 03/02/2016 (at 10:04) by Erwan Jahier> *)


module type PartialOrder =
  sig
    type elt
    type store
    val have_dep : store -> elt -> bool
    val find_dep : store -> elt -> elt list
    val remove_dep:store -> elt -> store
  end

module type S =
  sig
    type elt
    type store
    exception DependencyCycle of elt * elt list
    val check_there_is_no_cycle : store -> elt list -> unit
    val f : store -> elt list -> elt list
end


module Make(PO: PartialOrder) : S 
       with type elt = PO.elt
       with type store = PO.store
