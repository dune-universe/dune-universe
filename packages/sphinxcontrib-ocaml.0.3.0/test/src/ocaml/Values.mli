module type MT = sig
  (** Doc for v *)
  val v: int
  (** Doc' for v *)

  (** Doc for hidden *)
  val hidden: int [@@autodoc.hide]
  (** Doc' for hidden *)

  (** Doc for (+): there should be parentheses *)
  val (+): unit
  (** Doc' for (+) *)
end

include MT

(** A reference to a value: :val:`Values.v`. *)
