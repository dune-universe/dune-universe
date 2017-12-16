module Focused_list :
sig
  type 'a t= List_zip of ('a list*'a*'a list)
  (** This type aims to implement a zipper for lists. The context is
      the first parameter. It represents the list of the elements {e
      in reverse order} that has been traversed to reach the focused
      element (the second parameter). The last element is the
      remaining elements of the list. *)

  exception Empty_list
  exception End_of_list

  (** [init l] inits a focused list. It raises an exception {!
      Empty_list} if [l] is empty, hence has no element to
      focus on. *)
  val init : 'a list -> 'a t

  (** [forward z] returns a the new focused_list where the focuses
      element is the next one in the initial list. It raises
      {!End_of_list} if no such element is available *)
  val forward : 'a t -> 'a t

  (** [backward z] returns a the new focused_list where the focuses
      element is the previous one in the initial list. It raises
      {!End_of_list} if no such element is available *)
  val backward : 'a t -> 'a t

  (** [fold f a z] returns [f (... (f (f a z1) z2) ...) z_n] where
      [z_1=z] and [z_{i+1}=forward z_i] and [forward z_n] would raise
      an exception {!End_of_list}. *)
  val fold : ('b -> ('a list*'a*'a list) -> 'b) -> 'b -> 'a t -> 'b
end
