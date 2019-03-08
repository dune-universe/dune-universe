open Owl_ode.Types

module Make: functor
  (M: Owl_types_ndarray_algodiff.Sig with type elt = float) 
  -> sig 

    type f_t = M.arr -> M.arr -> float -> M.arr

    val contact1_damped_s :
      a:(float -> float) ->
      f:f_t ->
      dt:float -> 
      M.arr ->
      M.arr ->
      float ->
      M.arr * M.arr * float

    val contact2_damped_s :
      a:(float -> float) ->
      f:f_t ->
      dt:float -> 
      M.arr ->
      M.arr ->
      float ->
      M.arr * M.arr * float
  end

module S: sig
  type mat = Owl_dense_matrix_s.mat

  module Contact1_damped :
    functor (A : sig val a : float -> float end) -> 
      SolverT with type s = mat * mat
               and type t = mat
               and type output = mat * mat * mat

  module Contact2_damped :
    functor (A : sig val a : float -> float end) ->
      SolverT with type s = mat * mat
               and type t = mat
               and type output = mat * mat * mat

end

module D: sig
  type mat = Owl_dense_matrix_d.mat

  module Contact1_damped :
    functor (A : sig val a : float -> float end) -> 
      SolverT with type s = mat * mat
               and type t = mat
               and type output = mat * mat * mat

  module Contact2_damped :
    functor (A : sig val a : float -> float end) ->
      SolverT with type s = mat * mat
               and type t = mat
               and type output = mat * mat * mat

end