open Owl_ode.Types

module Make (M : Owl_types_ndarray_algodiff.Sig with type elt = float) : sig
  type f_t = M.arr * M.arr -> float -> M.arr

  val contact1_damped_s
    :  a:(float -> float)
    -> f_t
    -> dt:float
    -> M.arr * M.arr
    -> float
    -> (M.arr * M.arr) * float

  val contact2_damped_s
    :  a:(float -> float)
    -> f_t
    -> dt:float
    -> M.arr * M.arr
    -> float
    -> (M.arr * M.arr) * float
end

module S : sig
  type mat = Owl_dense_matrix_s.mat

  module Contact1_damped (A : sig
    val a : float -> float
  end) :
    Solver
      with type state = mat * mat
       and type f = mat * mat -> float -> mat
       and type step_output = (mat * mat) * float
       and type solve_output = mat * mat * mat

  module Contact2_damped (A : sig
    val a : float -> float
  end) :
    Solver
      with type state = mat * mat
       and type f = mat * mat -> float -> mat
       and type step_output = (mat * mat) * float
       and type solve_output = mat * mat * mat
end

module D : sig
  type mat = Owl_dense_matrix_d.mat

  module Contact1_damped (A : sig
    val a : float -> float
  end) :
    Solver
      with type state = mat * mat
       and type f = mat * mat -> float -> mat
       and type step_output = (mat * mat) * float
       and type solve_output = mat * mat * mat

  module Contact2_damped (A : sig
    val a : float -> float
  end) :
    Solver
      with type state = mat * mat
       and type f = mat * mat -> float -> mat
       and type step_output = (mat * mat) * float
       and type solve_output = mat * mat * mat
end
