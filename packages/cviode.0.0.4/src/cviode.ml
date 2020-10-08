module Make (M : Owl_types_ndarray_algodiff.Sig with type elt = float) = struct
  type f_t = M.arr * M.arr -> float -> M.arr

  module M = struct
    include M

    (* TODO: implement this in owl *)
    let ( *$ ) = M.mul_scalar
    let ( + ) = M.add
  end

  module C = Owl_ode.Common.Make (M)

  let prepare step f (x0, p0) tspec () =
    let open Owl_ode.Types in
    let tspan, dt =
      match tspec with
      | T1 { t0; duration; dt } -> (t0, t0 +. duration), dt
      | T2 { tspan; dt } -> tspan, dt
      | T3 _ -> raise Owl_exception.(NOT_IMPLEMENTED "T3 not implemented")
    in
    let step = step f ~dt in
    C.symplectic_integrate ~step ~tspan ~dt (x0, p0)


  (** {2:lib cviode library} *)
  let contact1_damped_s ~a (f : f_t) ~dt (xs, ps) t0 =
    let t = t0 +. dt in
    let c0 = 1.0 -. (dt *. a t) in
    let c1 = 0.5 *. dt in
    let fxs = f (xs, ps) t in
    let xs' = M.(xs + (ps *$ (dt *. c0)) + (fxs *$ (dt *. c1))) in
    let fxs' = f (xs', ps) t in
    let ps' = M.((ps *$ c0) + ((fxs + fxs') *$ c1)) in
    (xs', ps'), t


  let contact2_damped_s ~a (f : f_t) ~dt (xs, ps) t0 =
    let t = t0 +. dt in
    let at = a t in
    let fxs = f (xs, ps) t in
    let c0m = 1.0 -. (0.5 *. dt *. at) in
    let c0p = 1.0 +. (0.5 *. dt *. at) in
    let c1 = 0.5 *. dt in
    let xs' = M.(xs + (ps *$ (dt *. c0m)) + (fxs *$ (dt *. c1))) in
    let fxs' = f (xs', ps) t in
    let ps' = M.((ps *$ (c0m /. c0p)) + ((fxs + fxs') *$ (c1 /. c0p))) in
    (xs', ps'), t
end

module S = struct
  type mat = Owl_dense_matrix_s.mat

  include Make (Owl_algodiff_primal_ops.S)

  module Contact1_damped (A : sig
    val a : float -> float
  end) =
  struct
    type state = mat * mat
    type f = mat * mat -> float -> mat
    type step_output = (mat * mat) * float
    type solve_output = mat * mat * mat

    let step = contact1_damped_s ~a:A.a
    let solve = prepare step
  end

  module Contact2_damped (A : sig
    val a : float -> float
  end) =
  struct
    type state = mat * mat
    type f = mat * mat -> float -> mat
    type step_output = (mat * mat) * float
    type solve_output = mat * mat * mat

    let step = contact2_damped_s ~a:A.a
    let solve = prepare step
  end
end

module D = struct
  type mat = Owl_dense_matrix_d.mat

  include Make (Owl_algodiff_primal_ops.D)

  module Contact1_damped (A : sig
    val a : float -> float
  end) =
  struct
    type state = mat * mat
    type f = mat * mat -> float -> mat
    type step_output = (mat * mat) * float
    type solve_output = mat * mat * mat

    let step = contact1_damped_s ~a:A.a
    let solve = prepare step
  end

  module Contact2_damped (A : sig
    val a : float -> float
  end) =
  struct
    type state = mat * mat
    type f = mat * mat -> float -> mat
    type step_output = (mat * mat) * float
    type solve_output = mat * mat * mat

    let step = contact2_damped_s ~a:A.a
    let solve = prepare step
  end
end

(* TODO: Add integrator for generic g_2(z) as by description *)
