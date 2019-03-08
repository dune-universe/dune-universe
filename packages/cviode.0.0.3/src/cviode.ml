module Make
    (M: Owl_types_ndarray_algodiff.Sig with type elt = float) 
= struct

  module M = struct
    include M
    (* TODO: implement this in owl *)
    let ( *$ ) = M.mul_scalar
    let ( + ) = M.add
  end

  module Sym = Owl_ode.Symplectic_generic.Make(M)

  type f_t = M.arr -> M.arr -> float -> M.arr

  (** {2:lib cviode library} *)
  let contact1_damped_s ~a ~(f:f_t) ~dt = fun xs ps t0 ->
    let t = t0 +. dt in
    let c0 = 1.0 -. dt*.(a t) in
    let c1 = 0.5 *. dt in
    let fxs = f xs ps t in
    let xs' = M.(xs + ps *$ (dt*.c0) + fxs *$ (dt*.c1)) in
    let fxs' = f xs' ps t in
    let ps' = M.(ps *$ c0 + (fxs + fxs') *$ c1) in
    xs', ps', t

  let contact2_damped_s ~a ~(f:'f_t) ~dt = fun xs ps t0 ->
    let t = t0 +. dt in
    let at = a t in
    let fxs = f xs ps t in
    let c0m = 1.0 -. 0.5*.dt*.at in
    let c0p = 1.0 +. 0.5*.dt*.at in
    let c1 = 0.5*.dt in
    let xs' = M.(xs + ps *$ (dt*.c0m) + fxs *$ (dt*.c1)) in
    let fxs' = f xs' ps t in
    let ps' = M.(ps *$ (c0m/.c0p) + (fxs + fxs') *$ (c1/.c0p)) in
    xs', ps', t
end

module S = struct
  type mat = Owl_dense_matrix_s.mat

  include Make(Owl_dense_ndarray.S)

  module Contact1_damped(A: sig val a:float->float end) =
  struct
    type s = mat * mat
    type t = mat
    type output = mat * mat * mat
    let solve = Sym.prepare (contact1_damped_s ~a:A.a)
  end

  module Contact2_damped(A: sig val a:float->float end) =
  struct
    type s = mat * mat
    type t = mat
    type output = mat * mat * mat
    let solve = Sym.prepare (contact2_damped_s ~a:A.a)
  end
end

module D = struct
  type mat = Owl_dense_matrix_d.mat

  include Make(Owl_dense_ndarray.D)

  module Contact1_damped(A: sig val a:float->float end) =
  struct
    type s = mat * mat
    type t = mat
    type output = mat * mat * mat
    let solve = Sym.prepare (contact1_damped_s ~a:A.a)
  end

  module Contact2_damped(A: sig val a:float->float end) =
  struct
    type s = mat * mat
    type t = mat
    type output = mat * mat * mat
    let solve = Sym.prepare (contact2_damped_s ~a:A.a)
  end
end

(* TODO: Add integrator for generic g_2(z) as by description *)
