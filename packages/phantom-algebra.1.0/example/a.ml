[@@@warning "-32"]
let const_e = exp 1.
let fexp = exp

open Phantom_algebra.Core
[@@@warning "-33"]
open Phantom_algebra.Type_functions
[@@@warning "+33"]
open Tools


let v = vec2 0. 1. |? "v (0 1)"
let m = "m = (1 0 | 2 * v)" :=
  mat2 (vec2 1. 0.) (scalar 2. * v) =? mat2 (vec2 1. 0.) (vec2 0. 2.)

let m2 = "m * m" := m * m  =? mat2 (vec2 1. 0.) (vec2 0. 4.)
let ez = "v * m" := v * m  =? vec2 0. 2.

let ew = vec3 0. 0. 1.  |? "(0 0 1)"
let s = scalar 7.  |? "s=7"

let ew = "(0 1) + 1" := v + vec2 1. 1.  =? vec2 1. 2.
let v' = "s + v" := s + v  =? vec2 7.  8.
let v'' = "s * v" := s * v  =? vec2 0. 7.

let v''' = "v' * ( 1 + v'')" := v' * ( scalar 1. + v'') =? vec2 7. 64.

let xyrot theta = mat3
    (vec3 (  cos theta) (-.sin theta) 0.)
    (vec3 (sin theta) (cos theta) 0.)
    (vec3        0.         0.      1.)

let xp1 =
  " id + 1 " :=
  map ( (+.) 1.) (eye d2) =? mat2 (vec2 2. 1.) (vec2 1. 2.)

let pi = 4. *. atan 1.
let r = "Rxy_(π/6)" :=
    xyrot (pi /.6.) =?
    let x = sqrt 3. /. 2. and y = 0.5 in
    mat3 (vec3 x ~-.y 0.) (vec3 y x 0.) (vec3 0. 0. 1.)
let r' =
"rotation x y (π/6)" :=
  rotation (vec3 1. 0. 0.) (vec3 0. 1. 0.) (pi /. 6.) =? r

let e1 = vec3 1. 0. 0. |? "e1=(1 0 0)"
let rot_pi2 = "R_(π/2)" :=
    xyrot (pi /. 2.) =? mat3
      (vec3 0. ~-.1. 0.)
      (vec3 1. 0. 0.)
      (vec3 0. 0. 1.)
let e2 = "R_(π/2) e1 = e2" := xyrot (pi /. 2.) * e1 =? vec3 0. 1. 0.

let r =
  "exp (π/6 (dx ^ dy)) = Rxy_(π/6)" :=
    exp ( scalar (pi /. 6.) * e1 ^ e2 ) =? r

let rec ortho g () =
  match orthonormalize [g (); g()] with
    | [x;y] -> x, y
    | _ -> ortho g ()

let ext_prod_antisym =
  let x, y = Alea.(vec4(), vec4 ()) in
  " x ^ y =  (y ^ x)^T" := x ^ y =? transpose( y ^ x )

let lie =
    let x, y = ortho Alea.vec3 () in
    let theta = Alea.u() in
  "exp (θ (dx ^ dy)) = Rxy_θ" :=
      exp( scalar theta * (x ^ y) ) =? rotation x y theta

let e3 = "*(e1 ^ e2)=e3" := cross e1 e2 =? vec3 0. 0. 1.
let f2 = "dx ^ dy" :=
    e1 ^ e2 =? mat3
      (vec3    0. ~-.1. 0.)
      (vec3    1. 0. 0.)
      (vec3    0. 0. 0.)

let m_one =
  "(0 1) ^ (1 0) = -1" := cross (vec2 0. 1.) (vec2 1. 0.) =? scalar (-1.)

let v4 = "concat: scalar |+| vec2 |+| scalar " :=
    scalar 0. |+| vec2 1. 2. |+| scalar 3.
    =? vec4 0. 1. 2. 3.

let v4' = "concat: vec2 |+| vec2" :=
    vec2 1. 2. |+| vec2 3. 4. =? vec4 1. 2. 3. 4.

let stretch =
  "stretch vec4'  (3. 2. |+| 1.)  ⇒ (3. 2. 1. 1.)" :=
  vec4' (vec2 3. 2. |+| scalar 1.) =? vec4 3. 2. 1. 1.
let eye2 = "eye" := eye d2 =? mat2 (vec2 1. 0.) (vec2 0. 1.)

let di1 =
  "di1= diag (2 1)" := mat2 (vec2 2. 0.) (vec2 0. 1.) =? diag (vec2 2. 1.)
let di2 = "di2= diag(1 2)" :=
  mat2 (vec2 1. 0.) (vec2 0. 2.) =? diag (vec2 1. 2.)
let di3 = "di1/di2" := di1 / di2 =? diag (vec2 2. 0.5)

let exp_eye =
  "exp id = diag(e,e)" := exp eye2 =? diag (vec2' @@ scalar const_e)

let sym = mat2 (vec2 0. 1.) (vec2 1. 0.)

let t = "( 1 2; 3 4) (0 1; 1 0)" :=
    mat2 (vec2 1. 2.) (vec2 3. 4.) * sym
    =? mat2 (vec2 2. 1.) (vec2 4. 3.)
let sym = "sym is an involution" := eye2 / sym =? sym

let mxy = mat3 (vec3 (-1.) 0. 0.) (vec3 0. (-1.) 0.) (vec3 0. 0. 1.)
let mid =
   "R_(π/24) ** 24 = -xy " := ( xyrot (pi/. 24.) ** 24 ) =? mxy
let mid =
  "R_(π/7) ** -7 = -xy" := ( xyrot (pi/. 7.) ** (-7) ) =? mxy

;;

let linear =
  let open Alea in
  let s =scalar () in
  let v = vec4 () in
  let w = vec4 () in
  let u = vec4 () in
  "s * (( u + v ) + w) = s * u + (s * v + s * w)" :=
    s * (( u + v ) + w) =? s * u + (s * v + s * w)

let mat_distribution =
  let open Alea in
  let s = scalar () in
  let s2 = scalar () in
  let s3 = scalar () in
  let m = mat4 () in
  let n = mat4 () in
  let u = vec4 () in
  let v = vec4 () in
  "s * (m + s2 * n) * (u + s3 * v) = s m u + s s3 m v + s s2 n u + s s2 s3 n v"
  :=
    s*(m+ s2 * n)*(u + s3 * v) =?
    s * m * u
    + s * s3 * m * v + s * s2 *  n * u + s * s2 * s3 * n * v

let vec_mat =
  let m,n = Alea.(mat4 (),mat4 ()) in
  let s, t = Alea.(scalar (), scalar ()) in
  let v, w = Alea.(vec4 (), vec4 ()) in
  "v M" := (v + s * w) * (m + t * n) =?
           v * m + t * v * n
           + s * w * m + s * t * w * n

let vec_mat =
  let m, v = Alea.(mat3() , vec3()) in
  "v M = M^T v" := v * m =? transpose m * v

let associativity =
  let m1, m2, m3 = Alea.(mat4 (), mat4 (), mat4 () ) in
  " M (N K) =  (M N) K" := m1 * (m2 * m3) =? (m1 * m2) * m3


let associativity_vec =
  let m1, m2, v = Alea.(mat4 (), mat4 (), vec4 () ) in
  " M (N v) =  (M N) v" := m1 * (m2 * v) =? (m1 * m2) * v

let associativity_form =
  let m1, m2, v = Alea.(mat4 (), mat4 (), vec4 () ) in
  " v (M N) =  (v M) N" := v * (m1 * m2) =? (v * m1) * m2


let rand =
  let m1 = Alea.mat3 () in
  let m2 = Alea.mat3 () in
  let m3 = m1 * m2  in
  "rm1 * rm2 / rm2 - m1 " := m3 / m2  =? m1
(*
let error = cross (vec4 0. 0. 0. 1.) (vec4 0. 1. 0. 0.)
let error' = cross (scalar 1.) (scalar 2.)
*)


let vec_div0 =
  let m = diag (vec4 1. 2. 3. 4.) in
  let v = vec4 5. 7. 11. 13. in
  " v M / M, 1)" := (v * m) / m =? v

let vec_div2 =
  let m = mat2 (vec2 1. 0.) (vec2 1. 1.) in
  let v = vec2 31. 37. in
  " v M / M, 3)" := (v * m) / m =? v

let vec_div =
  let m = Alea.mat4 () in
  let v = Alea.vec4 () in
  let vm = v * m in
  "v M / M = v, 4)" := vm / m =? v

let rotation_are_so =
  let s = 2. *. pi *. Alea.u () in
  "R∈SO(n)" :=  +(det (xyrot s)) =? (scalar 1.)

let det_isomorphism =
  let m1, m2 = Alea.(mat4 (), mat4 () ) in
  let eye4 = eye d4 in
  let m1, m2 = m1 + eye4, m2 + eye4 (* avoid ill-conditioned matrices *)
  in
  "det(A B) = (det A) (det B)" :=  +det (m1 * m2) =? +(det m1 *. det m2)

let transpose_is_contravariant =
  let a, b = Alea.( mat4 (), mat4 () ) in
  "(A B)^T = B^T * A^T" := transpose (a * b) =? transpose b * transpose a

let transpose_and_on =
  let v, w, theta = Alea.(vec4 (), vec4 (), u () ) in
  let r = rotation v w theta and r' = transpose (rotation v w (-.theta)) in
  "R∈O(n)" := r =? r'


let rec ortho4 () =
  let l = Alea.[vec4 (); vec4(); vec4 (); vec4 (); vec4 ()] in
  match orthonormalize l with
  | [a;b;c;d] -> mat4 a b c d
  | _ -> ortho4 ()

let group_orthogonal =
  let m = ortho4 () in
  " m ∈ O(n), m m^T = id " := m * transpose m =? eye d4


let sym =
  let s = Alea.mat4 () in
  let s = (s <+> transpose s) / scalar 2. in
  let x, y = Alea.(vec4 (), vec4 ()) in
  "⟨ x | M y ⟩ = ⟨ x M | y ⟩" := +(x |*| s * y) =? +(x * s |*| y)

module X :sig val t: _ scalar end = struct
let fn v w =
  (cross v w) + scalar 1.
let t =
  "(0 1) ^ (1 0) + 1 = 0" := fn (vec2 0. 1.) (vec2 1. 0.) =? scalar 0.
end

module M: sig end = struct
  let one r =
    let s = scalar 1. in
    dim_match r
      (fun _ -> s )
      (fun _ -> vec2' s )
      (fun _ -> vec3' s )
      (fun _ -> vec4' s )

  let t = "dim matching (1 1 1)" := one d3 =? vec3 1. 1. 1.
end


let trace =
  let a, b ,c = Alea.( mat4(), mat4(), mat4 ()) in
  "trace (ABC) = trace(BCA) " :=
    scalar (trace (a * b * c)) =? scalar ( trace (b * c * a) )


let jacobi_for_commutator =
  let a, b ,c = Alea.( mat4(), mat4(), mat4 ()) in
  let ( ^ ) = commutator in
  "[A,[B,C]] + [B,[C,A]] + [C, [A,B]] = 0 " :=
    (a^(b^c)) + (b^(c^a)) =? ((a^b)^c)

let jacobi_for_cross =
  let a, b ,c = Alea.( vec3(), vec3(), vec3 ()) in
  let ( ^ ) = cross in
  "cross u (cross v w) + cross v cross w u + cross w (cross u v) = 0 " :=
    (a^(b^c)) + (b^(c^a)) =? ((a^b)^c)

let det_is_antisymmetric =
  let a = [|Alea.vec4() + vec4 1. 0. 0. 0.;
            Alea.vec4() + vec4 0. 1. 0. 0.;
            Alea.vec4() + vec4 0. 0. 1. 0.;
            Alea.vec4() + vec4 0. 0. 0. 1.|] in
  let rec gen () =
    let i, j = Random.int 4, Random.int 4 in
    if i = j then gen () else i, j in
  let i, j = gen () in
  let a' = Array.copy a in
  a'.(i) <- a.(j); a'.(j) <- a.(i);
  let det a = +det (mat4 a.(0) a.(1) a.(2) a.(3)) in
  "det m = -det τ m" := det a =? (-det a')

let det_is_multilinear =
  let a = Alea.mat4 () + scalar 3. * eye d4 in
  let v = Alea.vec4 () + vec4 3. 0. 0. 0. in
  let x, y, z, w = slice a x', slice a y', slice a z', slice a w' in
  "det (a+b,...) = det (a,...) + det (b,...)" :=
  +det (mat4 (x+v) y z w) =? +det (mat4 x y z w) + +det (mat4 v y z w)

let scalar_product_is_sym =
  "(v,w) = (w,v)" :=
    let v, w = Alea.(mat4(), mat4()) in
    +(v|*|w) =? +(w|*|v)

let scalar_product_is_linear =
  "(u + λ v ,w) = (u,w) + λ (v,w)" :=
    let u,v,w, s = Alea.(mat4(), mat4(), mat4(), u () ) in
    +(u +  scalar s * v|*|w) =? +((u|*|w) +. s *.  (v|*|w))

let exponential_addition =
  let m, a, b, c ,d, e, f = let open Alea in
    mat4(), scalar(), scalar (), scalar (), scalar (), scalar (), scalar () in
  let k = a * m + b * m * m + c * eye d4 in
  let l = d * m +  e * m * m + f * eye d4 in
  Tools.set_epsilon (fexp (norm_1 k +. norm_1 l) *. 1e-12);
  "if [a,b] = 0 then exp(a+b) = (exp a) (exp b)" :=
    exp ( k + l ) =? exp k * exp l

;; Tools.set_epsilon 1e-12
