open Type_functions

module Int = struct
  let(+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
  let ( ~- ) = ( ~- )
end
type _ dim =
  | D1
  | D2
  | D3
  | D4

let d1 = D1
let d2 = D2
let d3 = D3
let d4 = D4

let dim_to_int: type a. a dim -> int = function
  | D1 -> 1
  | D2 -> 2
  | D3 -> 3
  | D4 -> 4

type (+'input_dim,+'output_dim,+'rank,+'group) index = int

type _ rank = Scalar | Vector | Matrix

module Rank = struct
  let scalar=Scalar
  let vector=Vector
  let matrix=Matrix
  let rank_from_int = function
    | 0 -> Scalar
    | 1 -> Vector
    | 2 -> Matrix
    | _ -> assert false

  let rank_to_int = function
    | Scalar -> 0
    | Vector -> 1
    | Matrix -> 2
end
open Rank

let ilen x = 0x3F land (x lsr 18)
let shift x = 0xFF land (x lsr 16)
let irank x = rank_from_int (x lsr 24)
let t3 x = x, x, x
let x', r', s' = t3 0x1040000
let y', g', t' = t3 0x1040001
let z', b', p' = t3 0x1040002
let w', a', q' = t3 0x1040003


let xx', rr', ss' = t3 0x2040000
let xy', rg', st' = t3 0x2040001
let xz', rb', sp' = t3 0x2040002
let xw', ra', sq' = t3 0x2040003
let yx', gr', ts' = t3 0x2040004
let yy', gg', tt' = t3 0x2040005
let yz', gb', tp' = t3 0x2040006
let yw', ga', tq' = t3 0x2040007
let zx', br', ps' = t3 0x2040008
let zy', bg', pt' = t3 0x2040009
let zz', bb', pp' = t3 0x204000A
let zw', ba', pq' = t3 0x204000B
let wx', ar', qs' = t3 0x204000C
let wy', ag', qt' = t3 0x204000D
let wz', ab', qp' = t3 0x204000E
let ww', aa', qq' = t3 0x204000F


let dim_mask = 0xFF0000

let (&) x y =
  let nx = shift x in
  x + ((y land 0xFFFF) lsl nx) + y land dim_mask

type (+'dim,+'rank) t = Flat_array.t

module A = struct

  include Flat_array
  let elt_len a =
    let l = len a in
    if l = 5 then 4 else l
  let (#.) = get
  let iteri f x =
    for i = 0 to elt_len x - 1 do
      f i x#.i
    done
  let init n f  =
    let a = create n in
    if n = 5 then begin
      for i = 0 to 3 do set a i (f i) done;
      set a 4 0.
    end
    else for i = 0 to n - 1 do
        set a i (f i)
      done;
    a

  let copy x =
    let len = len x in
    let a = create len in
    iteri (set a) x;
    a
  let make n x =
    init n (fun _ -> x)

  let map f x = init (len x) (fun i -> f x#.i)
  let fold f x a = let res = ref x in
    for i = 0 to elt_len a do res := f !res a#.i done; !res

  let fold2 f acc x y  = let res = ref acc in
    for i = 0 to min (elt_len x) (elt_len y) do
      res := f !res x#.i y#.i
    done; !res
end


let dim a = match A.len a with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 4 -> 4
  | 5 -> 2
  | 9 -> 3
  | 16 -> 4
  | _ -> assert false


let rank a = match A.len a with
  | 1 -> Scalar
  | 2 -> Vector
  | 3 -> Vector
  | 4 -> Vector
  | 5 -> Matrix
  | 9 -> Matrix
  | 16 -> Matrix
  | _ -> assert false

let (#.) = A.(#.)

let mat_dim a = match A.len a with
  | 5 -> 2
  | 9 -> 3
  | 16 -> 4
  | n -> int_of_float (sqrt (float_of_int n))


let mat_len = function
  | 2 -> 5
  | 3 -> 9
  | 4 -> 16
  | _ -> assert false


let mat_init dim f =
  let a = A.create (mat_len dim) in
  let pos = ref 0 in
  for i = 0 to dim -1 do
    for j = 0 to dim -1 do
      A.set a !pos (f i j);
      incr pos
    done;
  done;
  if dim = 2 then A.set a !pos 0.;
  a

let pp ppf a = match rank a with
  | Scalar -> Format.pp_print_float ppf a#.0
  | Vector ->
    Format.fprintf ppf "@[(% g" a#.0;
    for i=1 to (A.len a -1) do
      Format.fprintf ppf "@ % g" a#.i
    done;
    Format.fprintf ppf ")@]"
  | Matrix ->
    let dim = mat_dim a in
    let line i =
      Format.fprintf ppf "@[|% g" a#.(dim * i);
    for j=1 to (dim-1) do
      Format.fprintf ppf "@ % g" a#.(dim * i + j)
    done;
    Format.fprintf ppf " |@]" in
    Format.fprintf ppf "@[<v>";
      line 0;
    for i = 1 to dim - 1 do Format.pp_print_cut ppf (); line i done;
    Format.fprintf ppf "@]"

let lu dim col_transf m =
    let perm = Array.init dim (fun n -> n) in
  let switch i j =
    let tmp = perm.(i) in
    perm.(i) <- perm.(j); perm.(j) <- tmp; in
  let pivot start =
    let pos = start * dim in
    let mx = ref (abs_float m#.(pos + perm.(start) ) )
    and i = ref start in
    for j = start + 1 to dim -1 do
      let m = abs_float m#.(pos + perm.(j)) in
      if m > !mx then
        (mx := m; i:=j)
    done;
    if start <> !i then switch start !i in
  (* zero upper *)
  for i = 0 to dim - 1 do
    let i' = dim * i in
    pivot i;
    let x = m#.(i' + perm.(i)) in
    if x <> 0. then
      for  j = i + 1 to dim - 1 do
        let coeff = -. m#.(i' + perm.(j)) /. x in
        if coeff <> 0. then
          col_transf perm coeff i j
      done
  done;
  perm

let det m =
  let dim = mat_dim m in
  let col_transf perm coeff k l =
    let k' = perm.(k) and l' = perm.(l) in
    for i = k to dim-1 do
      let i = dim * i in
      let il = i + l' and ik = i + k' in
      A.set m il @@ m#.(il) +.  coeff *. m#.(ik);
    done in
  let perm = lu dim col_transf m in
  let _, sign = Array.fold_left (fun (i,s) j ->
      if i > j then (i+1, -1 * s) else
        (i+1,s)) (0,1) perm in
  let res = ref 1. in
  for i = 0 to dim - 1 do
    res := !res *. m#.(i + perm.(i) * dim  )
  done;
  float sign *. !res

(* ( A / B) B = A *)
let mat_div x y =
  let dim = mat_dim y in
  let left = A.copy y and right = A.copy x in
  let dim_x = if rank x = Vector then 1 else dim in
  let col_transf perm coeff k l =
    let k' = perm.(k) and l' = perm.(l) in
    for i = k to dim-1 do
      let i = dim * i in
      let il = i + l' and ik = i + k' in
      A.set left il @@ left#.(il) +.  coeff *. left#.(ik);
    done;
    for i = 0 to dim_x - 1 do
      let i = dim * i in
      let il = i + l' and ik = i + k' in
      A.set right il @@ right#.(il) +.  coeff *. right#.(ik);
    done;
  in
  let perm = lu dim col_transf left in
  (* zero lower *)
  for i = dim-1 downto 0 do
    let i' = dim * i in
    let x = left#.(i' + perm.(i)) in
    if x <> 0. then
      for j = 0 to i-1 do
        let coeff = -. left#.(i' + perm.(j)) /. x in
        col_transf perm coeff i j;
      done
  done;
  for i = 0 to dim_x - 1 do
    for j = 0 to dim - 1 do
      let j' = perm.(j) in
      let ij = i * dim + j' in
      let x = left#.(j * dim + j') in
      if x <> 0. then
        A.set right ij @@ right#.(ij) /. x
    done;
  done;
  let data = if dim_x = 1 then A.create dim else left in
  for j = 0 to dim - 1 do
    let j' = perm.(j) in
    for i = 0 to dim_x - 1 do
      let i = dim * i in
      A.set data (i + j) @@ right#.(i + j');
    done
  done;
  data

type +'x scalar = ('a, 'b z) t constraint 'x = 'a * 'b

type +'x vec2 = ('a two,'b one) t constraint 'x = 'a * 'b
type +'x vec3 = ('a three,'b one) t constraint 'x = 'a * 'b
type +'x vec4 = ('a four,'b one) t constraint 'x = 'a * 'b

type +'x mat2 = ('a two,'b two) t constraint 'x = 'a * 'b
type +'x mat3 = ('a three,'b two) t constraint 'x = 'a * 'b
type +'x mat4 = ('a four,'b two) t constraint 'x = 'a * 'b


let ( |+| ) a b =
  let l = A.len a + A.len b in
  let sep = A.len a in
  let data = A.create l in
  for i = 0 to sep - 1 do
    A.set data i a#.i
  done;
  for i = sep to l - 1 do
    A.set data i @@ b#.(i-sep)
  done;
  data


let scalar x =
  let a = A.create 1 in
  A.set a 0 x;
  a
let vec2 x y =
  let a = A.create 2 in
  A.set a 0 x;
  A.set a 1 y;
  a
let vec3 x y z =
  let a = A.create 3 in
  A.set a 0 x;
  A.set a 1 y;
  A.set a 2 z;
  a

let vec4 x y z t =
  let a = A.create 4 in
  A.set a 0 x;
  A.set a 1 y;
  A.set a 2 z;
  A.set a 3 t;
  a

let vec2' a =
  if rank a = Scalar then
    A.make 2 a#.0
  else
    A.copy a

let vec_stretch k a =
  if rank a = Scalar then
    A.make k a#.(0)
  else
    let data = A.create k in
    A.iteri (A.set data) a;
    let l = A.len a in
    let repeated = a#.(l-1) in
    for i = l to k -1 do
      A.set data i repeated
    done;
    data

let vec3' x = vec_stretch 3 x
let vec4' x = vec_stretch 4 x

let mat2 a b =
  let m = A.create (mat_len 2) in
  A.set m 0 a#.0; A.set m 1 a#.1;
  A.set m 2 b#.0; A.set m 3 b#.1;
  A.set m 4 0.;
  m


let mat3 a b c =
  let m = A.create (mat_len 3) in
  A.set m 0 a#.0; A.set m 1 a#.1; A.set m 2 a#.2;
  A.set m 3 b#.0; A.set m 4 b#.1; A.set m 5 b#.2;
  A.set m 6 c#.0; A.set m 7 c#.1; A.set m 8 c#.2;
  m

let mat4 a b c d =
  let m = A.create (mat_len 4) in
  A.set m  0 a#.0; A.set m  1 a#.1; A.set m  2 a#.2; A.set m  3 a#.3;
  A.set m  4 b#.0; A.set m  5 b#.1; A.set m  6 b#.2; A.set m  7 b#.3;
  A.set m  8 c#.0; A.set m  9 c#.1; A.set m 10 c#.2; A.set m 11 c#.3;
  A.set m 12 d#.0; A.set m 13 d#.1; A.set m 14 d#.2; A.set m 15 d#.3;
  m


let swizzle v index =
  let size = ilen index in
  A.init size
    (fun i ->
       let pos = i lsl 2 in
       let index' = index lsr pos in
       let masked = 0xF land index' in
       v#.(masked)
    )

let slice (t: (_,_) t) (n:(_ index)) =
  match rank t with
  | Scalar -> scalar t#.(0)
  | Vector ->
    if ilen n = 1 then
      scalar t#.(n land 0xF)
    else swizzle t n
  | Matrix ->
    let dim = mat_dim t in
    let len = ilen n in
    if irank n = Matrix then
      if len = 1 then
        scalar @@ t#.( dim * ( 0x3 land n) + ((n lsr 2) land 0x3) )
      else begin
        A.init len (fun i ->
            let s = ((n lsr (i lsl 2)) land 0xF) in
            let i = 0x3 land s in
            let j = s lsr 2 in
             t#.( dim * i + j) )
      end
    else if len = 1 then
      A.init dim
        (fun i -> t#.( i + dim * (n land 0x3) ))
    else
      let data = A.create (mat_len dim) in
      let pos = ref n in
      for i = 0 to dim - 1 do
        let s = dim * (0x3 land !pos) in
        let i = i * dim in
        for j = 0 to dim - 1 do
          A.set data (i + j) @@ t#.( s + j)
        done;
        pos := !pos lsr 4
      done;
      if dim = 2 then A.set data 4 0.;
      data


let get (t: (_,_) t) (n:(_ index)) = match rank t with
  | Scalar -> t#.(0)
  | Vector -> t#.(n land 0x3 )
  | Matrix -> t#.( (n lsr 2) land 0x3 + mat_dim t * (n land 0x3) )
;;

let amap2 f x y =
  A.init
    (min (A.len x) (A.len y))
    (fun i -> f x#.(i) y#.(i))

let map f x = A.map f x
let map2 f x y = amap2 f x y
let smap f x y = map (f x#.(0)) y


let cross a b =
  if A.len a = 2 then
    scalar (a#.(0)*.b#.(1) -. a#.(1) *. b#.(0))
  else
    vec3
      (a#.(1) *. b#.(2) -. a#.(2) *. b#.(1) )
      (a#.(2) *. b#.(0) -. a#.(0) *. b#.(2) )
      (a#.(0) *. b#.(1) -. a#.(1) *. b#.(0) )

let ( ^ ) a b =
  let dim = A.len a in
  let data = A.make (mat_len dim) 0. in
  for i = 0 to dim -1 do
    for j = (i+1) to dim - 1 do
      let r =  a#.(i) *. b#.(j) -. b#.(i) *. a#.(j) in
      A.set data ( i * dim + j ) @@ data#.( i * dim + j) -. r;
      A.set data ( j * dim + i ) @@ data#.( j * dim + i) +. r
    done
  done;
  data

let ( *% ) x = map ( ( *. ) x )

let ( * ) a b = match rank a, rank b with
  | Scalar, _ -> smap ( *. ) a b
  | _, Scalar -> smap ( *. ) b a
  | Vector, Vector -> map2 ( *. ) a b
  | Vector, Matrix | Matrix, Vector ->
    let dim = min (A.len a) (A.len b) in
    let a , b, s1, s2= if rank a = Vector then a, b, 1, dim
      else b, a, dim, 1 in
    let sum i = let s = ref 0. and ij = ref (i * s1) in
      for j = 0 to dim -1 do
        s:= !s +. a#.(j) *. b#.(!ij);
        ij := s2 + !ij done;
      !s
    in A.init dim sum
  | Matrix, Matrix ->
    let dim = mat_dim a in
    let sum i j = let s = ref 0. in
      for k = 0 to dim - 1 do
        s:= a#.(i * dim + k) *. b#.(k * dim + j) +. !s done;
      !s
    in
    let data = mat_init dim sum in
    data


let ( / ) a b =
  match rank a, rank b with
  | Vector, Vector -> map2 (/.) a b
  | Vector, Matrix -> mat_div a b
  | Matrix, Matrix ->
    mat_div a b
  | _ -> smap (fun x y -> y /. x ) b a

let dirac i j = if i=j then 1. else 0.

let eye dim =
  mat_init dim dirac

let diag vec =
  mat_init (dim vec) (fun i j -> if i = j then vec#.i else 0.)

let id dim rank = match rank with
  | Scalar -> scalar 1.
  | Vector -> A.make dim 1.
  | Matrix -> eye dim

let inv a =
  match rank a with
  | Scalar | Vector ->  A.map (fun x -> 1. /. x ) a
  | Matrix -> mat_div (eye @@ dim a) a

let ( |*| ) a b =
  let s = ref 0. in
  for i = 0 to (A.len a - 1) do
    s:= !s +. a#.(i) *. b#.(i)
  done;
  !s

let rec pow k x =
  match k with
  | 0 -> id (dim x) (rank x)
  | 1 -> x
  | 2 -> x * x
  | k ->
    if k mod 2 = 1 then
      x * pow (k lsr 1) (x*x)
    else
      pow (k lsr 1) (x*x)


let rec pow_2_k ln_2_k x =
  match ln_2_k with
  | 0 -> x
  | 1 -> x * x
  | k -> pow_2_k (k -1) (x*x)

let pow k x = if k < 0 then
    pow (-k) (inv x)
  else
    pow k x

let ( **. ) = ( ** )
let ( ** ) x k = pow k x

let norm x = sqrt (x|*|x)
let distance x y =
  sqrt @@ A.fold2 (fun acc x y-> acc +. (x -. y) **. 2.) 0. x y

let norm_1 = A.fold (fun acc x -> acc +. abs_float x ) 0.
let norm_q q a =
  (A.fold (fun acc x -> acc +. (abs_float x) **. q ) 0. a) **. (1./.q)

let (<+>) x y = map2 (+.) x y
let (<->) x y = map2 (-.) x y


let (+) a b =
  if rank a = Scalar then
    smap (+.) a b
  else if rank b = Scalar then
    smap (+.) b a
  else a <+> b


let (-) a b =
  if rank a = Scalar then
    smap (-.) a b
  else if rank b = Scalar then
    A.init (A.len b)
      (fun n -> b#.(n) -. a#.(0))
  else a <-> b

let (~-) = map (~-.)
let (~+) x = scalar x


let theta_13 = 5.371920351148152e0
let pade_13= [|
  64764752532480000.;
  32382376266240000.;
  7771770303897600.;
  1187353796428800.;
  129060195264000.;
  10559470521600.;
  670442572800.;
  33522128640.;
  1323241920.;
  40840800.;
  960960.;
  16380.;
  182.;
  1.
|]

(* Higham, 2005 *)
let expm a =
  let b = pade_13 in
  let norm1 = norm_1 a in
  let s = min 0 @@ snd @@ frexp (norm1 /. theta_13) in
  let a = map (fun f -> ldexp f s) a in
  let a0 = eye (dim a) in
  let a2 = a *a in let a4 = a2 * a2 in let a6 = a2 * a4 in
  let u =
    a *
    (a6 * ( b.(13) *% a6 + b.(11) *% a4 + b.(9) *% a2)
     + b.(7) *% a6 + b.(5) *% a4 + b.(3) *% a2 + b.(1) *% a0) in
  let v =
    (a6 * ( b.(12) *% a6 + b.(10) *% a4 + b.(8) *% a2)
     + b.(6) *% a6 + b.(4) *% a4 + b.(2) *% a2 + b.(0) *% a0) in
  pow_2_k  Int.(-s) ( (u + v) / (v - u) )

let exp m = match rank m with
  | Scalar | Vector -> map exp m
  | Matrix -> expm m


let normalize x =
  let n = norm x in
  if n = 0. then failwith "Null vector cannot be normalized"
  else
      x / (scalar (norm x))
let orthonormalize vs =
  let normalize_next rs vs x =
    let x = List.fold_left (fun x b -> x - (b|*|x) *% b ) x vs in
    let n = norm x in
    if n > 0. then x / (scalar n) :: rs else rs in
  let normalize_next x = normalize_next x x in
  List.fold_left normalize_next [] vs

let row_map f m =
  match dim m with
  | 2 -> mat2 (f (slice m x')) (f (slice m y'))
  | 3 -> mat3 (f (slice m x')) (f (slice m y')) (f (slice m z'))
  | 4 ->
    mat4 (f (slice m x')) (f (slice m y')) (f (slice m z')) (f (slice m w'))
  | _ -> assert false

let rotation x y theta =
  match orthonormalize [x;y] with
  | [] | [_] | _ :: _ :: _  :: _ ->
    failwith "rotation: non-orthogonal vectors do not define a rotation plane"
  | [y;x] ->
    let cosm1 = cos theta -. 1. and sin = sin theta in
    let f v =
      let vx = (x|*|v) and vy = (y|*|v) in
      ( cosm1 *. vx +. sin *. vy) *% x
      + ( -. sin *. vx +. cosm1 *. vy) *% y
      + v
    in
    row_map f (eye (dim x))

let transpose m =
  let dim = mat_dim m in
  let x = A.create Int.(dim * dim) in
  for i = 0 to Int.(dim - 1) do
    for j = 0 to Int.( dim - 1 ) do
      A.set x Int.(i + dim * j) m#.Int.( i * dim + j)
    done
  done;
  x

let eye dim = eye (dim_to_int dim)

let zero (type a) (d: a dim) r = match r with
  | Scalar -> A.make 1 0.
  | Vector -> A.make (dim_to_int d) 0.
  | Matrix -> A.make (mat_len @@ dim_to_int d) 0.

let id d = id (dim_to_int d)

let dim_match dim one two three four =
(* we are using intersection type in a way that the typechecker cannot infer *)
  match dim with
  | D1 -> Obj.magic @@ one D1
  | D2 -> Obj.magic @@ two D2
  | D3 -> Obj.magic @@ three D3
  | D4 ->   Obj.magic @@ four D4


let rank_match rank zero one two =
(* we are using intersection type in a way that the typechecker cannot infer *)
  match rank with
  | Scalar -> Obj.magic @@ zero Scalar
  | Vector -> Obj.magic @@ one Vector
  | Matrix -> Obj.magic @@ two Matrix

let clone_2 v = v, v
let clone_3 v = v, v, v
let clone_7 v = v, v, v, v ,v ,v ,v


let commutator m n = m * n - n * m
let anticommutator m n = m * n + n * m

let trace m =
  let r = ref 0. in
  let dim = mat_dim m in
  for i = 0 to Int.(dim - 1) do
    r := !r +. m#.Int.( i*(1 + dim))
  done; !r

;;
#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
let (.%()) x = get x
let (.%[]) x = slice x
#endif
