open Bigarray

(* Useful conversion functions to avoid Obj.magic at many places while
   being more secure. *)
external vec_to_c : ('a, 'b, 'c) Array1.t -> ('a, 'b, c_layout) Array1.t
  = "%identity"
external vec_to_fortran : ('a, 'b, 'c) Array1.t -> ('a, 'b, fortran_layout) Array1.t
  = "%identity"
external mat_to_c : ('a, 'b, 'c) Array2.t -> ('a, 'b, c_layout) Array2.t
  = "%identity"
external mat_to_fortran : ('a, 'b, 'c) Array2.t -> ('a, 'b, fortran_layout) Array2.t
  = "%identity"

external vec_opt_to_c :
  ('a, 'b, 'c) Array1.t option -> ('a, 'b, c_layout) Array1.t option
  = "%identity"
external vec_opt_to_fortran :
  ('a, 'b, 'c) Array1.t option -> ('a, 'b, fortran_layout) Array1.t option
  = "%identity"
external mat_opt_to_c :
  ('a, 'b, 'c) Array2.t option -> ('a, 'b, c_layout) Array2.t option
  = "%identity"
external mat_opt_to_fortran :
  ('a, 'b, 'c) Array2.t option -> ('a, 'b, fortran_layout) Array2.t option
  = "%identity"

let is_c_layout (l: _ layout) =
  l = (Obj.magic c_layout : _ layout)

let copy_vec v =
  let v' = Array1.create (Array1.kind v) (Array1.layout v) (Array1.dim v) in
  Array1.blit v v';
  v'

let copy_mat m =
  let m' = Array2.create (Array2.kind m) (Array2.layout m)
                         (Array2.dim1 m) (Array2.dim2 m) in
  Array2.blit m m';
  m'
