
let name_of_rank = ref (Array.make 0 "")
let rank_of_name = ref ([] : (string * int) list)
let nbdims = ref 0
let already_initialized = ref false

let in_assoc = (fun name -> List.assoc name !rank_of_name)
let out_assoc = (fun rank -> !name_of_rank.(rank))

let initialize ~strict ~dims:lnames ~maxrows =
  if !already_initialized then
    Polka.finalize();
  nbdims := List.length lnames;
  Polka.initialize ~strict:strict ~maxdims: (!nbdims+3) ~maxrows: maxrows;
  name_of_rank := Array.make !nbdims "";
  rank_of_name := [];
  let l = ref lnames in
  for i=0 to !nbdims-1 do
    let name = List.hd !l in
    l := List.tl !l;
    !name_of_rank.(i) <- name;
    rank_of_name := (name,i) :: !rank_of_name;
  done;
  already_initialized := true;
  ()

let vector_of_constraint str = Vector.of_constraint in_assoc !nbdims str
and vector_of_frame str = Vector.of_frame in_assoc !nbdims str
and vector_of_expr str = Vector.of_expr in_assoc !nbdims str

let matrix_of_lconstraints str = Matrix.of_lconstraints in_assoc !nbdims str
and matrix_of_lframes str = Matrix.of_lframes in_assoc !nbdims str

let poly_of_lconstraint str = Poly.of_lconstraints in_assoc !nbdims str
and poly_of_lframe str = Poly.of_lframes in_assoc !nbdims str

let vector_print_constraint = Vector.print_constraint out_assoc
and vector_print_frame = Vector.print_frame out_assoc
and vector_print_expr = Vector.print_expr out_assoc

let matrix_print_constraints = Matrix.print_constraints out_assoc
and matrix_print_frames = Matrix.print_frames out_assoc

let poly_print_constraint = Poly.print_constraints out_assoc
and poly_print_frame = Poly.print_frames out_assoc
and poly_print = Poly.print out_assoc
