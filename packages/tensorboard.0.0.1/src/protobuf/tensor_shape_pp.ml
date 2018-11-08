[@@@ocaml.warning "-27-30-39"]

let rec pp_tensor_shape_proto_dim fmt (v:Tensor_shape_types.tensor_shape_proto_dim) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "size" Pbrt.Pp.pp_int64 fmt v.Tensor_shape_types.size;
    Pbrt.Pp.pp_record_field "name" Pbrt.Pp.pp_string fmt v.Tensor_shape_types.name;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_tensor_shape_proto fmt (v:Tensor_shape_types.tensor_shape_proto) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "dim" (Pbrt.Pp.pp_list pp_tensor_shape_proto_dim) fmt v.Tensor_shape_types.dim;
    Pbrt.Pp.pp_record_field "unknown_rank" Pbrt.Pp.pp_bool fmt v.Tensor_shape_types.unknown_rank;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
