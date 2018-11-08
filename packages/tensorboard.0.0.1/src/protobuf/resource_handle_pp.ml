[@@@ocaml.warning "-27-30-39"]

let rec pp_resource_handle_proto fmt (v:Resource_handle_types.resource_handle_proto) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "device" Pbrt.Pp.pp_string fmt v.Resource_handle_types.device;
    Pbrt.Pp.pp_record_field "container" Pbrt.Pp.pp_string fmt v.Resource_handle_types.container;
    Pbrt.Pp.pp_record_field "name" Pbrt.Pp.pp_string fmt v.Resource_handle_types.name;
    Pbrt.Pp.pp_record_field "hash_code" Pbrt.Pp.pp_int64 fmt v.Resource_handle_types.hash_code;
    Pbrt.Pp.pp_record_field "maybe_type_name" Pbrt.Pp.pp_string fmt v.Resource_handle_types.maybe_type_name;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
