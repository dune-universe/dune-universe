[@@@ocaml.warning "-27-30-39"]


type tensor_shape_proto_dim = {
  size : int64;
  name : string;
}

type tensor_shape_proto = {
  dim : tensor_shape_proto_dim list;
  unknown_rank : bool;
}

let rec default_tensor_shape_proto_dim 
  ?size:((size:int64) = 0L)
  ?name:((name:string) = "")
  () : tensor_shape_proto_dim  = {
  size;
  name;
}

let rec default_tensor_shape_proto 
  ?dim:((dim:tensor_shape_proto_dim list) = [])
  ?unknown_rank:((unknown_rank:bool) = false)
  () : tensor_shape_proto  = {
  dim;
  unknown_rank;
}
