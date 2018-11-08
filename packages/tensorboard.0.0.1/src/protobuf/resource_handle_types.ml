[@@@ocaml.warning "-27-30-39"]


type resource_handle_proto = {
  device : string;
  container : string;
  name : string;
  hash_code : int64;
  maybe_type_name : string;
}

let rec default_resource_handle_proto 
  ?device:((device:string) = "")
  ?container:((container:string) = "")
  ?name:((name:string) = "")
  ?hash_code:((hash_code:int64) = 0L)
  ?maybe_type_name:((maybe_type_name:string) = "")
  () : resource_handle_proto  = {
  device;
  container;
  name;
  hash_code;
  maybe_type_name;
}
