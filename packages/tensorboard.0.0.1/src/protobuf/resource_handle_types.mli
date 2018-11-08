(** resource_handle.proto Types *)



(** {2 Types} *)

type resource_handle_proto = {
  device : string;
  container : string;
  name : string;
  hash_code : int64;
  maybe_type_name : string;
}


(** {2 Default values} *)

val default_resource_handle_proto : 
  ?device:string ->
  ?container:string ->
  ?name:string ->
  ?hash_code:int64 ->
  ?maybe_type_name:string ->
  unit ->
  resource_handle_proto
(** [default_resource_handle_proto ()] is the default value for type [resource_handle_proto] *)
